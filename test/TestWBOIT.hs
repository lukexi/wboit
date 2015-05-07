{-# LANGUAGE RecordWildCards #-}

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL

import Data.Bits
import Control.Monad
import Linear
import Foreign

import SetupGLFW
import Shader
import Cube
import Quad
import Mesh
import Reshader
import Halive.Utils

---------------------
-- Implementing WBOIT
---------------------

----
-- NOTE: we need to preserve the depth buffer for time-warp... is that possible?
----

{-
drawOpaqueSurfaces();
clear accumTexture to vec4(0), revealageTexture to float(1)
bindFramebuffer(accumTexture, revealageTexture);
glDepthMask(GL_FALSE);
glEnable(GL_BLEND);
glBlendFunci(0, GL_ONE, GL_ONE);
glBlendFunci(1, GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);
bindFragmentShader("...
  gl_FragData[0] = vec4(Ci, ai) * w(zi, ai);
  gl_FragData[1] = vec4(ai);
  ...}");
drawTransparentSurfaces();
unbindFramebuffer();
glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
bindFragmentShader("...
  vec4 accum = texelFetch(accumTexture, ivec2(gl_FragCoord.xy), 0);
  float r = texelFetch(revealageTexture, ivec2(gl_FragCoord.xy), 0).r;
  gl_FragColor = vec4(accum.rgb / clamp(accum.a, 1e-4, 5e4), r);
  ...}", accumTexture, revealageTexture);
-}


{-
Implementing Weighted, Blended OIT
----------------------------------

The shader modifications to implement the technique are very simple. 
During transparent surface rendering, shade surfaces as usual but output to two render targets. 
The first render target must have at least RGBA16F precision and the second must have at least R8 precision. 
Clear the first render target to vec4(0) and the second render target to 1 (using a pixel shader or glClearBuffer + glClear).

Then, render the surfaces in any order to these render targets, 
adding the following to the bottom of the pixel shader and using the specified blending modes:

// Output linear (not gamma encoded!), unmultiplied color from
// the rest of the shader.
vec4 color = ... // regular shading code
 
 
 
// Insert your favorite weighting function here. The color-based factor
// avoids color pollution from the edges of wispy clouds. The z-based
// factor gives precedence to nearer surfaces.
float weight = 
      max(min(1.0, max(max(color.r, color.g), color.b) * color.a)), color.a) *
      clamp(0.03 / (1e-5 + pow(z / 200, 4.0)), 1e-2, 3e3);
 
// Blend Func: GL_ONE, GL_ONE -- Luke sez: he means "specify these outside" (see topmost snippet above^^^)
// Switch to premultiplied alpha and weight
gl_FragData[0] = vec4(color.rgb * color.a, color.a) * weight;
 
// Blend Func: GL_ZERO, GL_ONE_MINUS_SRC_ALPHA
gl_FragData[1].a = color.a;

If you are rendering some calls (such as particles or out of focus regions) at low resolution, 
then you can run the above code to different resolution textures independently and combine them during compositing. 
It will work even if the low- and high-resolution content are interleaved in depth.

Finally, after all surfaces have been rendered, composite the result onto the screen using a full-screen pass:

vec4 accum = texelFetch(RT0, int2(gl_FragCoord.xy), 0);
float reveal = texelFetch(RT1, int2(gl_FragCoord.xy), 0).r;
 
// Blend Func: GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA
gl_FragColor = vec4(accum.rgb / max(accum.a, 1e-5), reveal);

The compositing must occur before gamma-encoding 
(that is, I'm assuming that the renderer performs gamma-encoding (if needed by your target), 
color grading, etc. in a subsequent full-screen pass.)

See the paper for full details and fallback paths for GPUs or APIs 
that do not support multiple render targets or per-render target blending functions.

Keep the depth buffer that was rendered for opaque surfaces bound for depth testing 
when rendering transparent surfaces, but do not write to it. 
This preserves early-Z culling, which can significantly improve throughput. 
I recommend rendering surfaces that have alpha cutouts during both the transparent and opaque passes. 
During the opaque pass, use an alpha test of GL_EQUAL to 1.0. 
This makes the fully-opaque regions of the surfaces appear in the depth buffer 
and avoids any fading of that part of the surface with depth.
-}
resX, resY :: Num a => a
resX = 1920
resY = 1080
projection :: M44 GLfloat
projection = perspective 45 (resX/resY) 0.01 1000

data WBOIT = WBOIT 
    { wboitBlendProgramR :: IO GLProgram
    , wboitAccumTexture :: TextureID
    , wboitRevealageTexture :: TextureID
    , wboitBlendQuad    :: Mesh
    , wboitFramebuffer  :: Framebuffer
    }

main :: IO a
main = do
    -- Create our window
    win <- reacquire 0 (setupGLFW "WBOIT" resX resY)

    (framebuffer, accumTexture, revealageTexture) <- createFramebuffer resX resY

    -- Load the shaders and geometry for our scene
    cubeProgramR  <- createReshaderProgram "test/cube.vert"       "test/cube.frag"
    blendProgramR <- createReshaderProgram "test/blendPass.vert"  "test/blendPass.frag"
    transProgramR <- createReshaderProgram "test/renderPass.vert" "test/renderPass.frag"
    
    transMVPUniform   <- transProgramR >>= flip getShaderUniform "uMVP"
    transColorUniform <- transProgramR >>= flip getShaderUniform "uDiffuseColor"
    
    cube          <- makeCube =<< cubeProgramR
    transQuad     <- makeQuad =<< transProgramR

    
    blendQuad     <- makeQuad =<< blendProgramR

    let wboit = WBOIT
            { wboitBlendProgramR     = blendProgramR
            , wboitAccumTexture      = accumTexture
            , wboitRevealageTexture  = revealageTexture
            , wboitBlendQuad         = blendQuad
            , wboitFramebuffer       = framebuffer
            }

    glEnable GL_DEPTH_TEST
    glClearDepth 1

    -- Begin our renderloop
    forever $ do
        glGetErrors

        -- Get mouse/keyboard/OS events from GLFW
        GLFW.pollEvents

        -------------------------
        -- Render opaque surfaces
        -------------------------
        glClearColor 0.75 0.75 0.75 1

        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glDepthMask GL_TRUE

        let model      = mkTransformation 1 (V3 0 0 (-5))
            view       = lookAt (V3 0 20 5) (V3 0 0 (-50)) (V3 0 1 0)
            mvp        = projection !*! view !*! model
        renderCube cube mvp

        withWBOIT wboit $ do
            useProgram =<< transProgramR
            drawTransQuad transQuad transColorUniform transMVPUniform view (1,0,0,0.75) (-70)
            drawTransQuad transQuad transColorUniform transMVPUniform view (1,1,0,0.75) (-60)
            drawTransQuad transQuad transColorUniform transMVPUniform view (0,0,1,0.75) (-50)

        GLFW.swapBuffers win

withWBOIT WBOIT{..} drawTransparentSurfaces = do
    ---------------------
    -- Begin WBOIT config
    ---------------------

    -- Bind the framebuffer
    glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer wboitFramebuffer)

    withArray [GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1] $ glDrawBuffers 2
    -- Clear the accum to vec4(0) and the revealage to float(1)

    withArray [0,0,0,0] $ glClearBufferfv GL_COLOR 0
    withArray [1,0,0,0] $ glClearBufferfv GL_COLOR 1

    glDepthMask GL_FALSE
    glEnable GL_BLEND
    glBlendEquation GL_FUNC_ADD

    glBlendFunci 0 GL_ONE GL_ONE
    glBlendFunci 1 GL_ZERO GL_ONE_MINUS_SRC_ALPHA

    ----------- Draw our transparent surfaces here

    drawTransparentSurfaces

    ----------------------------------------------

    -- Done drawing transparent surfaces
    glBindFramebuffer GL_FRAMEBUFFER 0
    glDrawBuffer GL_BACK

    -- Blend to screenspace blendQuad
    glBlendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA

    blendProgram <- wboitBlendProgramR
    useProgram blendProgram

    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D (unTextureID wboitAccumTexture)
    glActiveTexture GL_TEXTURE1
    glBindTexture GL_TEXTURE_2D (unTextureID wboitRevealageTexture)
    accumTextureU     <- getShaderUniform blendProgram "accumTexture"
    revealageTextureU <- getShaderUniform blendProgram "revealageTexture"
    glUniform1i (fromIntegral (unUniformLocation accumTextureU))     0
    glUniform1i (fromIntegral (unUniformLocation revealageTextureU)) 1
    -- glDisable GL_BLEND
    drawMesh wboitBlendQuad


drawTransQuad :: Mesh
              -> UniformLocation
              -> UniformLocation
              -> V4 (V4 GLfloat)
              -> (GLfloat, GLfloat, GLfloat, GLfloat)
              -> GLfloat
              -> IO ()
drawTransQuad transQuad transColorUniform transMVPUniform view (r,g,b,a) z = do
    let transModel = mkTransformation 1 (V3 0 0 z)
        transMVP   = projection !*! view !*! transModel
    glUniform4f (unUniformLocation transColorUniform) r g b a
    uniformM44 transMVPUniform transMVP
    drawMesh transQuad


newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

-- | Create the framebuffer we'll render into and pass to the Oculus SDK
createFramebuffer :: GLsizei -> GLsizei -> IO (Framebuffer, TextureID, TextureID)
createFramebuffer sizeX sizeY = do
    accumTexture     <- createFramebufferTexture GL_RGBA16F
    revealageTexture <- createFramebufferTexture GL_R16F

    frameBuffer <- overPtr (glGenFramebuffers 1)
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer

    -- Attach the accumTexture and revealageTexture as color attachments 0 and 1, resp.
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unTextureID accumTexture) 0
    -- (this apparently won't work on GLES)
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT1 GL_TEXTURE_2D (unTextureID revealageTexture) 0

    -- Enable both color attachments
    withArray [GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1] $ glDrawBuffers 2

    checkFramebufferStatus

    -- Unbind the framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0

    return (Framebuffer frameBuffer, accumTexture, revealageTexture)
    where 
        -- | Create and configure the texture to use for our framebuffer
        createFramebufferTexture :: GLenum -> IO TextureID
        createFramebufferTexture storage = do
            texID <- overPtr (glGenTextures 1)
            
            glBindTexture   GL_TEXTURE_2D texID
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
            glTexStorage2D  GL_TEXTURE_2D 1 storage sizeX sizeY
            glBindTexture   GL_TEXTURE_2D 0
            
            return (TextureID texID)

        checkFramebufferStatus = do
            status <- glCheckFramebufferStatus GL_FRAMEBUFFER
            case status of
                GL_FRAMEBUFFER_COMPLETE                      -> return ()
                GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         -> error "Incomplete framebuffer attachment"
                GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS         -> error "Framebuffer images have differing dimensions"
                GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> error "No images attached to framebuffer"
                GL_FRAMEBUFFER_UNSUPPORTED                   -> error "Unsupported framebuffer configuration"


