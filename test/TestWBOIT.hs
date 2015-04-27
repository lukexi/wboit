
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

main :: IO a
main = do    
    -- Create our window
    win <- setupGLFW "WBOIT" resX resY


    (frameBuffer, accumTexture, revealageTexture) <- createFrameBuffer resX resY

    -- Load the shaders and geometry for our scene
    blendProgram  <- createShaderProgram "test/blendPass.vert"  "test/blendPass.frag"
    renderProgram <- createShaderProgram "test/renderPass.vert" "test/renderPass.frag"
    cubeProgram   <- createShaderProgram "test/cube.vert"       "test/cube.frag"
    cube          <- makeCube cubeProgram
    quad          <- makeQuad renderProgram

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    -- Begin our renderloop
    forever $ do
        -- glGetErrors

        -- Get mouse/keyboard/OS events from GLFW
        GLFW.pollEvents

        -- Bind the eye texture as the frame buffer to render into
        glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer frameBuffer)

        -- Clear the framebuffer
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        ------------------
        -- Render our cube
        ------------------
            
        -- Render our scene
        let model      = mkTransformation 1 (V3 0 0 (-4))
            view       = lookAt (V3 0 2 500) (V3 0 0 (-4)) (V3 0 1 0)
            mvp        = projection !*! view !*! model

        glDepthMask GL_FALSE
        glEnable GL_BLEND
        
        withArray [GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1] $ glDrawBuffers 2

        glBlendFunci 0 GL_ONE GL_ONE
        glBlendFunci 1 GL_ZERO GL_ONE_MINUS_SRC_ALPHA

        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D (unTextureID accumTexture)
        glActiveTexture GL_TEXTURE1
        glBindTexture GL_TEXTURE_2D (unTextureID revealageTexture)

        renderCube cube mvp

        glBindFramebuffer GL_FRAMEBUFFER 0

        -- Blend to screenspace quad
        glBlendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
        useProgram blendProgram

        accumTextureU     <- getShaderUniform blendProgram "accumTexture"
        revealageTextureU <- getShaderUniform blendProgram "revealageTexture"
        glUniform1i (fromIntegral (unUniformLocation accumTextureU))     0
        glUniform1i (fromIntegral (unUniformLocation revealageTextureU)) 1

        drawQuad quad

        GLFW.swapBuffers win


newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

-- | Create the framebuffer we'll render into and pass to the Oculus SDK
createFrameBuffer :: GLsizei -> GLsizei -> IO (Framebuffer, TextureID, TextureID)
createFrameBuffer sizeX sizeY = do
    frameBufferTexture0 <- createFrameBufferTexture
    frameBufferTexture1 <- createFrameBufferTexture

    frameBuffer <- overPtr (glGenFramebuffers 1)

    -- Attach the eye texture as the color buffer
    glBindFramebuffer GL_FRAMEBUFFER frameBuffer
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unTextureID frameBufferTexture0) 0
    -- (this apparently won't work on GLES)
    glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT1 GL_TEXTURE_2D (unTextureID frameBufferTexture1) 0


    -- Generate a render buffer for depth
    overPtr (glGenRenderbuffers 1) >>= \renderBuffer -> do
        -- Configure the depth buffer dimensions to match the eye texture
        glBindRenderbuffer GL_RENDERBUFFER renderBuffer
        glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 sizeX sizeY
        glBindRenderbuffer GL_RENDERBUFFER 0

        -- Attach the render buffer as the depth target
        glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER renderBuffer

    -- Unbind the framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0

    return (Framebuffer frameBuffer, frameBufferTexture0, frameBufferTexture1)
    where 
        -- | Create and configure the texture to use for our framebuffer
        createFrameBufferTexture :: IO TextureID
        createFrameBufferTexture = do
            texID <- overPtr (glGenTextures 1)
            
            glBindTexture   GL_TEXTURE_2D texID
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
            glTexStorage2D  GL_TEXTURE_2D 1 GL_RGBA8 sizeX sizeY
            glBindTexture   GL_TEXTURE_2D 0
            
            return (TextureID texID)


