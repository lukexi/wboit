{-# LANGUAGE RecordWildCards #-}
module WBOIT where
import           Graphics.GL
import qualified Graphics.UI.GLFW as GLFW

import           Control.Monad
import           Data.Bits
import           Foreign
import           Linear

import           Cube
import           Data.Time
import           Halive.Utils
import           Mesh
import           Quad
import           SetupGLFW
import           Shader
import           TransCube

data WBOIT = WBOIT
    { wboitBlendProgram                 :: GLProgram
    , wboitAccumTexture                 :: TextureID
    , wboitRevealageTexture             :: TextureID
    , wboitBlendQuad                    :: Mesh
    , wboitFramebuffer                  :: Framebuffer
    , wboitBlendAccumTextureUniform     :: UniformLocation
    , wboitBlendRevealageTextureUniform :: UniformLocation
    }


createWBOIT resX resY = do
    (framebuffer, accumTexture, revealageTexture) <- createFramebuffer (resX*2) (resY*2)

    blendProgram <- createShaderProgram "test/blendPass.vert"  "test/blendPass.frag"
    
    blendQuad    <- makeQuad blendProgram

    blendAccumTextureUniform     <- getShaderUniform blendProgram "accumTexture"
    blendRevealageTextureUniform <- getShaderUniform blendProgram "revealageTexture"

    let wboit = WBOIT
            { wboitBlendProgram                 = blendProgram
            , wboitAccumTexture                 = accumTexture
            , wboitRevealageTexture             = revealageTexture
            , wboitBlendQuad                    = blendQuad
            , wboitFramebuffer                  = framebuffer
            , wboitBlendAccumTextureUniform     = blendAccumTextureUniform
            , wboitBlendRevealageTextureUniform = blendRevealageTextureUniform
            }
    return wboit



-- withWBOIT :: WBOIT -> IO a -> IO ()
withWBOIT WBOIT{..} resX resY drawTransparentSurfaces = do
    ---------------------------
    -- Draw into WBOIT textures
    ---------------------------

    -- Copy the opaque depth buffer into the WBOIT framebuffer
    -- so we can z-reject transparent objects behind opaque ones
    glBindFramebuffer GL_READ_FRAMEBUFFER 0 -- The normal depth buffer
    glBindFramebuffer GL_DRAW_FRAMEBUFFER (unFramebuffer wboitFramebuffer)
    glBlitFramebuffer 0 0 (resX*2) (resY*2)
                      0 0 (resX*2) (resY*2)
                      GL_DEPTH_BUFFER_BIT GL_NEAREST

    -- Bind the WBOIT framebuffer
    glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer wboitFramebuffer)

    -- Set draw buffers to be both color attachments
    withArray [GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1] $ glDrawBuffers 2

    -- Clear the color buffers
    withArray [0,0,0,0] $ glClearBufferfv GL_COLOR 0 -- clear accum to vec4(0)
    withArray [1,0,0,0] $ glClearBufferfv GL_COLOR 1 -- clear revealage to float(1)

    -- Disable writing to depth buffer, enable additive blending
    glDepthMask GL_FALSE
    glEnable GL_BLEND

    -- Use 1,1 for accum blending, 0,1-srcAlpha for revealage
    glBlendFunci 0 GL_ONE GL_ONE
    glBlendFunci 1 GL_ZERO GL_ONE_MINUS_SRC_ALPHA

    -- Draw our transparent surfaces into the textures
    drawTransparentSurfaces



    ---------------------------------------------
    -- Draw WBOIT textures to a screen-space quad
    ---------------------------------------------

    -- Restore the regular framebuffer
    glBindFramebuffer GL_FRAMEBUFFER 0
    glDrawBuffer GL_BACK

    -- Blend to screenspace blendQuad
    -- Composite the WBOIT framebebuffer with 1-srcAlpha, srcAlpha
    glBlendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA

    
    -- Bind the accum and revealage textures so
    -- they can be blitted when drawing the mesh
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D (unTextureID wboitAccumTexture)
    glActiveTexture GL_TEXTURE1
    glBindTexture GL_TEXTURE_2D (unTextureID wboitRevealageTexture)


    useProgram wboitBlendProgram
    glUniform1i (fromIntegral (unUniformLocation wboitBlendAccumTextureUniform))     0
    glUniform1i (fromIntegral (unUniformLocation wboitBlendRevealageTextureUniform)) 1


    drawMesh wboitBlendQuad

    -- Restore state
    glDepthMask GL_TRUE
    glDisable GL_BLEND





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

    -- Add a depth buffer to the FBO
    depthBuffer <- overPtr (glGenRenderbuffers 1)
    glBindRenderbuffer GL_RENDERBUFFER depthBuffer
    glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT sizeX sizeY
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER depthBuffer

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
