{-# LANGUAGE RecordWildCards #-}

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
import           WBOIT

---------------------
-- Implementing WBOIT
---------------------

{-
NOTE: we need to preserve the depth buffer for
    positional time-warp... is that possible?

Update: we are now blitting the opaque depth buffer into the wboit buffer,
and we could write to it (but not use depth testing) during wboit
and then blit it back again.
-}


resX, resY :: Num a => a
resX = 640
resY = 480

main :: IO a
main = do
    -- Create our window
    win <- reacquire 0 (setupGLFW "WBOIT" resX resY)

    wboit <- createWBOIT resX resY

    -- Load the shaders and geometry for our scene
    cubeProgram       <- createShaderProgram "test/cube.vert"       "test/cube.frag"

    transProgram      <- createShaderProgram "test/renderPass.vert" "test/renderPass.frag"

    transMVPUniform   <- getShaderUniform transProgram "uMVP"
    transColorUniform <- getShaderUniform transProgram "uDiffuseColor"


    opaqueCube        <- makeCube cubeProgram
    transCube         <- makeTransCube transProgram
    transQuad         <- makeQuad transProgram


    glEnable GL_DEPTH_TEST
    glClearDepth 1
    glClearColor 0.0 0.0 0.0 1

    -- Begin our renderloop
    forever $ do
        now <- realToFrac . utctDayTime <$> getCurrentTime

        glGetErrors

        -- Get mouse/keyboard/OS events from GLFW
        GLFW.pollEvents

        -- Recalculate projection and camera view
        (w,h)<- GLFW.getWindowSize win
        let projection = perspective 45 (fromIntegral w/fromIntegral h) 0.01 1000
            view       = lookAt (V3 0 2 5) (V3 0 0 (-50)) (V3 0 1 0)
            -- view       = lookAt (V3 0 20 5) (V3 0 0 (-50)) (V3 0 1 0)
            viewProj   = projection !*! view

        -- Render opaque surfaces
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        glDepthMask GL_TRUE

        let wav1 = (+ 1) . sin $ now/2
            wav2 = (+ 1) . sin $ now/3 + 1
        let cubeModel  = mkTransformation 1 (V3 0 0 ((-29) * wav2))
            cubeModel2  = mkTransformation 1 (V3 0 0 ((-30) * wav1))

        renderCube opaqueCube (viewProj !*! cubeModel2)

        -- Render transparent surfaces
        withWBOIT wboit resX resY $ do
            useProgram transProgram
            let cubeModel  = mkTransformation (axisAngle (V3 0 1 0) wav1) (V3 0 0 ((-29) * wav2))
            glUniform4f (unUniformLocation transColorUniform) 0 1 1 0.3
            renderTransCube transCube (viewProj !*! cubeModel)
            drawTransQuad transQuad transColorUniform transMVPUniform viewProj (1,0,0,0.75) (-30)
            drawTransQuad transQuad transColorUniform transMVPUniform viewProj (1,1,0,0.75) (-20)
            -- drawTransQuad transQuad transColorUniform transMVPUniform viewProj (0,0,1,0.75) (-10)
            -- drawTransQuad transQuad transColorUniform transMVPUniform viewProj (1,0,1,0.75) ((-31) * wav1)
            -- drawTransQuad transQuad transColorUniform transMVPUniform viewProj (0,1,0,0.75) (-8)

        GLFW.swapBuffers win

drawTransQuad :: Mesh
              -> UniformLocation
              -> UniformLocation
              -> V4 (V4 GLfloat)
              -> (GLfloat, GLfloat, GLfloat, GLfloat)
              -> GLfloat
              -> IO ()
drawTransQuad transQuad transColorUniform transMVPUniform viewProj (r,g,b,a) z = do
    let transModel = mkTransformation 1 (V3 0 0 z)
        transMVP   = viewProj !*! transModel
    glUniform4f (unUniformLocation transColorUniform) r g b a
    uniformM44 transMVPUniform transMVP
    drawMesh transQuad
