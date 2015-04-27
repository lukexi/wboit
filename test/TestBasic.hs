{-# LANGUAGE RecordWildCards #-}
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GL.Freetype

import Data.Bits
import Control.Monad
import Linear

import SetupGLFW
import ShaderLoader
-- import Cube
import GlyphQuad

-------------------------------------------------------------
-- A test to make sure font rendering works
-------------------------------------------------------------

resX, resY :: Num a => a
resX = 1920
resY = 1080

main :: IO a
main = do

    win <- setupGLFW "Freetype-GL" resX resY

    -- Test Freetype
    atlas <- newTextureAtlas 512 512 BitDepth1
    -- font  <- newFontFromFile atlas 100 "freetype-gl/fonts/SourceSansPro-Regular.ttf"
    font  <- newFontFromFile atlas 50 "freetype-gl/fonts/Vera.ttf"

    let text = "And he said his name was BEAR-A..."
    missed <- loadFontGlyphs font text
    putStrLn $ "Missed: " ++ show missed

    let textureID = TextureID (atlasTextureID atlas)

    glyphQuadProg <- createShaderProgram "test/glyphQuad.vert" "test/glyphQuad.frag"
    
    (quads, xOffset, _) <- foldM (\(quads, xOffset, maybeLastChar) thisChar -> do
        glyph <- getGlyph font thisChar
        kerning <- case maybeLastChar of
            Nothing       -> return 0
            Just lastChar -> getGlyphKerning glyph lastChar
        
        glyphMetrics            <- getGlyphMetrics glyph
        (newXOffset, glyphQuad) <- makeGlyphQuad glyphQuadProg textureID glyphMetrics (xOffset, 0) kerning
        return (glyphQuad:quads, newXOffset, Just thisChar)
        ) ([], 0, Nothing) text

    -- Scene rendering setup
    -- cubeProg <- createShaderProgram "test/cube.vert" "test/cube.frag"
    
    -- cube <- makeCube cubeProg

    glClearColor 0 0.1 0.1 1
    glEnable GL_DEPTH_TEST

    forever $ 
        mainLoop win quads (-xOffset/2)


mainLoop :: GLFW.Window -> [GlyphQuad] -> Float -> IO ()
mainLoop win glyphQuads xOffset = do
    -- glGetErrors

    -- Get mouse/keyboard/OS events from GLFW
    GLFW.pollEvents

    -- Clear the framebuffer
    glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    -- Render our scene
    let projection = perspective 45 (resX/resY) 0.01 1000
        model      = mkTransformation 1 (V3 (realToFrac xOffset) 0 (-4))
        view       = lookAt (V3 0 2 500) (V3 0 0 (-4)) (V3 0 1 0)
        mvp        = projection !*! view !*! model
        (x,y,w,h)  = (0,0,1920,1080)

    glViewport x y w h

    -- renderCube cube mvp
    forM_ glyphQuads (\glyphQuad -> renderGlyphQuad glyphQuad mvp)
    
    GLFW.swapBuffers win



