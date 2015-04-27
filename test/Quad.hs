module Quad where

import Shader

import Graphics.GL
import Foreign
import Linear
import Data.Foldable
import Control.Monad.Trans

data Quad = Quad
        { quadVAO        :: VertexArrayObject
        , quadShader     :: GLProgram
        , quadIndexCount :: GLsizei
        , quadUniformMVP :: UniformLocation
        }

----------------------------------------------------------
-- Make Quad
----------------------------------------------------------

renderQuad :: Quad -> M44 GLfloat -> IO ()
renderQuad quad mvp = do

    useProgram (quadShader quad)

    let mvpUniformLoc = fromIntegral (unUniformLocation (quadUniformMVP quad))
    
    withArray (concatMap toList (transpose mvp)) (\mvpPointer ->
        glUniformMatrix4fv mvpUniformLoc 1 GL_FALSE mvpPointer)

    drawQuad quad

drawQuad :: MonadIO m => Quad -> m ()
drawQuad quad = do
    glBindVertexArray (unVertexArrayObject (quadVAO quad))

    glDrawElements GL_TRIANGLES (quadIndexCount quad) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0


makeQuad :: GLProgram -> IO Quad
makeQuad program = do

    aPosition <- getShaderAttribute program "aPosition"
    uMVP      <- getShaderUniform   program "uMVP"

    -- Setup a VAO
    vaoQuad <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoQuad


    -----------------
    -- Quad Positions
    -----------------
    
    -- Buffer the quad vertices
    let quadVertices = 
            [ -1.0 , -1.0 ,  1.0  
            ,  1.0 , -1.0 ,  1.0  
            ,  1.0 ,  1.0 ,  1.0  
            , -1.0 ,  1.0 ,  1.0 ] :: [GLfloat]

    vaoQuadVertices <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoQuadVertices

    let quadVerticesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length quadVertices)

    withArray quadVertices $ 
        \quadVerticesPtr ->
            glBufferData GL_ARRAY_BUFFER quadVerticesSize (castPtr quadVerticesPtr) GL_STATIC_DRAW 

    -- Describe our vertices array to OpenGL
    glEnableVertexAttribArray (fromIntegral (unAttributeLocation aPosition))

    glVertexAttribPointer
        (fromIntegral (unAttributeLocation aPosition)) -- attribute
        3                 -- number of elements per vertex, here (x,y,z)
        GL_FLOAT          -- the type of each element
        GL_FALSE          -- don't normalize
        0                 -- no extra data between each position
        nullPtr           -- offset of first element


    ----------------
    -- Quad Indicies
    ----------------

    -- Buffer the quad indices
    let quadIndices = 
            [ 0, 1, 2
            , 2, 3, 0 ] :: [GLuint]
    
    iboQuadElements <- overPtr (glGenBuffers 1)
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER iboQuadElements

    let quadElementsSize = fromIntegral (sizeOf (undefined :: GLuint) * length quadIndices)
    
    withArray quadIndices $ 
        \quadIndicesPtr ->
            glBufferData GL_ELEMENT_ARRAY_BUFFER quadElementsSize (castPtr quadIndicesPtr) GL_STATIC_DRAW
    
    glBindVertexArray 0

    return $ Quad 
        { quadVAO        = VertexArrayObject vaoQuad
        , quadShader     = program
        , quadIndexCount = fromIntegral (length quadIndices)
        , quadUniformMVP = uMVP
        } 
