module TransCube where

import           Shader

import           Data.Foldable
import           Foreign
import           Graphics.GL
import           Linear

import           Mesh

data Cube = Cube
        { cubeVAO        :: VertexArrayObject
        , cubeShader     :: GLProgram
        , cubeIndexCount :: GLsizei
        , cubeUniformMVP :: UniformLocation
        }

----------------------------------------------------------
-- Make Cube
----------------------------------------------------------

renderTransCube :: Cube -> M44 GLfloat -> IO ()
renderTransCube cube mvp = do

    useProgram (cubeShader cube)

    uniformM44 (cubeUniformMVP cube) mvp

    glBindVertexArray (unVertexArrayObject (cubeVAO cube))

    glDrawElements GL_TRIANGLES (cubeIndexCount cube) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0


makeTransCube :: GLProgram -> IO Cube
makeTransCube program = do

    aPosition <- getShaderAttribute program "aPosition"
    uMVP      <- getShaderUniform   program "uMVP"

    -- Setup a VAO
    vaoCube <- overPtr (glGenVertexArrays 1)

    glBindVertexArray vaoCube


    -----------------
    -- Cube Positions
    -----------------

    -- Buffer the cube vertices
    let cubeVertices =
            --- front
            [ -1.0 , -1.0 ,  1.0
            ,  1.0 , -1.0 ,  1.0
            ,  1.0 ,  1.0 ,  1.0
            , -1.0 ,  1.0 ,  1.0

            --- back
            , -1.0 , -1.0 , -1.0
            ,  1.0 , -1.0 , -1.0
            ,  1.0 ,  1.0 , -1.0
            , -1.0 ,  1.0 , -1.0 ] :: [GLfloat]



    vaoCubeVertices <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ARRAY_BUFFER vaoCubeVertices



    let cubeVerticesSize = fromIntegral (sizeOf (undefined :: GLfloat) * length cubeVertices)

    withArray cubeVertices $
        \cubeVerticesPtr ->
            glBufferData GL_ARRAY_BUFFER cubeVerticesSize (castPtr cubeVerticesPtr) GL_STATIC_DRAW

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
    -- Cube Indicies
    ----------------

    -- Buffer the cube indices
    let cubeIndices =
            -- front
            [ 0, 1, 2
            , 2, 3, 0
            -- top
            , 1, 5, 6
            , 6, 2, 1
            -- back
            , 7, 6, 5
            , 5, 4, 7
            -- bottom
            , 4, 0, 3
            , 3, 7, 4
            -- left
            , 4, 5, 1
            , 1, 0, 4
            -- right
            , 3, 2, 6
            , 6, 7, 3 ] :: [GLuint]

    iboCubeElements <- overPtr (glGenBuffers 1)

    glBindBuffer GL_ELEMENT_ARRAY_BUFFER iboCubeElements

    let cubeElementsSize = fromIntegral (sizeOf (undefined :: GLuint) * length cubeIndices)

    withArray cubeIndices $
        \cubeIndicesPtr ->
            glBufferData GL_ELEMENT_ARRAY_BUFFER cubeElementsSize (castPtr cubeIndicesPtr) GL_STATIC_DRAW

    glBindVertexArray 0

    return $ Cube
        { cubeVAO        = VertexArrayObject vaoCube
        , cubeShader     = program
        , cubeIndexCount = fromIntegral (length cubeIndices)
        , cubeUniformMVP = uMVP
        }
