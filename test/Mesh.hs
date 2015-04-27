module Mesh where
import Graphics.GL
import Control.Monad.Trans
import Foreign

newtype VertexArrayObject = VertexArrayObject   { unVertexArrayObject   :: GLuint }

data Mesh = Mesh
        { meshVAO          :: VertexArrayObject
        , meshIndexCount   :: GLsizei
        }

drawMesh :: MonadIO m => Mesh -> m ()
drawMesh mesh = do
    glBindVertexArray (unVertexArrayObject (meshVAO mesh))

    glDrawElements GL_TRIANGLES (meshIndexCount mesh) GL_UNSIGNED_INT nullPtr

    glBindVertexArray 0


