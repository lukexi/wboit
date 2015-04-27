{-# LANGUAGE OverloadedStrings #-}
module Reshader where
import System.FSNotify
import Shader
import Data.IORef
import Control.Concurrent
import Control.Monad
import Filesystem.Path
import Data.String
import Graphics.GL

createReshaderProgram :: String -> String -> IO (IO GLProgram)
createReshaderProgram vertexShaderPath fragmentShaderPath = do
    
    let vsName = filename (fromString vertexShaderPath)
        fsName = filename (fromString fragmentShaderPath)

    initialShader <- createShaderProgram vertexShaderPath fragmentShaderPath
    shaderRef     <- newIORef initialShader

    let predicate event = case event of
            Modified path _ -> filename path `elem` [vsName, fsName]
            _               -> False
        recompile event = do
            putStrLn $ "Recompiling due to event: " ++ show event
            newShader@(GLProgram prog) <- createShaderProgram vertexShaderPath fragmentShaderPath
            linked <- overPtr (glGetProgramiv prog GL_LINK_STATUS)
            when (linked == GL_TRUE) $ 
                writeIORef shaderRef newShader
    forkIO $ 
        withManager $ \mgr -> do
            -- start a watching job (in the background)
            watchTree
              mgr          -- manager
              "."          -- directory to watch
              predicate    -- predicate
              recompile    -- action
            forever $ threadDelay 10000000

    return (readIORef shaderRef)