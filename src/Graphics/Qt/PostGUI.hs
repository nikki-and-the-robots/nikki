{-# language ForeignFunctionInterface #-}


-- | Module implementing operations to execute arbitrary IO-actions in the GUI thread.

module Graphics.Qt.PostGUI (
    postGUI,
  ) where


import Foreign.Ptr

import Graphics.Qt.CPPWrapper


-- | Non-blocking operations, that gets the gui thread to perform the given action.
postGUI :: Ptr GLContext -> IO () -> IO ()
postGUI widget action = cppPostGUI widget =<< wrapGuiAction action

foreign import ccall "postGUI" cppPostGUI :: Ptr GLContext -> FunPtr (IO ()) -> IO ()

foreign import ccall "wrapper" wrapGuiAction ::
    IO () -> IO (FunPtr (IO ()))

