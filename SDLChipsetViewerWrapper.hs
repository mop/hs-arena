{-# LANGUAGE ForeignFunctionInterface #-}
module SDLChipsetViewerWrapper where
import ChipsetViewer
foreign export ccall "haskell_main" main :: IO ()
