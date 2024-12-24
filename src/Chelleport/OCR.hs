module Chelleport.OCR (getWordsOnScreen) where

import Chelleport.Types
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (asks)
import qualified Data.ByteString as BS
import Foreign (Bits (shiftR), Ptr, Storable (peek, pokeByteOff), alloca, allocaBytes, peekArray, (.&.))
import Foreign.C (CInt, CString, newCString)
import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (WriteMode))
import qualified Graphics.X11 as X11
import qualified SDL
import System.Directory (removeFile)
import System.IO (hPutStrLn)
import System.IO.Temp (emptySystemTempFile)

foreign import ccall unsafe "libchelleport.h findWordCoordinates"
  c_findWordCoordinates :: CString -> Ptr CInt -> IO (Ptr OCRMatch)

class (Monad m) => MonadOCR m where
  getWordsOnScreen :: m [OCRMatch]

instance (MonadIO m) => MonadOCR (AppM m) where
  getWordsOnScreen = do
    SDL.V2 width height <- asks ctxWindow >>= SDL.get . SDL.windowSize
    SDL.V2 x y <- asks ctxWindow >>= SDL.getWindowAbsolutePosition
    liftIO $ do
      imgFilePath <- liftIO $ createTemporaryScreenshot (x, y) (width, height)
      findWordCoordinates imgFilePath <* removeFile imgFilePath

findWordCoordinates :: String -> IO [OCRMatch]
findWordCoordinates imgPath = alloca $ \sizePtr -> do
  imgPathC <- newCString imgPath
  arrayPtr <- c_findWordCoordinates imgPathC sizePtr

  size <- peek sizePtr
  peekArray (fromIntegral size) arrayPtr

createTemporaryScreenshot :: (CInt, CInt) -> (CInt, CInt) -> IO String
createTemporaryScreenshot offset size = do
  tmpFilePath <- emptySystemTempFile "chelleport-screenshot.png"
  screenshot tmpFilePath offset size
  pure tmpFilePath

screenshot :: String -> (CInt, CInt) -> (CInt, CInt) -> IO ()
screenshot filename (offsetX, offsetY) (width, height) = do
  dpy <- X11.openDisplay ""
  root <- X11.rootWindow dpy (X11.defaultScreen dpy)

  image <-
    X11.getImage
      dpy
      root
      offsetX
      offsetY
      (fromIntegral width)
      (fromIntegral height)
      (fromIntegral X11.allPlanes_aux)
      X11.zPixmap

  allocaBytes (fromIntegral $ width * height * 3) $ \ptr -> do
    let getPixel :: CInt -> CInt -> IO ()
        getPixel x y = do
          pixel <- X11.xGetPixel image x y
          let r = pixel `shiftR` 16 .&. 0xFF
          let g = pixel `shiftR` 8 .&. 0xFF
          let b = pixel .&. 0xFF
          pokeByteOff ptr (fromIntegral (y * width + x) * 3) r
          pokeByteOff ptr (fromIntegral (y * width + x) * 3 + 1) g
          pokeByteOff ptr (fromIntegral (y * width + x) * 3 + 2) b

    sequence_ [getPixel x y | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    rgbData <- BS.packCStringLen (ptr, fromIntegral $ width * height * 3)
    savePPMFile filename (fromIntegral width) (fromIntegral height) rgbData

  X11.destroyImage image
  X11.closeDisplay dpy

savePPMFile :: FilePath -> Int -> Int -> BS.ByteString -> IO ()
savePPMFile path width height rgbData = withFile path WriteMode $ \h -> do
  hPutStrLn h "P6"
  hPutStrLn h $ show width ++ " " ++ show height
  hPutStrLn h "255"
  BS.hPut h rgbData
