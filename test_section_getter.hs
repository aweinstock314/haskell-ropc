{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Numeric
import qualified Data.Vector as V

foreign import ccall "get_section_by_name"
    rawGetSectionByName :: CString -> CString -> Ptr Int -> IO (Ptr Word8)

getSectionByName :: String -> String -> IO (V.Vector Word8)
getSectionByName fname section =
    withCString fname $ \pFname ->
    withCString section $ \pSection ->
    with 0 $ \pSize -> do
        result <- rawGetSectionByName pFname pSection pSize
        size <- peek pSize
        vec <- V.generateM size (peekByteOff result)
        free result
        return vec

main = do
    textSection <- getSectionByName "./a.out" ".text"
    V.mapM_ (putStr . flip showHex "" . fromIntegral) textSection
    putStrLn ""
