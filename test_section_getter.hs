{-# LANGUAGE ForeignFunctionInterface #-}
--module ROPc where
import Control.Arrow
import Control.Monad.Identity
import Data.List
import Data.Char
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Harpy.X86Disassembler
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

eitherToMaybe = either (const Nothing) Just

getGadgets backLength codeVec = nub . concat $ map tryPosition (V.toList retIdxs) where
    retIdxs = V.findIndices (== 0xc3) codeVec
    len = V.length codeVec
    invalidSlice i j = (i+j) > len || i < 0
    trySlice :: Int -> Int -> Maybe [Instruction]
    trySlice i j | invalidSlice i j = Nothing
    trySlice i j = eitherToMaybe . runIdentity . disassembleList . V.toList $ V.slice i j codeVec
    tryPosition j = catMaybes $ map (\i -> fmap ((,)i) $ trySlice (j-i) (i+1)) [0..backLength]

main = do
    textSection <- getSectionByName "./a.out" ".text"
    V.mapM_ (putStr . flip showHex "" . fromIntegral) textSection
    putStrLn ""
    let gadgets = getGadgets 5 textSection
    putStr . unlines $ map (show . second show) gadgets
