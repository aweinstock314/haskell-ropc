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
import System.Environment
import qualified Data.Vector as V

foreign import ccall "get_section_by_name"
    rawGetSectionByName :: CString -> CString -> Ptr Int -> IO (Ptr Word8)

getSectionByName :: String -> String -> IO (Maybe (V.Vector Word8))
getSectionByName fname section =
    withCString fname $ \pFname ->
    withCString section $ \pSection ->
    with 0 $ \pSize -> do
        result <- rawGetSectionByName pFname pSection pSize
        if result == nullPtr then return Nothing else do
            size <- peek pSize
            vec <- V.generateM size (peekByteOff result)
            free result
            return $ Just vec

eitherToMaybe = either (const Nothing) Just

getGadgets backLength codeVec = nub . validate . concat $ map tryPosition retIdxs where
    retIdxs = V.toList $ V.findIndices (== 0xc3) codeVec
    len = V.length codeVec
    invalidSlice i j = (i+j) > len || i < 0
    trySlice :: Int -> Int -> Maybe [Instruction]
    trySlice i j | invalidSlice i j = Nothing
    trySlice i j = eitherToMaybe . runIdentity . disassembleList . V.toList $ V.slice i j codeVec
    tryPosition j = catMaybes $ map (\i -> fmap ((,)(j-i)) $ trySlice (j-i) (i+1)) [0..backLength]
    isValid (Instruction op _ _ _ _) = show op /= "InvalidOpcode"
    isValid _ = False
    validate = filter (all isValid . snd)


showHexList lst = "[" ++ concat (intersperse "," (map (flip showHex "") lst)) ++ "]"

myShowInstruction (Instruction op _ args _ bytes) = show op ++ showHexList bytes
myShowInstruction (BadInstruction a b c d) = "???" ++ show (a,b,c,d)
myShowInstruction (PseudoInstruction a b) = "???" ++ show (a,b)

main = do
    args <- getArgs
    case args of
        [] -> main' "./a.out" "5" ".text"
        [fname] -> main' fname "5" ".text"
        [fname, backLen] -> main' fname backLen ".text"
        [fname, backLen, section] -> main' fname backLen section

main' fname numGadgets section = do
    textSection' <- getSectionByName fname section
    let textSection = maybe (error $ concat ["Unable to read section ", section, " of ", fname]) id textSection'
    V.mapM_ (putStr . flip showHex "" . fromIntegral) textSection
    putStrLn ""
    let gadgets = getGadgets (read numGadgets) textSection
    putStr . unlines $ map (show . second (map myShowInstruction)) gadgets
