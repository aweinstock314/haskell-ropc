{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction, TemplateHaskell #-}
--module ROPc where
import Control.Arrow
import Control.Monad.Identity
import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Harpy.X86Disassembler
import Numeric
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Lazy as L
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

getSectionByName_ fname section = fmap
    (maybe (error $ concat ["Unable to read section \"", section, "\" of \"", fname, "\""]) id) $
    getSectionByName fname section

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

toLBS = L.pack . map (fromIntegral . ord)
fromLBS = map (chr . fromIntegral) . L.unpack

showAsJSON = fromLBS . encode . toJSON

fmap concat $ mapM (AT.deriveJSON AT.defaultOptions) [''InstrOperandSize, ''Operand, ''Opcode, ''Instruction]

instance Show Operand where show = showAsJSON

myShowInstruction (Instruction op _ args _ bytes) = show op ++ showHexList bytes
myShowInstruction (BadInstruction a b c d) = "???" ++ show (a,b,c,d)
myShowInstruction (PseudoInstruction a b) = "???" ++ show (a,b)

data Options = Options {
    optFilename :: String,
    optSection :: String,
    optGadgetLength :: Int
    }

defaultOptions = Options "a.out" ".text" 5

showHelp :: IO a
showHelp = do
    progName <- getProgName
    hPutStrLn stderr $ usageInfo (concat ["Usage: ", progName, " SUBCOMMAND [OPTIONS]"]) options
    hPutStrLn stderr $ "Subcommands:\n" ++ unlines [
        "\tdump\tDump the specified sections in the specified executable",
        "\tsearch\tSearch for ROP gadgets in the specified executable"
        ]
    exitSuccess

options = [
    Option "f" ["file"] (ReqArg (\arg opt -> return opt { optFilename = arg }) "FILENAME") "Name of executable",
    Option "s" ["section"] (ReqArg (\arg opt -> return opt { optSection = arg }) "SECTION") "Name of section",
    Option "l" ["length"] (ReqArg (\arg opt -> return opt { optGadgetLength = read arg }) "LENGTH") "Maximum length of gadgets to search for",
    Option "h?" ["help"] (NoArg $ \_ -> showHelp) "Show this help menu"
    ]

parse args = let (actions, _, _) = getOpt RequireOrder options args in foldl (>>=) (return defaultOptions) actions

main = do
    args <- getArgs
    case args of
        "dump":rest -> parse rest >>= main_dump
        "search":rest -> parse rest >>= main_search
        _ -> showHelp

main_dump (Options fname section _) = do
    textSection <- getSectionByName_ fname section
    V.mapM_ (putStr . flip showHex "" . fromIntegral) textSection
    putStrLn ""

main_search (Options fname section maxGadgetLen) =  do
    textSection <- getSectionByName_ fname section
    let gadgets = getGadgets maxGadgetLen textSection
    putStr . unlines $ map showAsJSON gadgets
