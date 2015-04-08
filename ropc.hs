{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction, TemplateHaskell #-}
--module ROPc where
import Control.Arrow
import Control.Monad.Identity
import Data.Aeson
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Word
import Foreign
import Foreign.C
import Hdis86
import Numeric
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Vector.Storable as V

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
            gcPtr <- newForeignPtr finalizerFree result
            return . Just $ V.unsafeFromForeignPtr gcPtr 0 size

getSectionByName_ fname section = fmap
    (maybe (error $ concat ["Unable to read section \"", section, "\" of \"", fname, "\""]) id) $
    getSectionByName fname section

eitherToMaybe = either (const Nothing) Just

getGadgets backLength codeVec = nub . validate . concat $ map tryPosition retIdxs where
    retIdxs = V.ifoldr (\i x -> if isJumpStart i x then (i:) else id) [] codeVec
    isJumpStart i x = any (\j -> maybe False id . fmap ((`elem` [Iret]) . inOpcode . head) $ trySlice i j) [1..8]
    len = V.length codeVec
    invalidSlice i j = (i+j) > len || i < 0
    maybeSlice i j | invalidSlice i j = Nothing
    maybeSlice i j = Just $ V.slice i j codeVec
    trySlice i j = maybeSlice i j >>= (Just . disassemble intel32 . L.toStrict . L.pack . V.toList)
    tryPosition j = catMaybes $ map (\i -> fmap ((,)(j-i)) $ trySlice (j-i) (i+1)) [0..backLength]
    isValid = not . (== Iinvalid) . inOpcode
    validate = filter (all isValid . snd)

showHexList lst = "[" ++ concat (intersperse "," (map (flip showHex "") lst)) ++ "]"

toLBS = L.pack . map (fromIntegral . ord)
fromLBS = map (chr . fromIntegral) . L.unpack

showAsJSON = fromLBS . encode . toJSON

fmap concat $ mapM (AT.deriveJSON AT.defaultOptions) [
    ''ControlRegister,
    ''DebugRegister,
    ''GPR,
    ''Half,
    ''Immediate,
    ''Instruction,
    ''MMXRegister,
    ''Memory,
    ''Opcode,
    ''Operand,
    ''Pointer,
    ''Prefix,
    ''Register,
    ''Segment,
    ''WordSize,
    ''X87Register,
    ''XMMRegister]

data Options = Options {
    optFilename :: String,
    optSection :: String,
    optGadgetLength :: Int
    }

defaultOptions = Options "a.out" ".text" 5

showHelp :: String -> [OptDescr (Options -> IO Options)] -> IO a
showHelp subcommand options = do
    progName <- getProgName
    hPutStrLn stderr $ usageInfo (concat ["Usage: ", progName, " ", subcommand, " [OPTIONS]"]) options
    exitSuccess

showSubcommands :: IO a
showSubcommands = do
    progName <- getProgName
    hPutStrLn stderr $ concat ["Usage: ", progName, " SUBCOMMAND [OPTIONS]"]
    hPutStr stderr $ "Subcommands:\n" ++ unlines [
        "\tdump\tDump the specified sections in the specified executable",
        "\tsearch\tSearch for ROP gadgets in the specified executable"
        ]
    exitSuccess

subprogram_options = M.fromList [
    ("dump", (main_dump, fix $ \x -> [filename, section, help "dump" x])),
    ("search", (main_search, fix $ \x -> [filename, section, gadgetLength, help "search" x]))
    ] where
    filename = Option "f" ["file"] (ReqArg (\arg opt -> return opt { optFilename = arg }) "FILENAME") "Name of executable"
    section = Option "s" ["section"] (ReqArg (\arg opt -> return opt { optSection = arg }) "SECTION") "Name of section"
    gadgetLength = Option "l" ["length"] (ReqArg (\arg opt -> return opt { optGadgetLength = read arg }) "LENGTH") "Maximum length of gadgets to search for"
    help subcommand options = Option "h?" ["help"] (NoArg $ \_ -> showHelp subcommand options) "Show this help menu"

parse options args = let (actions, _, _) = getOpt RequireOrder options args in foldl (>>=) (return defaultOptions) actions

main = do
    args <- getArgs
    case args of
        subcommand:arguments -> do
            case M.lookup subcommand subprogram_options of
                Just (action, options) -> parse options arguments >>= action
                Nothing -> showSubcommands
        [] -> showSubcommands

main_dump (Options fname section _) = do
    textSection <- getSectionByName_ fname section
    V.mapM_ (putStr . flip showHex "" . fromIntegral) textSection
    putStrLn ""

main_search (Options fname section maxGadgetLen) =  do
    textSection <- getSectionByName_ fname section
    let gadgets = getGadgets maxGadgetLen textSection
    putStr . unlines $ map showAsJSON gadgets
