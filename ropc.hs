{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction #-}
import Control.Arrow
import Control.Monad.Identity
import Data.Function
import Data.List
import Data.Maybe
import Data.Word
import Foreign
import Foreign.C
import Hdis86
import Numeric
import ROPCUtils
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

getGadgets backLength codeVec asmSyntax = nub . validate . concat $ map tryPosition retIdxs where
    isBranchy = (`elem` [Iret, Ijmp, Icall, Iint, Isyscall, Ijo, Ijno, Ijb, Ijae, Ijz, Ijnz, Ijbe, Ija, Ijs, Ijns, Ijp, Ijnp, Ijl, Ijge, Ijle, Ijg])
    retIdxs = V.ifoldr (\i x -> if isJumpStart i x then (i:) else id) [] codeVec
    isJumpStart i x = any (\j -> maybe False id . fmap (isBranchy . inOpcode . mdInst . head) $ trySlice i j) [1..8]
    len = V.length codeVec
    invalidSlice i j = (i+j) > len || i < 0
    maybeSlice i j | invalidSlice i j = Nothing
    maybeSlice i j = Just $ V.slice i j codeVec
    trySlice i j = maybeSlice i j >>= (Just . disassembleMetadata (intel32 {cfgSyntax = asmSyntax}) . L.toStrict . L.pack . V.toList)
    tryPosition j = catMaybes $ map (\i -> fmap ((,)(j-i)) $ trySlice (j-i) (i+1)) [0..backLength]
    isValid instrs = (all (not . (== Iinvalid) . inOpcode) instrs) && (isBranchy . inOpcode $ last instrs)
    validate = filter (isValid . map mdInst . snd)

data OutputStyle = JSONOutput | ASMWithHexOutput | ROPGadgetOutput

data Options = Options {
    optFilename :: String,
    optSection :: String,
    optGadgetLength :: Int,
    optAssemblySyntax :: Syntax,
    optOutputStyle :: OutputStyle
    }

defaultOptions = Options "a.out" ".text" 5 SyntaxATT ROPGadgetOutput

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
    ("search", (main_search, fix $ \x -> [filename, section, gadgetLength, asmSyntax, outputStyle, help "search" x]))
    ] where
    filename = Option "f" ["file"] (ReqArg (\arg opt -> return opt { optFilename = arg }) "FILENAME") "Name of executable"
    section = Option "s" ["section"] (ReqArg (\arg opt -> return opt { optSection = arg }) "SECTION") "Name of section"
    gadgetLength = Option "l" ["length"] (ReqArg (\arg opt -> return opt { optGadgetLength = read arg }) "LENGTH") "Maximum length of gadgets to search for"
    asmSyntax = Option "" ["syntax"] (ReqArg (\arg opt -> return opt { optAssemblySyntax = case arg of {
            "intel" -> SyntaxIntel; "att" -> SyntaxATT;
            other -> error $ concat ["Unrecognized syntax option \"", other, "\" (valid options: \"att\", \"intel\")."]
        }}) "[intel|att]") "Which syntax to use for displaying assembly"
    outputStyle = Option "" ["output-style"] (ReqArg (\arg opt -> return opt { optOutputStyle = case arg of {
            "json" -> JSONOutput; "asmwithhex" -> ASMWithHexOutput; "ropgadget" -> ROPGadgetOutput;
            other -> error $ concat ["Unrecognized output style option \"", other, "\" (valid options: \"json\", \"asmwithhex\", \"ropgadget\")."]
        }}) "[json|asmwithhex|ropgadget]") "Which style of output to use"
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

main_dump (Options fname section _ _ _) = do
    textSection <- getSectionByName_ fname section
    V.mapM_ (putStr . flip showHex "" . fromIntegral) textSection
    putStrLn ""

main_search (Options fname section maxGadgetLen asmSyntax outputStyle) = do
    textSection <- getSectionByName_ fname section
    let gadgets = getGadgets maxGadgetLen textSection asmSyntax
    putStr . unlines $ map (case outputStyle of {
        JSONOutput -> (showAsJSON . (id *** map mdInst));
        ASMWithHexOutput -> (show . (id *** (map mdAssembly &&& map mdHex)));
        ROPGadgetOutput -> (\(i, x) -> ("0x"++) . showHex i . (": "++) . intercalate "; " $ map mdAssembly x)
    }) gadgets
