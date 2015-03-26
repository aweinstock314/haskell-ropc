{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
import Control.Applicative ((<$>),(<*),(<*>),(*>))
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Vector ((!?))
import System.Environment
import Text.Parsec
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import qualified Data.Vector as V

data AccessPathElement = DotNotation T.Text | BracketSubscript Int deriving Show

derefBy (DotNotation property) (Object obj) = H.lookup property obj
derefBy (BracketSubscript idx) (Array arr) = arr !? idx
derefBy _ _ = Nothing

derefByPath = flip (foldM (flip derefBy))

any_of predicates x = any ($ x) predicates

-- http://stackoverflow.com/questions/1661197/valid-characters-for-javascript-variable-names
js_id_start = any_of [(`elem` [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter, LetterNumber]) . generalCategory, (`elem` "$_")]
js_id_rest = any_of [js_id_start, (`elem` [NonSpacingMark, SpacingCombiningMark, DecimalNumber, ConnectorPunctuation]) . generalCategory]

parseJSIdentifier = (:) <$> satisfy js_id_start <*> many (satisfy js_id_rest)

parseDotNotation = DotNotation . T.pack <$> (char '.' *> parseJSIdentifier)
parseSubscript = BracketSubscript . read <$> (char '[' *> many1 (satisfy isDigit) <* char ']')

parseAccessPath = parse ((many1 (parseDotNotation <|> parseSubscript)) <|> (eof *> return [])) ""

main = do
    args <- getArgs
    case args of
        [path] -> case parseAccessPath path of
            Left err -> print err
            Right path' -> main' path'
        _ -> do
            prog <- getProgName
            putStr $ unlines [concat ["Usage: ", prog, " PATHSPEC"], "PATHSPEC = '.' javascriptIdentifer | '[' number ']'",
                "(e.g. \".foo[1]\", \".bar\", \"[13]\", \"[42].__testing__\")"]

main' path = interact (unlines . map (L.unpack . maybe "null" (encode . derefByPath path) . decode . L.pack) . lines)
