{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, OverloadedStrings #-}
import Control.Applicative ((<$),(<$>),(<*),(<*>),(*>))
import Control.Monad
import Data.Aeson
import Data.Char
import System.Environment
import Text.Parsec
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as T
import qualified Data.Vector as V

any_of predicates x = any ($ x) predicates
vMapMaybe f = V.foldl (\a e -> maybe a (a `V.snoc`) (f e)) V.empty

data AccessPathElement = DotNotation T.Text | BracketSubscript Int | Wildcard deriving Show

derefByPath ((DotNotation property):xs) (Object obj) = (H.lookup property obj) >>= (derefByPath xs)
derefByPath ((BracketSubscript idx):xs) (Array arr) = (arr V.!? idx) >>= (derefByPath xs)
derefByPath (Wildcard:xs) (Array arr) = Just . Array $ vMapMaybe (derefByPath xs) arr
derefByPath [] x = Just x
derefByPath _ _ = Nothing

-- http://stackoverflow.com/questions/1661197/valid-characters-for-javascript-variable-names
js_id_start = any_of [(`elem` [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter, LetterNumber]) . generalCategory, (`elem` "$_")]
js_id_rest = any_of [js_id_start, (`elem` [NonSpacingMark, SpacingCombiningMark, DecimalNumber, ConnectorPunctuation]) . generalCategory]
parseJSIdentifier = ((:) <$> satisfy js_id_start <*> many (satisfy js_id_rest)) <?> "javascriptIdentifier"

parseDotNotation = char '.' *> ((DotNotation . T.pack <$> parseJSIdentifier) <|> (Wildcard <$ char '*'))
parseSubscript = BracketSubscript . read <$> (char '[' *> many1 (satisfy isDigit) <* char ']')

parseAccessPath = parse ((many1 (parseDotNotation <|> parseSubscript)) <|> (eof *> return [])) ""

showHelp progName =
    putStr $ unlines [concat ["Usage: ", progName, " PATHSPEC"],
        "PATHSPEC = '.' javascriptIdentifer | '[' number ']' | '.*'",
        "(e.g. \".foo[1]\", \".bar\", \"[13]\", \"[42].__testing__\", \"[0].*.foo\")"]

main = getArgs >>= \case
    [path] -> either print main' $ parseAccessPath path
    _ -> getProgName >>= showHelp

interactBytestringLines f = interact (unlines . map (L.unpack . f . L.pack) . lines)
main' path = interactBytestringLines $ maybe "null" (encode . derefByPath path) . decode
