{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
module ROPCUtils where
import Data.Aeson
import Data.Char
import Data.List
import Hdis86
import Numeric
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Lazy as L

eitherToMaybe = either (const Nothing) Just

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
