module Aurora.Pretty where

import Data.Text (Text, unpack)
import Text.PrettyPrint.Leijen hiding (text)

import Aurora.Optional (Optional(..))

recordDoc :: Text -> [(Text, Optional Doc)] -> Doc
recordDoc recordName pairs =
    label recordName <> tupled' (concatMap format pairs)
  where
    format (fieldName, mValue) = case mValue of
        Default        -> []
        Specific value -> [label fieldName <+> equals <+> value]

    tupled' ds = tupled (map (space <>) ds)

label :: Text -> Doc
label txt = string (unpack txt)

text :: Text -> Doc
text txt = dquotes (string (unpack txt))

list' :: (a -> Doc) -> [a] -> Doc
list' f = list . map ((space <>) . f)
