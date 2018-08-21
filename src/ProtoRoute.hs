module ProtoRoute
       ( main
       ) where

import Data.Text (pack)
import ProtoRoute.Ghcid (runGhci)
import ProtoRoute.Message (MsgName (..), FieldName (..), TValue (..),
                           FieldValue (..), constructProtoMsg)

main :: IO ()
main = runGhci $ constructProtoMsg msg [(field1, value1)
                                      , (field2, value2)
                                      , (unknown, none)]
  where
    msg = MN "SearchRequest"
    field1 = FN "query"
    value1 = FText (Req $ pack "someText")
    field2 = FN "browser"
    value2 = FText (Opt (Just $ pack "Safari"))
    unknown = FN "_unknownFields"
    none = FText (Rep [])
