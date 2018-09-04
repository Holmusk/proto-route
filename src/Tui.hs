{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tui
    ( setAttr
    , setFieldName
    , setMsgName
    , setValue
    , processFTV
    , completeField
    , addFVPair
    , main
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Char (toUpper)
import Text.Read (readEither)
import ProtoRoute.Ghcid (runGhci)
import ProtoRoute.Message ( MsgName (..)
                          , MessageField (..)
                          , FieldName (..)
                          , TValue (..)
                          , Message (..)
                          , FieldValue (..)
                          , SchemaField (..)
                          , MessageSchema (..)
                          , constructProtoMsg
                          , SchemaRule (..)
                          , SchemaType (..)
                          , validateMessage)

drawKey :: String -> String -> Widget ()
drawKey act key = padRight Max (padLeft (Pad 1) $ str act) <+>
                  padLeft Max (padRight (Pad 1) $ str key)

setAttr :: String -> [(String, String)] -> Widget ()
setAttr s1 opts = withBorderStyle unicodeBold
    $ hCenter
    $ hLimit 80
    $ vLimit 400
    $ borderWithLabel (str s1)
    $ vBox
    $ map (uncurry drawKey) opts

setMsgName :: Widget ()
setMsgName = setAttr "Construct a message" [("Enter", "Type msg name")]
onMsgName :: Widget ()
onMsgName = setAttr ("**" ++ map toUpper "Construct a message" ++ "**")
    [("Enter", "Type msg name")]

setFieldName :: Widget ()
setFieldName = setAttr "Press enter to type field name"
    [("Enter", "name of new field")]
onFieldName :: Widget ()
onFieldName =
    setAttr (">>>>" ++ map toUpper "Press enter to type field name" ++ "<<<<")
    [("Enter", "Type msg name")]

showSchema :: Widget ()
showSchema = setAttr "MESSAGE NAME: SearchRequest"
    [ ("required string query", "Must give string here for valid message")
    , ("optional int maybeNum", "Nothing or Just <yourValue>")
    , ("repeated int numList" , "Empty list or list of integers")
    ]

setValue :: String -> Widget ()
setValue s = case toFC s of
  FReq  -> setAttr "Press enter key to give value for this new field"
         [("Enter", "Type new value in string quotes")]
  FOpt  -> setAttr "Press enter to give optional value for this new field"
         [("Enter", "Type Nothing or Just \"yourValue\" ")]
  FRep  -> setAttr "Press enter to give repeated value for this new field"
        [("Enter", "Type list of values")]
  Wrong -> onFieldName

msgSchema :: MessageSchema
msgSchema = MessageSchema { msgSchemaName = MN "SearchRequest"
                          , msgSchemaFields = [
                            SchemaField
                            { schemaFieldRule = SReq
                            , schemaFieldType = SText
                            , schemaFieldName = FN "query"
                            }
                          , SchemaField
                            { schemaFieldRule = SOpt
                            , schemaFieldType = SInt
                            , schemaFieldName = FN "maybeNum"
                            }
                          , SchemaField
                            { schemaFieldRule = SRep
                            , schemaFieldType = SInt
                            , schemaFieldName = FN "numList"
                            }
                          , SchemaField
                            { schemaFieldRule = SRep
                            , schemaFieldType = SText
                            , schemaFieldName = FN "_unknownFields"
                            }]
                          }

data FieldChoice = FReq | FOpt | FRep | Wrong deriving (Show, Read)

toFC :: String -> FieldChoice
toFC s = case s of
    "query"    -> FReq
    "maybeNum" -> FOpt
    "numList"  -> FRep
    _          -> Wrong

processFTV :: FieldChoice -> String -> Either String FieldValue
processFTV field value = case field of
    FReq -> FText . Req <$> readEither value
    FOpt -> FInt . Opt <$> readEither value
    FRep  -> FInt . Rep <$> readEither value
    Wrong -> readEither "Wrong field names"

addMsgName :: IO MsgName
addMsgName = do
    simpleMain $ vBox [onMsgName, setFieldName, showSchema]
    msgName <- getLine
    let mn = MN msgName
    return mn

addFVPair :: IO (FieldName, FieldValue)
addFVPair = do
    simpleMain $ vBox [setMsgName, onFieldName, showSchema]
    putStrLn "If you are changing a value, type the same field name as before"
    field <- getLine
    let fn = FN field
    simpleMain (setValue field)
    val <- getLine
    let fc = toFC field
    let fv = either read id (processFTV fc val)
    return (fn,fv)

completeField :: IO (FieldName, FieldValue)
completeField = do
    (fn, fv) <- addFVPair
    putStrLn "Change field value? y to keep editing, any other key to continue"
    choice <- getLine
    if choice == "y"
    then do
        (fn2, fv2) <- addFVPair
        return (fn2, fv2)
    else return (fn, fv)

finishOff :: MsgName -> [(FieldName, FieldValue)] -> [MessageField] -> IO ()
finishOff mn namesVals fields = do
    let stringMsg = constructProtoMsg mn namesVals
    let msg = Message {messageName = mn, messageFields = fields}
    let validationWrong = validateMessage msgSchema msg
    if validationWrong == Right ()
    then runGhci stringMsg
    else print validationWrong

main :: IO ()
main = do
    mn <- addMsgName
    (fnA, fvA) <- completeField
    let field1 = MessageField {messageFieldName = fnA, messageFieldValue = fvA}
    (fnB, fvB) <- completeField
    let field2 = MessageField {messageFieldName = fnB, messageFieldValue = fvB}
    (fnC, fvC) <- completeField
    let field3 = MessageField {messageFieldName = fnC, messageFieldValue = fvC}
    let namesVals = [(fnA, fvA)
                   , (fnB, fvB)
                   , (fnC, fvC)
                   , (FN "_unknownFields", FText (Rep []))]
    let fields = [field1, field2, field3]
    finishOff mn namesVals fields
