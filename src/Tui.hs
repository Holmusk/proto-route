{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

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
import Data.List (intercalate)
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
    [("Enter", "Type field name")]

fieldToDisplay :: SchemaField -> (String, String)
fieldToDisplay (SchemaField sfr sft sfn) = (protoLine, description)
  where
    description = case sfr of
        SReq -> "Must give " ++ fromType sft ++ " here for valid msg"
        SOpt -> "Nothing or Just <yourValue>, must be of type " ++ fromType sft
        SRep -> "Empty list i.e. [], or list of type " ++ fromType sft
    protoLine = intercalate " " [fromRule sfr, fromType sft, unFieldName sfn]

    fromRule :: SchemaRule -> String
    fromRule r = case r of
        SReq -> "required"
        SOpt -> "optional"
        SRep -> "repeated"

    fromType :: SchemaType -> String
    fromType t = case t of
        SText -> "string"
        SInt -> "int"
        SMsg -> "message"

showSchema :: MessageSchema -> Widget ()
showSchema (MessageSchema smn sFields) =
    setAttr ("MESSAGE NAME: " ++ unMsgName smn) $ map fieldToDisplay (init sFields)

setValue :: FieldName -> Widget ()
setValue s = case toFC msgSchema s of
    Right SReq -> setAttr "Press enter: give value for new field"
           [("Enter", "Type new value in string quotes")]
    Right SOpt -> setAttr "Press enter: give optional value for new field"
           [("Enter", "Type Nothing or Just \"yourValue\" ")]
    Right SRep -> setAttr "Press enter: give repeated value for new field"
           [("Enter", "Type list of values")]
    Left _ -> onFieldName

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

toFC :: MessageSchema -> FieldName -> Either String SchemaRule
toFC (MessageSchema _ []) _ = Left "Invalid field"
toFC sch@(MessageSchema _ (SchemaField sfr _ sfn : sFields)) fn =
    if fn == sfn
    then Right sfr
    else toFC (sch {msgSchemaFields = sFields}) fn

processFTV :: Either String SchemaRule -> String -> Either String FieldValue
processFTV field value = case field of
    Right SReq  -> FText . Req <$> readEither value
    Right SOpt  -> FInt  . Opt <$> readEither value
    Right SRep  -> FInt  . Rep <$> readEither value
    Left _ -> Left "Wrong field names"

addMsgName :: IO MsgName
addMsgName = do
    simpleMain $ vBox [onMsgName, setFieldName, showSchema msgSchema]
    msgName <- getLine
    let mn = MN msgName
    return mn

addFVPair :: IO (FieldName, FieldValue)
addFVPair = do
    simpleMain $ vBox [setMsgName, onFieldName, showSchema msgSchema]
    putStrLn "If you are changing a value, type the same field name as before"
    field <- getLine
    let fn = FN field
    let fc = toFC msgSchema fn
    case fc of
      Right _ -> do
          simpleMain (setValue fn)
          val <- getLine
          let fv = either read id (processFTV fc val)
          return (fn,fv)
      Left _ -> addFVPair


completeField :: IO (FieldName, FieldValue)
completeField = do
    (fn, fv) <- addFVPair
    putStrLn "Change this field's value? y to keep editing, any other key to continue"
    choice <- getLine
    if choice == "y"
    then do
        (fn2, fv2) <- addFVPair
        return (fn2, fv2)
    else return (fn, fv)

serialize :: MsgName -> [(FieldName, FieldValue)] -> [MessageField] -> IO ()
serialize mn namesVals fields = do
    let stringMsg = constructProtoMsg mn namesVals
    let msg = Message {messageName = mn, messageFields = fields}
    let validationWrong = validateMessage msgSchema msg
    if validationWrong == Right ()
    then runGhci stringMsg
    else print validationWrong

replace :: Int -> a -> [a] -> [a]
replace n arg list = x ++ arg : ys
  where
    (x,_:ys) = splitAt n list

main :: IO ()
main = do
    mn <- addMsgName
    (fnA, fvA) <- completeField
    let field1 = (MessageField fnA fvA)
    (fnB, fvB) <- completeField
    let field2 = (MessageField fnB fvB)
    (fnC, fvC) <- completeField
    let field3 = (MessageField fnC fvC)
    let namesVals = [(fnA, fvA)
                   , (fnB, fvB)
                   , (fnC, fvC)
                   , (FN "_unknownFields", FText (Rep []))]
    let fields = [field1, field2, field3]
    serialize mn namesVals fields
    changeFieldsOrNot mn namesVals fields
  where
    changeFieldsOrNot :: MsgName
                      -> [(FieldName, FieldValue)]
                      -> [MessageField]
                      -> IO ()
    changeFieldsOrNot mn namesVals fields = do
        putStrLn "Change any field's value? y for yes, s to serialize, any other key to quit"
        choice <- getLine
        if choice == "y"
        then do
          (newFN, newFV) <- completeField
          if | newFN == messageFieldName (fields!!0) ->
               let (newNamesVals, newFields) = doubleReplace 0 (newFN, newFV) namesVals fields
               in changeFieldsOrNot mn newNamesVals newFields
             | newFN == messageFieldName (fields!!1) ->
               let (newNamesVals, newFields) = doubleReplace 1 (newFN, newFV) namesVals fields
               in changeFieldsOrNot mn newNamesVals newFields
             | otherwise ->
               let (newNamesVals, newFields) = doubleReplace 2 (newFN, newFV) namesVals fields
               in changeFieldsOrNot mn newNamesVals newFields
        else if choice == "s"
        then useTwo serialize changeFieldsOrNot mn namesVals fields
        else putStrLn "Ok, see final serialisation above."

    doubleReplace :: Int
                  -> (FieldName, FieldValue)
                  -> [(FieldName, FieldValue)]
                  -> [MessageField]
                  -> ([(FieldName, FieldValue)], [MessageField])
    doubleReplace n (newFN, newFV) namesVals fields =
        ( replace n (newFN, newFV) namesVals
        , replace n (MessageField newFN newFV) fields)

    useTwo :: (a -> b -> c -> IO ())
           -> (a -> b -> c -> IO ())
           -> a -> b -> c -> IO ()
    useTwo f g a b c = do
        f a b c
        g a b c
