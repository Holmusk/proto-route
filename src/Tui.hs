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
import Data.Foldable (for_)
import Data.List ((!!), insert)
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
onMsgName = setAttr (">>>>" ++ map toUpper "Construct a message" ++ "<<<<")
    [("Enter", "Type msg name")]

setFieldName :: Widget ()
setFieldName = setAttr "Press enter to type field name"
    [("Enter", "name of new field")]
onFieldName :: Widget ()
onFieldName =
    setAttr (">>>>" ++ map toUpper "Press enter to type field name" ++ "<<<<")
    [("Enter", "Type field name")]

fromRule :: SchemaRule -> String
fromRule r = case r of
    SReq -> "required"
    SOpt -> "optional"
    SRep -> "repeated"

toRule :: String -> SchemaRule
toRule s = case s of
    "required" -> SReq
    "optional" -> SOpt
    "repeated" -> SRep
    _ -> SOpt

fromType :: SchemaType -> String
fromType t = case t of
    SText -> "string"
    SInt -> "int32"
    SMsg -> "message"

toType :: String -> SchemaType
toType s = case s of
    "string"  -> SText
    "int32"   -> SInt
    "message" -> SMsg
    _ -> SMsg

fieldToDisplay :: SchemaField -> (String, String)
fieldToDisplay (SchemaField sfr sft sfn) = (protoLine, description)
  where
    description = case sfr of
        SReq -> "Must give " ++ fromType sft ++ " here for valid msg"
        SOpt -> "Nothing or Just <yourValue>, " ++ fromType sft
        SRep -> "Empty list i.e. [], or " ++ fromType sft ++ " list"
    protoLine = unwords [fromRule sfr, fromType sft, unFieldName sfn]

showSchema :: MessageSchema -> Widget ()
showSchema (MessageSchema smn sFields) =
    setAttr ("MESSAGE NAME: " ++ unMsgName smn) $ map fieldToDisplay (init sFields)

setValue :: FieldName -> MessageSchema -> Widget ()
setValue s sch = case toFCTC sch s of
    (Right SReq, _) -> setAttr "Press enter: give value for new field"
                       [("Enter", "Type new value in string quotes")]
    (Right SOpt, _) -> setAttr "Press enter: give optional value for new field"
                       [("Enter", "Type Nothing or Just \"yourValue\" ")]
    (Right SRep, _) -> setAttr "Press enter: give repeated value for new field"
                       [("Enter", "Type list of values")]
    (Left _, _) -> onFieldName

unknownFields :: SchemaField
unknownFields = SchemaField { schemaFieldRule = SRep
                            , schemaFieldType = SText
                            , schemaFieldName = FN "_unknownFields"
                            }

msgSchema :: IO MessageSchema
msgSchema = do
    name <- parseName fp
    stuff <- parseFields fp
    return $ MessageSchema (MN name) (stuff ++ [unknownFields])
  where
    fp = "proto/protobuf2.proto"

parseName :: FilePath -> IO String
parseName fp = do
    content <- readFile fp
    return $ name $ lines content
  where
    name :: [String] -> String
    name parts = words (head parts) !! 1

parseField :: String -> SchemaField
parseField s = do
    let comps = words s
    let sfr = toRule (head comps)
    let sft = toType $ comps !! 1
    let sfn = FN $ comps !! 2
    toField sfr sft sfn
  where
    toField :: SchemaRule -> SchemaType -> FieldName -> SchemaField
    toField = SchemaField

parseFields :: FilePath -> IO [SchemaField]
parseFields fp = do
    content <- readFile fp
    let (_,fields) = splitAt 3 $ lines content
    -- toFields fields
    return $ map parseField (init fields)

toFCTC :: MessageSchema
       -> FieldName
       -> (Either String SchemaRule, Either String SchemaType)
toFCTC (MessageSchema _ []) _ = (Left "Invalid field", Left "No fields")
toFCTC sch@(MessageSchema _ (SchemaField sfr sft sfn : sFields)) fn =
    if fn == sfn
    then (Right sfr, Right sft)
    else toFCTC (sch {msgSchemaFields = sFields}) fn

processFV :: (Read a)
          => Either String SchemaRule
          -> String
          -> Either String (TValue a)
processFV rule value = case rule of
    Right SReq -> Req <$> readEither value
    Right SOpt -> Opt <$> readEither value
    Right SRep -> Rep <$> readEither value
    Left _ -> Left "Invalid value"

processFTV :: (Either String SchemaRule, Either String SchemaType)
           -> String
           -> Either String FieldValue
processFTV (rule, schType) value = case schType of
    Right SText -> FText <$> processFV rule value
    Right SInt  -> FInt  <$> processFV rule value
    Right SMsg  -> FMsg  <$> processFV rule value
    Left _ -> Left "Wrong field names"

addMsgName :: MessageSchema -> IO MsgName
addMsgName sch = do
    simpleMain $ vBox [onMsgName, setFieldName, showSchema sch]
    msgName <- getLine
    let mn = MN msgName
    return mn

addFVPair :: MessageSchema -> IO (FieldName, FieldValue)
addFVPair sch = do
    simpleMain $ vBox [setMsgName, onFieldName, showSchema sch]
    putStrLn "If you are changing a value, type the same field name as before"
    field <- getLine
    let fn = FN field
    let fc = toFCTC sch fn
    case fc of
      (Right _, Right _) -> do
          simpleMain (setValue fn sch)
          val <- getLine
          let fv = either read id (processFTV fc val)
          return (fn,fv)
      _ -> addFVPair sch


completeField :: MessageSchema -> IO (FieldName, FieldValue)
completeField sch = do
    (fn, fv) <- addFVPair sch
    putStrLn "Change this field's value? y to keep editing, any other key to continue"
    choice <- getLine
    if choice == "y"
    then do
        (fn2, fv2) <- addFVPair sch
        return (fn2, fv2)
    else return (fn, fv)

serialize :: MsgName
          -> [(FieldName, FieldValue)]
          -> [MessageField]
          -> MessageSchema
          -> IO ()
serialize mn namesVals fields sch = do
    let stringMsg = constructProtoMsg mn namesVals
    let msg = Message mn fields
    let validationWrong = validateMessage sch msg
    if validationWrong == Right ()
    then runGhci stringMsg
    else print validationWrong

replace :: Int -> a -> [a] -> [a]
replace n arg list = x ++ arg : ys
  where
    (x,_:ys) = splitAt n list

changeFieldsOrNot :: MsgName
                  -> [(FieldName, FieldValue)]
                  -> [MessageField]
                  -> MessageSchema
                  -> IO ()
changeFieldsOrNot mn namesVals fields schema = do
    putStrLn "Change any field's value? y for yes, s to serialize, any other key to quit"
    choice <- getLine
    if choice == "y"
    then do
      (newFN, newFV) <- completeField schema
      if | newFN == messageFieldName (head fields) ->
           let (newNamesVals, newFields) = doubleReplace 0 (newFN, newFV) namesVals fields
           in changeFieldsOrNot mn newNamesVals newFields schema
         | newFN == messageFieldName (fields!!1) ->
           let (newNamesVals, newFields) = doubleReplace 1 (newFN, newFV) namesVals fields
           in changeFieldsOrNot mn newNamesVals newFields schema
         | otherwise ->
           let (newNamesVals, newFields) = doubleReplace 2 (newFN, newFV) namesVals fields
           in changeFieldsOrNot mn newNamesVals newFields schema
    else if choice == "s"
    then useTwo serialize changeFieldsOrNot mn namesVals fields schema
    else putStrLn "Ok, see final serialisation above."

doubleReplace :: Int
              -> (FieldName, FieldValue)
              -> [(FieldName, FieldValue)]
              -> [MessageField]
              -> ([(FieldName, FieldValue)], [MessageField])
doubleReplace n (newFN, newFV) namesVals fields =
    ( replace n (newFN, newFV) namesVals
    , replace n (MessageField newFN newFV) fields)

useTwo :: (a -> b -> c -> d -> IO ())
       -> (a -> b -> c -> d -> IO ())
       -> a -> b -> c -> d -> IO ()
useTwo f g a b c d = do
    f a b c d
    g a b c d

getNameVals :: MessageSchema -> IO [(FieldName, FieldValue)]
getNameVals sch = do
    (fn, fv) <- completeField sch
    moreInputs <- getNameVals sch
    return ((fn, fv) : moreInputs)

toMF :: (FieldName, FieldValue) -> MessageField
toMF (fn, fv) = MessageField fn fv

main :: IO ()
main = do
    someSchema <- msgSchema
    mn <- addMsgName someSchema
    schFields <- parseFields "proto/protobuf2.proto"
    namesVals <- getNameVals someSchema
    let msgFields = map toMF namesVals
    let namesVals' = namesVals ++ [(FN "_unknownFields", FText (Rep []))]
    serialize mn namesVals' msgFields someSchema
    changeFieldsOrNot mn namesVals' msgFields someSchema
