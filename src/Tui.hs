{-# LANGUAGE OverloadedStrings #-}
module Tui
    ( setAttr
    , setFieldName
    , setMsgName
    , setFieldType
    , setValue
    , processFTV
    , main
    ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.Char (toLower, toUpper)
import ProtoRoute.Ghcid (runGhci)
import ProtoRoute.Message (MsgName (..)
                         , FieldName (..)
                         , TValue (..)
                         , FieldValue (..)
                         , constructProtoMsg)

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

setFieldType :: Widget ()
setFieldType = setAttr "Press enter then select new message field type"
    [("r", "required"), ("o", "optional"), ("m", "repeated")]
onFieldType :: Widget ()
onFieldType =
    setAttr (">>>>" ++ map toUpper "Press enter then select field type" ++ "<<<<")
    [("r", "required"), ("o", "optional"), ("m", "repeated")]

setValue :: String -> Widget ()
setValue s = case s of
  "r" -> setAttr "Press enter key to give value for this new field"
         [("Enter", "Type new value in string quotes")]
  "o" -> setAttr "Press enter key to give optional value for this new field"
         [("Enter", "Type Nothing or Just \"yourValue\" ")]
  "m" -> setAttr "Press enter key to give repeated value for this new field"
        [("Enter", "Type string values in a string list")]
  _ -> setFieldType

processFTV :: (Read a) => String -> String -> TValue a
processFTV choice value = case choice of
    "r" -> Req $ read value
    "o" -> Opt $ read value
    "m" -> Rep $ read value
    _   -> Error

addFieldName :: IO FieldName
addFieldName = do
    simpleMain $ vBox [setMsgName, onFieldName, setFieldType]
    field <- getLine
    let fn = FN (map toLower field)
    return fn

addFVPair :: IO (FieldName, FieldValue)
addFVPair = do
    fn <- addFieldName
    simpleMain $ vBox [setMsgName, setFieldName, onFieldType]
    t <- getLine
    simpleMain (setValue t)
    val <- getLine
    let fv = FText (processFTV t val)
    return (fn,fv)

changeValue :: IO FieldValue
changeValue = do
    simpleMain setFieldType
    t <- getLine
    simpleMain (setValue t)
    val <- getLine
    let fv = FText (processFTV t val)
    return fv

finishOff :: MsgName ->[(FieldName, FieldValue)] -> IO ()
finishOff mn namesVals = do
    let msg = constructProtoMsg mn namesVals
    putStrLn msg
    runGhci msg

main :: IO ()
main = do
    simpleMain $ vBox [onMsgName, setFieldName, setFieldType]
    msgName <- getLine
    let mn = MN msgName

    (fn, fv) <- addFVPair
    putStrLn "Change field value? Type y/n to keep editing/stop"
    choice <- getLine
    if choice == "y"
    then do
        fv2 <- changeValue
        finishOff mn [(fn, fv2), (FN "_unknownFields", FText (Rep []))]
    else do
        finishOff mn [(fn, fv),  (FN "_unknownFields", FText (Rep []))]
