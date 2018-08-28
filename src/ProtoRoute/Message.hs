{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module ProtoRoute.Message
    ( MsgName (..)
    , Field (..)
    , FieldName (..)
    , TValue (..)
    , Message (..)
    , FieldValue (..)
    , constructProtoMsg) where

import Data.List (intercalate)
import Data.Text (Text)

newtype MsgName   = MN { unMsgName   :: String } deriving newtype (Show)
newtype FieldName = FN { unFieldName :: String } deriving newtype (Show)

data TValue a   = Req a | Opt (Maybe a) | Rep [a] | Error deriving (Show)
data FieldValue = FText (TValue Text) | FInt (TValue Int) |
                  FMsg (TValue Message) deriving (Show)
data Message = Message
    { messageName :: MsgName
    , messageFields :: [(FieldName, FieldValue)]
    } deriving (Show)

data Field = Field
    { fieldName :: FieldName
    , fieldValue :: FieldValue
    } deriving (Show)

fromFV :: FieldValue -> String
fromFV (FText txt) = fromTV txt
fromFV (FInt num)  = fromTV num
fromFV (FMsg msg)  = fromTV msg

fromTV :: (Show a) => TValue a -> String
fromTV (Req val)  = show val
fromTV (Opt txt)  = show txt
fromTV (Rep list) = show list
fromTV Error = "Error"

constructProtoMsg :: MsgName -> [(FieldName, FieldValue)] -> String
constructProtoMsg msgName namesVals =
    unMsgName msgName ++ " {" ++ addRest msgName namesVals ++ "} "
  where
    singleEntry :: MsgName -> (FieldName, FieldValue) -> String
    singleEntry name (fn, fv) =
        "_" ++ unMsgName name ++ "\'" ++ unFieldName fn ++ " = " ++ fromFV fv

    addRest :: MsgName -> [(FieldName, FieldValue)] -> String
    addRest name = intercalate ", " . map (singleEntry name)
