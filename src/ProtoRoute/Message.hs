{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module ProtoRoute.Message
    ( MsgName (..)
    , MessageField (..)
    , FieldName (..)
    , TValue (..)
    , Message (..)
    , FieldValue (..)
    , SchemaField (..)
    , MessageSchema (..)
    , constructProtoMsg) where

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MS
import Data.List (intercalate)
import Data.Text (Text)
import Data.These

newtype MsgName   = MN { unMsgName   :: String } deriving (Show, Eq, Ord)
newtype FieldName = FN { unFieldName :: String } deriving (Show, Eq, Ord)

data TValue a   = Req a | Opt (Maybe a) | Rep [a] | Error
                  deriving (Show, Eq, Ord)
data FieldValue = FText (TValue Text) | FInt (TValue Int) |
                  FMsg (TValue Message)
                  deriving (Show, Eq, Ord)

data SchemaRule = SReq | SOpt | SRep deriving (Show, Eq, Ord)
data SchemaType = SText | SInt | SMsg deriving (Show, Eq, Ord)

-- The guideline for creating a message
data MessageSchema = MessageSchema
    { msgSchemaName :: MsgName
    , msgSchemaFields :: [SchemaField]
    } deriving (Show, Eq, Ord)

-- Schema fields have no value, just <rule><type><name>
-- e.g. "required string query"
data SchemaField = SchemaField
    { schemaFieldRule :: SchemaRule
    , schemaFieldType :: SchemaType
    , schemaFieldName :: FieldName
    } deriving (Show, Eq, Ord)

-- Structure of an actual attempted message
data Message = Message
    { messageName :: MsgName
    , messageFields :: [MessageField]
    } deriving (Show, Eq, Ord)

-- Structure of a msg field, which has custom values
data MessageField = MessageField
    { messageFieldName :: FieldName
    , messageFieldValue :: FieldValue
    } deriving (Show, Eq, Ord)

-- Validation errors can be incomplete required fields, wrong fields types,
-- wrong rules or mismatching field names
data ValidationError = Incomplete String |
                       WrongName  String |
                       WrongType  String |
                       Invalid    String deriving (Show, Eq, Ord)

incomplete :: ValidationError
incomplete = Incomplete "Required field not filled"

incorrectFieldNames :: ValidationError
incorrectFieldNames = WrongName "Wrong field names"

incorrectMsgNames :: ValidationError
incorrectMsgNames = WrongName "Message names do not match"

incorrectTypes :: ValidationError
incorrectTypes = WrongType "There are mismatching types"

invalid :: ValidationError
invalid = Invalid "Your message is invalid"

-- Shows value of a message field
fromFV :: FieldValue -> String
fromFV (FText txt) = fromTV txt
fromFV (FInt num)  = fromTV num
fromFV (FMsg msg)  = fromTV msg

-- Shows value inside FieldValue constructor
fromTV :: (Show a) => TValue a -> String
fromTV (Req val)  = show val
fromTV (Opt txt)  = show txt
fromTV (Rep list) = show list
fromTV Error = "Error"

-- Checks if rules in msg match those in corresponding schema
correct :: TValue a -> SchemaRule -> Bool
correct (Req _) SReq = True
correct (Opt _) SOpt = True
correct (Rep _) SRep = True
correct _ _ = False

-- Checks if types are correct
okTypes :: FieldValue -> SchemaRule -> SchemaType -> Bool
okTypes (FText tvA) rA fTA = fTA == SText && correct tvA rA
okTypes (FInt tvB) rB fTB  = fTB == SInt && correct tvB rB
okTypes (FMsg tvC) rC fTC  = fTC == SMsg && correct tvC rC

-- Verifies if there exists a validation error
verify :: Bool -> ValidationError -> Either ValidationError ()
verify True someError = Left someError
verify False _ = Right ()

-- Validates single field, checking msg attempt against schema
validateField :: SchemaField -> MessageField -> Either ValidationError ()
validateField (SchemaField sfr sft sfn) (MessageField mfn mfv) = do
    verify (mfn /= sfn) incorrectFieldNames
    verify (not $ okTypes mfv sfr sft) incorrectTypes

validateMessageThese :: These SchemaField MessageField -> Either ValidationError ()
validateMessageThese x = case x of
    These sf mf -> validateField sf mf
    This (SchemaField sRule _ _) -> verify (sRule == SReq) incomplete
    That _ -> Left incorrectFieldNames

validateMessage :: MessageSchema -> Message -> Either ValidationError ()
validateMessage (MessageSchema sn sFields) (Message mn mFields) = do
    verify (mn /= sn) incorrectMsgNames
    mapM_ validateMessageThese (M.elems groupedFields)
  where
    groupedFields :: M.Map FieldName (These SchemaField MessageField)
    groupedFields = unionThese schemaMap msgMap

    schemaMap :: M.Map FieldName SchemaField
    schemaMap = M.fromList $ zip (map schemaFieldName sFields) sFields

    msgMap :: M.Map FieldName MessageField
    msgMap = M.fromList $ zip (map messageFieldName mFields) mFields

    makeThis :: Ord k => k -> x -> Maybe (These x y)
    makeThis _ val = Just (This val)

    makeThat :: Ord k => k -> y -> Maybe (These x y)
    makeThat _ val = Just (That val)

    makeThese :: Ord k => k -> x -> y -> Maybe (These x y)
    makeThese _ val1 val2 = Just (These val1 val2)

    unionThese :: Ord k => M.Map k a -> M.Map k b -> M.Map k (These a b)
    unionThese = MS.merge
        (MS.mapMaybeMissing makeThis)
        (MS.mapMaybeMissing makeThat)
        (MS.zipWithMaybeMatched makeThese)


-- Constructs message as a string
constructProtoMsg :: MsgName -> [(FieldName, FieldValue)] -> String
constructProtoMsg msgName namesVals =
    unMsgName msgName ++ " {" ++ addRest msgName namesVals ++ "} "
  where
    singleEntry :: MsgName -> (FieldName, FieldValue) -> String
    singleEntry name (fn, fv) =
        "_" ++ unMsgName name ++ "\'" ++ unFieldName fn ++ " = " ++ fromFV fv

    addRest :: MsgName -> [(FieldName, FieldValue)] -> String
    addRest name = intercalate ", " . map (singleEntry name)
