module Marlowe.Holes where

import Prelude

import Data.Array (foldMap, foldl, mapWithIndex, (:))
import Data.BigInteger (BigInteger)
import Data.Foldable (intercalate)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String (length)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Marlowe.Pretty (class Pretty, genericPretty, prettyFragment)
import Marlowe.Semantics (ChosenNum, Money, Party, PubKey, Slot, Timeout)
import Marlowe.Semantics as S
import Text.Parsing.Parser.Basic (replaceInPosition)
import Text.Parsing.Parser.Pos (Position(..))
import Text.PrettyPrint.Leijen (appendWithSoftbreak)
import Text.PrettyPrint.Leijen as Leijen
import Type.Proxy (Proxy)

data MarloweType
  = StringType
  | BigIntegerType
  | SlotType
  | AccountIdType
  | ChoiceIdType
  | ValueIdType
  | ActionType
  | PayeeType
  | CaseType
  | ValueType
  | InputType
  | ObservationType
  | ContractType
  | BoundType

derive instance eqMarloweType :: Eq MarloweType

data Argument
  = ArrayArg String
  | DataArg String
  | NewtypeArg

getMarloweConstructors :: MarloweType -> Map String (Array Argument)
getMarloweConstructors StringType = Map.singleton "String" [ NewtypeArg ]

getMarloweConstructors BigIntegerType = Map.singleton "Integer" [ NewtypeArg ]

getMarloweConstructors SlotType = Map.singleton "Integer" [ NewtypeArg ]

getMarloweConstructors AccountIdType = Map.singleton "AccountId" [ DataArg "accNumber", DataArg "accHolder" ]

getMarloweConstructors ChoiceIdType = Map.singleton "ChoiceId" [ DataArg "choiceNumber", DataArg "choiceOwner" ]

getMarloweConstructors ValueIdType = Map.singleton "ValueId" [ NewtypeArg ]

getMarloweConstructors ActionType =
  Map.fromFoldable
    [ (Tuple "Deposit" [ DataArg "accountId", DataArg "party", DataArg "value" ])
    , (Tuple "Choice" [ DataArg "choiceId", ArrayArg "bounds" ])
    , (Tuple "Notify" [ DataArg "observation" ])
    ]

getMarloweConstructors PayeeType =
  Map.fromFoldable
    [ (Tuple "Account" [ DataArg "accountId" ])
    , (Tuple "Party" [ DataArg "party" ])
    ]

getMarloweConstructors CaseType = Map.singleton "Case" [ DataArg "action", DataArg "contract" ]

getMarloweConstructors ValueType =
  Map.fromFoldable
    [ (Tuple "AvailableMoney" [ DataArg "accountId" ])
    , (Tuple "Constant" [ DataArg "amount" ])
    , (Tuple "NegValue" [ DataArg "value" ])
    , (Tuple "AddValue" [ DataArg "value", DataArg "value" ])
    , (Tuple "SubValue" [ DataArg "value", DataArg "value" ])
    , (Tuple "ChoiceValue" [ DataArg "choiceId", DataArg "value" ])
    , (Tuple "SlotIntervalStart" [])
    , (Tuple "SlotIntervalEnd" [])
    , (Tuple "UseValue" [ DataArg "valueId" ])
    ]

getMarloweConstructors InputType =
  Map.fromFoldable
    [ (Tuple "IDeposit" [ DataArg "accountId", DataArg "party", DataArg "money" ])
    , (Tuple "IChoice" [ DataArg "choiceId", DataArg "choiceNum" ])
    , (Tuple "INotify" [])
    ]

getMarloweConstructors ObservationType =
  Map.fromFoldable
    [ (Tuple "AndObs" [ DataArg "observation", DataArg "observation" ])
    , (Tuple "OrObs" [ DataArg "observation", DataArg "observation" ])
    , (Tuple "NotObs" [ DataArg "observation" ])
    , (Tuple "ChoseSomething" [ DataArg "choiceId" ])
    , (Tuple "ValueGE" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueGT" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueLE" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueLT" [ DataArg "value", DataArg "value" ])
    , (Tuple "ValueEQ" [ DataArg "value", DataArg "value" ])
    , (Tuple "TrueObs" [])
    , (Tuple "FalseObs" [])
    ]

getMarloweConstructors ContractType =
  Map.fromFoldable
    [ (Tuple "Close" [])
    , (Tuple "Pay" [ DataArg "accountId", DataArg "payee", DataArg "value", DataArg "contract" ])
    , (Tuple "If" [ DataArg "observation", DataArg "contract", DataArg "contract" ])
    , (Tuple "When" [ ArrayArg "case", DataArg "timeout", DataArg "contract" ])
    , (Tuple "Let" [ DataArg "valueId", DataArg "value", DataArg "contract" ])
    ]

getMarloweConstructors BoundType = Map.singleton "Bound" [ DataArg "from", DataArg "to" ]

constructMarloweType :: String -> MarloweHole -> Map String (Array Argument) -> String
constructMarloweType constructorName (MarloweHole { name, marloweType, start }) m = case Map.lookup constructorName m of
  Nothing -> ""
  Just [] -> constructorName
  Just vs ->
    let
      Position { line, column } = start
    in
      "(" <> constructorName <> " " <> intercalate " " (mapWithIndex (showArgument line column) vs) <> ")"
  where
  showArgument line column i (ArrayArg arg) = "[ ?" <> arg <> "_" <> show line <> "_" <> show (column + i) <> " ]"

  showArgument line column i (DataArg arg) = "?" <> arg <> "_" <> show line <> "_" <> show (column + i)

  showArgument _ _ _ NewtypeArg = ""

derive instance genericMarloweType :: Generic MarloweType _

instance showMarloweType :: Show MarloweType where
  show = genericShow

data Term a
  = Term a
  | Hole String (Proxy a) Position Position
  | Const String (Proxy a) Position Position

derive instance genericTerm :: Generic (Term a) _

instance eqTerm :: Eq a => Eq (Term a) where
  eq a b = genericEq a b

instance showTerm :: Show a => Show (Term a) where
  show (Term a) = show a
  show (Hole name _ _ _) = "?" <> name
  show (Const name _ _ _) = name

instance prettyTerm :: Pretty a => Pretty (Term a) where
  prettyFragment (Term a) = prettyFragment a
  prettyFragment (Hole name _ _ _) = Leijen.text $ "?" <> name
  prettyFragment (Const name _ _ _) = Leijen.text name

-- a concrete type for holes only
data MarloweHole
  = MarloweHole
    { name :: String
    , marloweType :: MarloweType
    , start :: Position
    , end :: Position
    }

derive instance eqMarloweHole :: Eq MarloweHole

instance ordMarloweHole :: Ord MarloweHole where
  compare (MarloweHole { start: (Position a) }) (MarloweHole { start: (Position b) }) = (compare `on` _.line <> compare `on` _.column) a b

-- a concrete type for constants only
data MarloweConstant
  = MarloweConstant
    { name :: String
    , marloweType :: MarloweType
    , start :: Position
    , end :: Position
    }

derive instance eqMarloweConstant :: Eq MarloweConstant

instance ordMarloweConstant :: Ord MarloweConstant where
  compare (MarloweConstant { start: (Position a) }) (MarloweConstant { start: (Position b) }) = (compare `on` _.line <> compare `on` _.column) a b

class IsMarloweType a where
  marloweType :: Proxy a -> MarloweType

instance stringIsMarloweType :: IsMarloweType String where
  marloweType _ = StringType

instance bigIntegerIsMarloweType :: IsMarloweType BigInteger where
  marloweType _ = BigIntegerType

-- a Monoid for collecting Holes
newtype Holes
  = Holes (Map String (Array MarloweHole))

derive instance newtypeHoles :: Newtype Holes _

instance semigroupHoles :: Semigroup Holes where
  append (Holes a) (Holes b) = Holes (Map.unionWith append a b)

instance monoidHoles :: Monoid Holes where
  mempty = Holes mempty

-- a Monoid for collecting Constants
newtype Constants
  = Constants (Map String (Array MarloweConstant))

derive instance newtypeConstants :: Newtype Constants _

instance semigroupConstants :: Semigroup Constants where
  append (Constants a) (Constants b) = Constants (Map.unionWith append a b)

instance monoidConstants :: Monoid Constants where
  mempty = Constants mempty

insertHole :: forall a. IsMarloweType a => Holes -> Term a -> Holes
insertHole (Holes m) (Hole name proxy start end) = Holes $ Map.alter f name m
  where
  marloweHole = MarloweHole { name, marloweType: (marloweType proxy), start, end }

  f v = Just (marloweHole : fromMaybe [] v)

insertHole m _ = m

insertConstant :: forall a. IsMarloweType a => Constants -> Term a -> Constants
insertConstant (Constants m) (Const name proxy start end) = Constants $ Map.alter f name m
  where
  marloweConstant = MarloweConstant { name, marloweType: (marloweType proxy), start, end }

  f v = Just (marloweConstant : fromMaybe [] v)

insertConstant m _ = m

class HasMarloweHoles a where
  getHoles :: Holes -> a -> Holes
  getConstants :: Constants -> a -> Constants

instance termHasMarloweHoles :: (IsMarloweType a, HasMarloweHoles a) => HasMarloweHoles (Term a) where
  getHoles m (Term a) = getHoles m a
  getHoles m (Const _ _ _ _) = m
  getHoles m h = insertHole m h
  getConstants m (Term a) = getConstants m a
  getConstants m (Hole _ _ _ _) = m
  getConstants m c = insertConstant m c

instance arrayHasMarloweHoles :: HasMarloweHoles a => HasMarloweHoles (Array a) where
  getHoles m as = foldMap (getHoles m) as
  getConstants m as = foldMap (getConstants m) as

-- Parsable versions of the Marlowe types
data Bound
  = Bound (Term BigInteger) (Term BigInteger)

derive instance genericBound :: Generic Bound _

instance showBound :: Show Bound where
  show v = genericShow v

instance prettyBound :: Pretty Bound where
  prettyFragment a = Leijen.text $ show a

instance boundFromTerm :: FromTerm Bound S.Bound where
  fromTerm (Bound a b) = S.Bound <$> termToValue a <*> termToValue b

instance boundIsMarloweType :: IsMarloweType Bound where
  marloweType _ = BoundType

instance boundHasMarloweHoles :: HasMarloweHoles Bound where
  getHoles m (Bound a b) = insertHole m a <> insertHole m b
  getConstants m (Bound a b) = insertConstant m a <> insertConstant m b

data AccountId
  = AccountId (Term BigInteger) (Term PubKey)

derive instance genericAccountId :: Generic AccountId _

instance showAccountId :: Show AccountId where
  show v = genericShow v

instance prettyAccountId :: Pretty AccountId where
  prettyFragment a = Leijen.text (show a)

instance accountIdFromTerm :: FromTerm AccountId S.AccountId where
  fromTerm (AccountId (Term b) (Term c)) = pure $ S.AccountId b c
  fromTerm _ = Nothing

instance accountIdIsMarloweType :: IsMarloweType AccountId where
  marloweType _ = AccountIdType

instance accountIdHasMarloweHoles :: HasMarloweHoles AccountId where
  getHoles m (AccountId a b) = insertHole m a <> insertHole m b
  getConstants m (AccountId a b) = insertConstant m a <> insertConstant m b

data ChoiceId
  = ChoiceId (Term String) (Term PubKey)

derive instance genericChoiceId :: Generic ChoiceId _

instance showChoiceId :: Show ChoiceId where
  show v = genericShow v

instance prettyChoiceId :: Pretty ChoiceId where
  prettyFragment a = Leijen.text (show a)

instance choiceIdFromTerm :: FromTerm ChoiceId S.ChoiceId where
  fromTerm (ChoiceId (Term a) (Term b)) = pure $ S.ChoiceId a b
  fromTerm _ = Nothing

instance choiceIdIsMarloweType :: IsMarloweType ChoiceId where
  marloweType _ = ChoiceIdType

instance choiceIdHasMarloweHoles :: HasMarloweHoles ChoiceId where
  getHoles m (ChoiceId a b) = insertHole m a <> insertHole m b
  getConstants m (ChoiceId a b) = insertConstant m a <> insertConstant m b

data Action
  = Deposit (Term AccountId) (Term Party) (Term Value)
  | Choice (Term ChoiceId) (Array (Term Bound))
  | Notify (Term Observation)

derive instance genericAction :: Generic Action _

instance showAction :: Show Action where
  show v = genericShow v

instance prettyAction :: Pretty Action where
  prettyFragment a = Leijen.text (show a)

instance actionFromTerm :: FromTerm Action S.Action where
  fromTerm (Deposit a b c) = S.Deposit <$> fromTerm a <*> termToValue b <*> fromTerm c
  fromTerm (Choice a b) = S.Choice <$> fromTerm a <*> (traverse fromTerm b)
  fromTerm (Notify a) = S.Notify <$> fromTerm a

instance actionMarloweType :: IsMarloweType Action where
  marloweType _ = ActionType

instance actionHasMarloweHoles :: HasMarloweHoles Action where
  getHoles m (Deposit a b c) = getHoles m a <> insertHole m b <> getHoles m c
  getHoles m (Choice a bs) = getHoles m a <> getHoles m bs
  getHoles m (Notify a) = getHoles m a
  getConstants m (Deposit a b c) = getConstants m a <> insertConstant m b <> getConstants m c
  getConstants m (Choice a bs) = getConstants m a <> getConstants m bs
  getConstants m (Notify a) = getConstants m a

data Payee
  = Account (Term AccountId)
  | Party (Term Party)

derive instance genericPayee :: Generic Payee _

instance showPayee :: Show Payee where
  show v = genericShow v

instance prettyPayee :: Pretty Payee where
  prettyFragment a = genericPretty a

instance payeeFromTerm :: FromTerm Payee S.Payee where
  fromTerm (Account a) = S.Account <$> fromTerm a
  fromTerm (Party (Term a)) = pure $ S.Party a
  fromTerm _ = Nothing

instance payeeMarloweType :: IsMarloweType Payee where
  marloweType _ = PayeeType

instance payeeHasMarloweHoles :: HasMarloweHoles Payee where
  getHoles m (Account a) = getHoles m a
  getHoles m (Party a) = insertHole m a
  getConstants m (Account a) = getConstants m a
  getConstants m (Party a) = insertConstant m a

data Case
  = Case (Term Action) (Term Contract)

derive instance genericCase :: Generic Case _

instance showCase :: Show Case where
  show v = genericShow v

-- FIXME: pretty printing is a disaster and slooooowwwww
instance prettyCase :: Pretty Case where
  prettyFragment (Case action' contract') = appendWithSoftbreak (Leijen.text "Case " <> prettyFragment action' <> Leijen.text " ") (prettyFragment contract')

instance caseFromTerm :: FromTerm Case S.Case where
  fromTerm (Case a b) = S.Case <$> fromTerm a <*> fromTerm b

instance caseMarloweType :: IsMarloweType Case where
  marloweType _ = CaseType

instance caseHasMarloweHoles :: HasMarloweHoles Case where
  getHoles m (Case a b) = getHoles m a <> getHoles m b
  getConstants m (Case a b) = getConstants m a <> getConstants m b

data Value
  = AvailableMoney (Term AccountId)
  | Constant (Term BigInteger)
  | NegValue (Term Value)
  | AddValue (Term Value) (Term Value)
  | SubValue (Term Value) (Term Value)
  | ChoiceValue (Term ChoiceId) (Term Value)
  | SlotIntervalStart
  | SlotIntervalEnd
  | UseValue (Term ValueId)

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show v = genericShow v

instance prettyValue :: Pretty Value where
  prettyFragment a = genericPretty a

instance valueFromTerm :: FromTerm Value S.Value where
  fromTerm (AvailableMoney a) = S.AvailableMoney <$> fromTerm a
  fromTerm (Constant a) = S.Constant <$> termToValue a
  fromTerm (NegValue a) = S.NegValue <$> fromTerm a
  fromTerm (AddValue a b) = S.AddValue <$> fromTerm a <*> fromTerm b
  fromTerm (SubValue a b) = S.SubValue <$> fromTerm a <*> fromTerm b
  fromTerm (ChoiceValue a b) = S.ChoiceValue <$> fromTerm a <*> fromTerm b
  fromTerm SlotIntervalStart = pure S.SlotIntervalStart
  fromTerm SlotIntervalEnd = pure S.SlotIntervalEnd
  fromTerm (UseValue a) = S.UseValue <$> fromTerm a

instance valueIsMarloweType :: IsMarloweType Value where
  marloweType _ = ValueType

instance valueHasMarloweHoles :: HasMarloweHoles Value where
  getHoles m (AvailableMoney a) = getHoles m a
  getHoles m (Constant a) = insertHole m a
  getHoles m (NegValue a) = getHoles m a
  getHoles m (AddValue a b) = getHoles m a <> getHoles m b
  getHoles m (SubValue a b) = getHoles m a <> getHoles m b
  getHoles m (ChoiceValue a b) = getHoles m a <> getHoles m b
  getHoles m SlotIntervalStart = mempty
  getHoles m SlotIntervalEnd = mempty
  getHoles m (UseValue a) = getHoles m a
  getConstants m (AvailableMoney a) = getConstants m a
  getConstants m (Constant a) = insertConstant m a
  getConstants m (NegValue a) = getConstants m a
  getConstants m (AddValue a b) = getConstants m a <> getConstants m b
  getConstants m (SubValue a b) = getConstants m a <> getConstants m b
  getConstants m (ChoiceValue a b) = getConstants m a <> getConstants m b
  getConstants m SlotIntervalStart = mempty
  getConstants m SlotIntervalEnd = mempty
  getConstants m (UseValue a) = getConstants m a

data Input
  = IDeposit (Term AccountId) (Term Party) (Term Money)
  | IChoice (Term ChoiceId) (Term ChosenNum)
  | INotify

data Observation
  = AndObs (Term Observation) (Term Observation)
  | OrObs (Term Observation) (Term Observation)
  | NotObs (Term Observation)
  | ChoseSomething (Term ChoiceId)
  | ValueGE (Term Value) (Term Value)
  | ValueGT (Term Value) (Term Value)
  | ValueLT (Term Value) (Term Value)
  | ValueLE (Term Value) (Term Value)
  | ValueEQ (Term Value) (Term Value)
  | TrueObs
  | FalseObs

derive instance genericObservation :: Generic Observation _

instance showObservation :: Show Observation where
  show v = genericShow v

instance prettyObservation :: Pretty Observation where
  prettyFragment a = genericPretty a

instance fromTermTerm :: FromTerm a b => FromTerm (Term a) b where
  fromTerm (Term a) = fromTerm a
  fromTerm _ = Nothing

instance observationFromTerm :: FromTerm Observation S.Observation where
  fromTerm (AndObs a b) = S.AndObs <$> fromTerm a <*> fromTerm b
  fromTerm (OrObs a b) = S.OrObs <$> fromTerm a <*> fromTerm b
  fromTerm (NotObs a) = S.NotObs <$> fromTerm a
  fromTerm (ChoseSomething a) = S.ChoseSomething <$> fromTerm a
  fromTerm (ValueGE a b) = S.ValueGE <$> fromTerm a <*> fromTerm b
  fromTerm (ValueGT a b) = S.ValueGT <$> fromTerm a <*> fromTerm b
  fromTerm (ValueLT a b) = S.ValueLT <$> fromTerm a <*> fromTerm b
  fromTerm (ValueLE a b) = S.ValueLE <$> fromTerm a <*> fromTerm b
  fromTerm (ValueEQ a b) = S.ValueEQ <$> fromTerm a <*> fromTerm b
  fromTerm TrueObs = pure S.TrueObs
  fromTerm FalseObs = pure S.FalseObs

instance observationIsMarloweType :: IsMarloweType Observation where
  marloweType _ = ObservationType

instance observationHasMarloweHoles :: HasMarloweHoles Observation where
  getHoles m (AndObs a b) = getHoles m a <> getHoles m b
  getHoles m (OrObs a b) = getHoles m a <> getHoles m b
  getHoles m (NotObs a) = getHoles m a
  getHoles m (ChoseSomething a) = getHoles m a
  getHoles m (ValueGE a b) = getHoles m a <> getHoles m b
  getHoles m (ValueGT a b) = getHoles m a <> getHoles m b
  getHoles m (ValueLT a b) = getHoles m a <> getHoles m b
  getHoles m (ValueLE a b) = getHoles m a <> getHoles m b
  getHoles m (ValueEQ a b) = getHoles m a <> getHoles m b
  getHoles m TrueObs = mempty
  getHoles m FalseObs = mempty
  getConstants m (AndObs a b) = getConstants m a <> getConstants m b
  getConstants m (OrObs a b) = getConstants m a <> getConstants m b
  getConstants m (NotObs a) = getConstants m a
  getConstants m (ChoseSomething a) = getConstants m a
  getConstants m (ValueGE a b) = getConstants m a <> getConstants m b
  getConstants m (ValueGT a b) = getConstants m a <> getConstants m b
  getConstants m (ValueLT a b) = getConstants m a <> getConstants m b
  getConstants m (ValueLE a b) = getConstants m a <> getConstants m b
  getConstants m (ValueEQ a b) = getConstants m a <> getConstants m b
  getConstants m TrueObs = mempty
  getConstants m FalseObs = mempty

data Contract
  = Close
  | Pay (Term AccountId) (Term Payee) (Term Value) (Term Contract)
  | If (Term Observation) (Term Contract) (Term Contract)
  | When (Array (Term Case)) (Term Timeout) (Term Contract)
  | Let (Term ValueId) (Term Value) (Term Contract)

derive instance genericContract :: Generic Contract _

instance showContract :: Show Contract where
  show v = genericShow v

instance prettyContract :: Pretty Contract where
  prettyFragment a = genericPretty a

instance contractFromTerm :: FromTerm Contract S.Contract where
  fromTerm Close = pure S.Close
  fromTerm (Pay a b c d) = S.Pay <$> fromTerm a <*> fromTerm b <*> fromTerm c <*> fromTerm d
  fromTerm (If a b c) = S.If <$> fromTerm a <*> fromTerm b <*> fromTerm c
  fromTerm (When as b c) = S.When <$> (traverse fromTerm as) <*> termToValue b <*> fromTerm c
  fromTerm (Let a b c) = S.Let <$> fromTerm a <*> fromTerm b <*> fromTerm c

instance contractIsMarloweType :: IsMarloweType Contract where
  marloweType _ = ContractType

instance contractHasMarloweHoles :: HasMarloweHoles Contract where
  getHoles m Close = mempty
  getHoles m (Pay a b c d) = getHoles m a <> getHoles m b <> getHoles m c <> getHoles m d
  getHoles m (If a b c) = getHoles m a <> getHoles m b <> getHoles m c
  getHoles m (When as b c) = getHoles m as <> insertHole m b <> getHoles m c
  getHoles m (Let a b c) = getHoles m a <> getHoles m b <> getHoles m c
  getConstants m Close = mempty
  getConstants m (Pay a b c d) = getConstants m a <> getConstants m b <> getConstants m c <> getConstants m d
  getConstants m (If a b c) = getConstants m a <> getConstants m b <> getConstants m c
  getConstants m (When as b c) = getConstants m as <> insertConstant m b <> getConstants m c
  getConstants m (Let a b c) = getConstants m a <> getConstants m b <> getConstants m c

newtype ValueId
  = ValueId String

derive instance genericValueId :: Generic ValueId _

instance showValueId :: Show ValueId where
  show (ValueId valueId') = show valueId'

instance prettyValueId :: Pretty ValueId where
  prettyFragment a = Leijen.text (show a)

instance valueIdFromTerm :: FromTerm ValueId S.ValueId where
  fromTerm (ValueId a) = pure $ S.ValueId a

instance valueIdIsMarloweType :: IsMarloweType ValueId where
  marloweType _ = ValueIdType

instance valueIdHasMarloweHoles :: HasMarloweHoles ValueId where
  getHoles m _ = m
  getConstants m _ = m

termToValue :: forall a. Term a -> Maybe a
termToValue (Term a) = Just a

termToValue _ = Nothing

class FromTerm a b where
  fromTerm :: a -> Maybe b

instance slotMarloweType :: IsMarloweType Slot where
  marloweType _ = SlotType

-- Replace all holes of a certain name with the value
replaceInPositions :: String -> MarloweHole -> Array MarloweHole -> String -> String
replaceInPositions constructor firstHole@(MarloweHole { marloweType }) holes currentContract =
  let
    offset (Position { line, column }) x = Position { line, column: column + x }

    lengthOfReplacement value (Position { column: start }) (Position { column: end }) = (length value) - (end - start)

    getLine (Position { line }) = line

    m = getMarloweConstructors marloweType

    holeString = constructMarloweType constructor firstHole m

    (_ /\ _ /\ final) =
      ( foldl
          ( \(currLength /\ currLineNumber /\ currString) hole@(MarloweHole { name, marloweType, start, end }) ->
              let
                thisLine = getLine start

                thisLength = currLength + (lengthOfReplacement holeString start end)
              in
                if currLineNumber == thisLine then
                  ( thisLength
                      /\ thisLine
                      /\ (replaceInPosition (offset start thisLength) (offset end thisLength) holeString currString)
                  )
                else
                  ( 0
                      /\ thisLine
                      /\ (replaceInPosition start end holeString currString)
                  )
          )
          (0 /\ 0 /\ currentContract)
          holes
      )
  in
    final
