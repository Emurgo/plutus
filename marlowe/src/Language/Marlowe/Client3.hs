{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Client3 where
import           Control.Monad              (void)
import           Control.Lens
import           Language.PlutusTx.AssocMap (Map)
import qualified Language.PlutusTx.AssocMap as Map
import           Language.Plutus.Contract
import           Language.Marlowe.Semantics hiding (Contract)
import qualified Language.PlutusCore.Universe          as PLC
import qualified Language.Marlowe.Semantics as Marlowe
import qualified Language.PlutusTx          as PlutusTx
import qualified Language.PlutusTx.Prelude  as P
import           Ledger                     (SlotRange, CurrencySymbol, ValidatorCtx(..), TokenName, Datum (..), pubKeyHash, Slot (..), mkValidatorScript, validatorHash, valueSpent)
import           Ledger.Ada                 (adaSymbol, adaValueOf)
import           Ledger.Interval
import           Language.Plutus.Contract.StateMachine (AsSMContractError, StateMachine (..), Void, getOnChainState)
import qualified Language.Plutus.Contract.StateMachine as SM
import           Ledger.Scripts             (Validator)
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Typed.Tx                               (TypedScriptTxOut (..))
import Ledger.Constraints
import qualified Ledger.Value               as Val

type MarloweSchema =
    BlockchainActions
        .\/ Endpoint "create" (MarloweParams, Marlowe.Contract)
        .\/ Endpoint "apply-inputs" (MarloweParams, [Input])
        .\/ Endpoint "sub" MarloweParams
        .\/ Endpoint "wait" MarloweParams

data MarloweError =
    StateMachineError (SM.SMContractError MarloweData [Input])
    | OtherContractError ContractError
  deriving (Show)

makeClassyPrisms ''MarloweError

instance AsSMContractError MarloweError MarloweData [Input] where
    _SMContractError = _StateMachineError

instance AsContractError MarloweError where
    _ContractError = _OtherContractError


marloweContract2 :: forall e. (AsContractError e
    , AsSMContractError e MarloweData [Input]
    )
    => Contract MarloweSchema e ()
marloweContract2 = do
    create `select` wait
  where
    create = do
        -- traceM "Here create"
        (params, cont) <- endpoint @"create" @(MarloweParams, Marlowe.Contract) @MarloweSchema
        -- traceM $ "Here cont " <> show cont
        createContract params cont
        apply `select` wait
    wait = do
        params <- endpoint @"wait" @MarloweParams @MarloweSchema
        r <- SM.waitForUpdate (client params)
        case r of
            Just (TypedScriptTxOut{tyTxOutData=_currentState}, _txOutRef) -> do
                -- traceM $ (show currentState)
                apply `select` wait
            Nothing -> pure () -- the contract is closed, no UTxO
    _sub = do
        params <- endpoint @"sub" @MarloweParams @MarloweSchema
        -- let inst = scriptInstance params
            -- address = (Scripts.scriptAddress inst)
        ((TypedScriptTxOut{tyTxOutData=_currentState}, _txOutRef), _utxo) <- getOnChainState (client params)
        -- traceM $ (show utxo)
        -- traceM $ (show currentState)
        -- txs <- nextTransactionsAt address
        -- traceM $ (show txs)
        apply `select` wait
    apply = do
        -- traceM "Here apply"
        (params, inputs) <- endpoint @"apply-inputs" @(MarloweParams, [Input]) @MarloweSchema
        -- traceM $ "Here endpoint " <> show inputs
        MarloweData{..}  <- applyInputs params inputs
        case marloweContract of
            Close -> pure ()
            _ -> apply `select` wait


{-| Create a Marlowe contract.
    Uses wallet public key to generate a unique script address.
 -}
createContract :: (AsContractError e, AsSMContractError e MarloweData [Input])
    => MarloweParams
    -> Marlowe.Contract
    -> Contract MarloweSchema e ()
createContract params contract = do
    slot <- awaitSlot 0
    _creator <- pubKeyHash <$> ownPubKey
    -- traceM $ "createContract: " <> show slot <> " " <> show creator
    let marloweData = MarloweData {
            marloweContract = contract,
            marloweState = emptyState slot }

    let payValue = adaValueOf 0
    let theClient = client params

    void $ SM.runInitialise theClient marloweData payValue


applyInputs :: (AsContractError e, AsSMContractError e MarloweData [Input])
    => MarloweParams
    -> [Input]
    -> Contract MarloweSchema e MarloweData
applyInputs params inputs = do
    (Slot slot) <- awaitSlot 1
    -- let slot = Slot 0
    let slotRange = interval (Slot $ slot - 1)  (Slot $ slot + 10)
    -- traceM $ "slotRange: " <> show slotRange
    let theClient = client params
    dat <- SM.runStep theClient inputs slotRange
    -- traceM $ "[applyInputs] After runStep " <> show (marloweContract dat) <> " ==> " <>  show (isFinal dat)
    return dat



rolePayoutScript :: Validator
rolePayoutScript = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]))
  where
    wrapped = Scripts.wrapValidator rolePayoutValidator


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: (CurrencySymbol, TokenName) -> () -> ValidatorCtx -> Bool
rolePayoutValidator (currency, role) _ ctx =
    Val.valueOf (valueSpent (valCtxTxInfo ctx)) currency role P.> 0


marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rolesCurrency = MarloweParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = validatorHash rolePayoutScript }


defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


{-# INLINABLE transition #-}
transition :: MarloweParams -> SM.State MarloweData -> [Input] -> SlotRange -> Maybe (TxConstraints Void Void, SM.State MarloweData)
transition params SM.State{ SM.stateData=MarloweData{..}, SM.stateValue=scriptInValue} inputs range = do
    let interval = case range of
            Interval (LowerBound (Finite l) True) (UpperBound (Finite h) True) -> (l, h)
            _ -> P.traceError "Tx valid slot must have lower bound and upper bounds"

    let positiveBalances = validateBalances marloweState ||
            P.traceError "Invalid contract state. There exists an account with non positive balance"

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsConstraints = validateInputs params inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = inputBalance == scriptInValue

    let preconditionsOk = P.traceIfFalse "Preconditions are false" $ positiveBalances && balancesOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (deducedTxOutputs, finalBalance) = case txOutContract of
                    Close -> (txPaymentOuts txOutPayments, P.zero)
                    _ -> let
                        txWithPayouts = txPaymentOuts txOutPayments
                        totalPayouts = foldMap (\(Payment _ v) -> v) txOutPayments
                        finalBalance = totalIncome P.- totalPayouts
                        in (txWithPayouts, finalBalance)
            if preconditionsOk
            then Just (inputsConstraints <> deducedTxOutputs, SM.State marloweData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    validateInputWitness :: CurrencySymbol -> Input -> TxConstraints Void Void
    validateInputWitness rolesCurrency input =
        case input of
            IDeposit _ party _ _         -> validatePartyWitness party
            IChoice (ChoiceId _ party) _ -> validatePartyWitness party
            INotify                      -> mempty
      where
        validatePartyWitness (PK pk)     = mustBeSignedBy pk
        validatePartyWitness (Role role) = mustSpendValue (Val.singleton rolesCurrency role 1)

    validateInputs :: MarloweParams -> [Input] -> TxConstraints Void Void
    validateInputs MarloweParams{..} = foldMap (validateInputWitness rolesCurrency)

    collectDeposits :: Input -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                    = P.zero

    totalIncome :: Val.Value
    totalIncome = foldMap collectDeposits inputs

    txPaymentOuts :: [Payment] -> TxConstraints i0 o0
    txPaymentOuts payments = foldMap paymentToTxOut paymentsByParty
      where
        paymentsByParty = Map.toList $ foldr collectPayments Map.empty payments

        paymentToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ PlutusTx.toData (rolesCurrency params, role)
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value

    collectPayments :: Payment -> Map Party Money -> Map Party Money
    collectPayments (Payment party money) payments = let
        newValue = money P.+ P.fromMaybe P.zero (Map.lookup party payments)
        in Map.insert party newValue payments


{-# INLINABLE isFinal #-}
isFinal :: MarloweData -> Bool
isFinal MarloweData{marloweContract=c} = c P.== Close


{-# INLINABLE mkValidator #-}
mkValidator :: MarloweParams -> Scripts.ValidatorType MarloweSym
mkValidator p = SM.mkValidator $ SM.mkStateMachine (transition p) isFinal

validatorCode :: MarloweParams -> PlutusTx.CompiledCode PLC.DefaultUni (Scripts.ValidatorType MarloweSym)
validatorCode params = $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params

type MarloweSym = StateMachine MarloweData [Input]

scriptInstance :: MarloweParams -> Scripts.ScriptInstance MarloweSym
scriptInstance params = Scripts.validator @MarloweSym
    (validatorCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarloweData @[Input]

machineInstance :: MarloweParams -> SM.StateMachineInstance MarloweData [Input]
machineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine (transition params) isFinal)
    (scriptInstance params)

client :: MarloweParams -> SM.StateMachineClient MarloweData [Input]
client p = SM.mkStateMachineClient (machineInstance p)
