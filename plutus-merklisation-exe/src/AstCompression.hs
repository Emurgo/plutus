{- Perform various transformations on the ASTs for the validators of
   the sample contracts and print out a markdown table showing how
   these effect the size of the CBOR. -}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

--{-# OPTIONS_GHC -fno-warn-unused-imports #-}
--{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
--{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module AstCompression (main) where

import qualified Language.PlutusCore                                           as PLC
import qualified Language.PlutusCore.CBOR                                      as PLC ()

import           Language.PlutusCore.Erasure.Untyped.Convert                   as C

import           Language.PlutusTx.Coordination.Contracts.Crowdfunding         as Crowdfunding
import           Language.PlutusTx.Coordination.Contracts.Currency             as Currrency
import           Language.PlutusTx.Coordination.Contracts.Escrow               as Escrow
import           Language.PlutusTx.Coordination.Contracts.Future               as Future
import           Language.PlutusTx.Coordination.Contracts.Game                 as Game
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine     as GameStateMachine
import           Language.PlutusTx.Coordination.Contracts.MultiSig             as MultiSig
import           Language.PlutusTx.Coordination.Contracts.MultiSigStateMachine as MultiSigStateMachine
import           Language.PlutusTx.Coordination.Contracts.PubKey               as PubKey
import           Language.PlutusTx.Coordination.Contracts.Swap                 as Swap
import           Language.PlutusTx.Coordination.Contracts.TokenAccount         as TokenAccount
import           Language.PlutusTx.Coordination.Contracts.Vesting              as Vesting

import           Language.PlutusTx                                             as PlutusTx

import qualified Codec.Compression.GZip                                        as G
import           Codec.Serialise                                               (serialise)
import qualified Data.ByteString.Lazy                                          as B
import           GHC.Int                                                       (Int64)
import           Numeric

{----------------- Analysis -----------------}

printHeader :: IO ()
printHeader = do
  putStrLn "| Contract | Compression | Typed | Typed, stringless names | Untyped | Untyped, stringless names | Untyped, integer IDs only | Untyped, de Bruijn |"
  putStrLn "| :---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | "
-- ^ The original table had a column at the end for "annotations not serialised".

printSeparator :: IO ()
printSeparator = do
  putStrLn "| |"  -- This is to separate entries in a table.  Two bars seems to be enough (but not one on GitHub).
  putStrLn "| |"  -- A thicker line or something would be better, but I don't think you can do that.

data CompressionMode = Uncompressed | Compressed
data PrintFormat = Alone | WithPercentage

printEntry :: Int64 -> (B.ByteString, PrintFormat) -> CompressionMode -> IO ()
printEntry fullSize (s, mode) cmode = do
  let s' = case cmode of
             Uncompressed -> s
             Compressed   -> G.compressWith G.defaultCompressParams {G.compressLevel = G.bestCompression} s
  let len = B.length s'
  putStr . show $ len
  case mode of
    Alone -> pure ()
    WithPercentage ->
        putStr $ " (" ++ Numeric.showFFloat (Just 1) (percentage len) "%" ++ ")"
        where percentage n = 100.0 * (fromIntegral n) / (fromIntegral fullSize) :: Float

printInfo1 :: Int64 -> [(B.ByteString, PrintFormat)] -> CompressionMode -> IO ()
printInfo1 _fullSize [] _ = putStrLn ""
printInfo1 fullSize (i : rest) cmode = do
  printEntry fullSize i cmode
  putStr " | "
  printInfo1 fullSize rest cmode

printInfo :: Int64 -> [(B.ByteString, PrintFormat)] -> IO ()
printInfo fullSize entries = do
  putStr " Uncompressed | "
  printInfo1 fullSize entries Uncompressed
  putStr "|     | Compressed | "
  printInfo1 fullSize entries Compressed


-- Print out various compression statistics for a program.  By
-- default, serialisation will include units.  To omit units, replace
-- the import Erasure.Untyped.CBOR by Erasure.Untyped.CBOR2 in
-- Language.PlutusCore.Merkle.Merklise This will entail a lot of
-- recompilation.

analyseCompression :: String -> PLC.Program PLC.TyName PLC.Name () -> IO ()
analyseCompression name prog = do
  let s1 = serialise prog  -- This will use
      s2 = serialise . C.removeNameStrings $ prog
      s3 = serialise . C.erasePLCProgram $ prog
      s4 = serialise . C.erasePLCProgram . C.removeNameStrings $ prog
      s5 = serialise . C.nameToIntProgram . C.erasePLCProgram $ prog
      s6 = serialise . C.deBruijnToIntProgram . C.erasePLCProgram . C.deBruijnPLCProgram $ prog
  putStr $ "| " ++ name ++ " | "
  printInfo (B.length s1) [(s1, Alone), (s2, Alone), (s3, Alone), (s4, Alone), (s5, WithPercentage), (s6, WithPercentage)]


analyseProg :: String -> CompiledCode a -> IO ()
analyseProg name prg = do
  analyseCompression name $ PlutusTx.getPlc prg


main :: IO ()
main = do
  printHeader
  analyseProg    "Crowdfunding"         Crowdfunding.exportedValidator
  printSeparator
  analyseProg    "Currrency"            Currrency.exportedValidator
  printSeparator
  analyseProg    "Escrow"               Escrow.exportedValidator
  printSeparator
  analyseProg    "Future"               Future.exportedValidator
  printSeparator
  analyseProg    "Game"                 Game.exportedValidator
  printSeparator
  analyseProg    "GameStateMachine"     GameStateMachine.exportedValidator
  printSeparator
  analyseProg    "MultiSig"             MultiSig.exportedValidator
  printSeparator
  analyseProg    "MultiSigStateMachine" MultiSigStateMachine.exportedValidator
  printSeparator
  analyseProg    "PubKey"               PubKey.exportedValidator
  printSeparator
  analyseProg    "Swap"                 Swap.exportedValidator
  printSeparator
  analyseProg    "TokenAccount"         TokenAccount.exportedValidator
  printSeparator
  analyseProg    "Vesting"              Vesting.exportedValidator

-- Current validator is a little different for Future and PubKey

-- See plutus-use-cases/bench/Bench.hs for examples of manipulating PLC code
