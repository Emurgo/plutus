module Marlowe.Gists
  ( mkNewGist
  , currentSimulationMarloweGistFile
  , oldSimulationMarloweGistFile
  , simulationState
  ) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Either (hush)
import Data.Lens (view)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Generic (decodeJSON, encodeJSON)
import Gist (Gist, GistFile, NewGist(NewGist), NewGistFile(NewGistFile), gistFileContent)
import Gists (firstMatch)
import Language.Haskell.Interpreter (SourceCode)
import Simulation.State (MarloweState)

mkNewGist ::
  Maybe SourceCode ->
  Maybe SourceCode ->
  NonEmptyList MarloweState ->
  Maybe NewGist
mkNewGist currentSource oldSource marloweState =
  if Array.null gistFiles then
    Nothing
  else
    Just
      $ NewGist
          { _newGistDescription: "Marlowe Smart Contract"
          , _newGistPublic: true
          , _newGistFiles: gistFiles
          }
  where
  gistFiles =
    catMaybes
      [ mkNewGistFile currentSimulationMarloweFile <<< unwrap <$> currentSource
      , mkNewGistFile oldSimulationMarloweFile <<< unwrap <$> oldSource
      , Just $ mkNewGistFile simulationFile (encodeJSON stateArray)
      ]

  stateArray :: Array MarloweState
  stateArray = NEL.toUnfoldable marloweState

  mkNewGistFile _newGistFilename _newGistFileContent =
    NewGistFile
      { _newGistFilename
      , _newGistFileContent
      }

currentSimulationMarloweFile :: String
currentSimulationMarloweFile = "CurrentMarlowe.hs"

oldSimulationMarloweFile :: String
oldSimulationMarloweFile = "OldMarlowe.hs"

simulationFile :: String
simulationFile = "simulation.json"

currentSimulationMarloweGistFile :: Gist -> Maybe GistFile
currentSimulationMarloweGistFile = firstMatch currentSimulationMarloweFile

oldSimulationMarloweGistFile :: Gist -> Maybe GistFile
oldSimulationMarloweGistFile = firstMatch oldSimulationMarloweFile

simulationState :: Gist -> Maybe (NonEmptyList MarloweState)
simulationState gist = do
  gistFile <- firstMatch simulationFile gist
  content <- view gistFileContent gistFile
  stateList <- hush $ runExcept $ decodeJSON content
  NEL.fromList stateList
