{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import API (API)
import qualified Auth
import Auth.Types (OAuthClientId(OAuthClientId), OAuthClientSecret(OAuthClientSecret))
import Aws.Lambda
import Aws.Lambda.Wai (WaiHandler, waiHandler)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (ToJSON, eitherDecode, encode)
import Data.IORef (readIORef)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Language.Marlowe.ACTUS.Definitions.ContractTerms (ContractTerms)
import Language.Marlowe.ACTUS.Generator (genFsContract, genStaticContract)
import Language.Marlowe.Pretty (pretty)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant (Application, Handler (Handler), Server, ServerError, hoistServer, serve, (:<|>) ((:<|>)))
import qualified Web.JWT as JWT

genActusContract :: ContractTerms -> Handler String
genActusContract = pure . show . pretty . genFsContract

genActusContractStatic :: ContractTerms -> Handler String
genActusContractStatic = pure . show . pretty . genStaticContract

liftedAuthServer :: Auth.GithubEndpoints -> Auth.Config -> Server Auth.API
liftedAuthServer githubEndpoints config =
  hoistServer (Proxy @Auth.API) liftAuthToHandler Auth.server
  where
    liftAuthToHandler ::
      ReaderT (Auth.GithubEndpoints, Auth.Config) (LoggingT (ExceptT ServerError IO)) a ->
      Handler a
    liftAuthToHandler =
      Handler . runStderrLoggingT . flip runReaderT (githubEndpoints, config)

type Web = API :<|> Auth.API

mkHandlers :: (MonadIO m) => AppConfig -> m (Server Web)
mkHandlers AppConfig {..} = do
  githubEndpoints <- liftIO Auth.mkGithubEndpoints
  pure $ (mhandlers :<|> liftedAuthServer githubEndpoints authConfig)

mhandlers :: Server API
mhandlers = genActusContract :<|> genActusContractStatic

app :: Server Web -> Application
app handlers =
  cors (const $ Just policy) . serve (Proxy @Web) $ handlers
  where
    policy =
      simpleCorsResourcePolicy

data AppConfig = AppConfig {authConfig :: Auth.Config}

initializeContext :: IO AppConfig
initializeContext = do
  let authConfig =
        Auth.Config
          { _configJWTSignature = JWT.hmacSecret "",
            _configRedirectUrl = "",
            _configGithubClientId = OAuthClientId "",
            _configGithubClientSecret = OAuthClientSecret ""
          }
  pure $ AppConfig authConfig

initializeApplication :: AppConfig -> IO Application
initializeApplication config = do
  handlers <- mkHandlers config
  pure $ app handlers

handler :: WaiHandler AppConfig
handler request context = do
  -- This application config will be preserved while the Lambda is warm
  appConfig :: AppConfig <- readIORef $ customContext context

  -- This uses Aws.Lambda.Wai from aws-lambda-haskell-runtime-wai in order to convert the Wai application to Lambda.
  waiHandler (initializeApplication appConfig) request context

generateLambdaDispatcher UseWithAPIGateway defaultDispatcherOptions