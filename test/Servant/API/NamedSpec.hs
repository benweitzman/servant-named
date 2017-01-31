{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.API.NamedSpec where

import Data.Proxy

import Network.HTTP.Types.Method

import Servant.API.Named

import Servant.API
import Servant.Server


import Test.Hspec
import Test.Hspec.Wai

handler :: Server API
handler = fromNamed $ Named @"getUser"  := return
                ::<|> Named @"getUsers" := return [1,2,3]
                ::<|> Nil

type API = FromNamed ( Named "getUsers" := "user" :> Get '[JSON] [Int]
                 ::<|> Named "getUser"  := "user" :> Capture "id" Int :> Get '[JSON] Int
                 ::<|> Nil
                 )

spec :: Spec
spec = do
  describe "Servant.API.Named" $ do
      with (return $ serve (Proxy @API) handler) $ do
        it "should work" $ do
          get "/user" `shouldRespondWith` "[1,2,3]"
          get "/user/100" `shouldRespondWith` "100"
