# servant-named

This package aims to address an issue with the amazing [servant](https://hackage.haskell.org/package/servant) library, wherein it can be hard to manage two complex lists of API endpoints (one of types, and one of handlers), making sure to keep them in the same order.

To address this, we introduce the concept of named endpoints. When using named endpoints, it's no longer necessary to keep endpoint types and handlers in the same order. When serving the servant application, the types and handlers are automatically matched up according to their names. Here's a little example:

```haskell
handler :: Server API
handler = fromNamed $ Named @"getUser"  := return
                ::<|> Named @"getUsers" := return [1,2,3]
                ::<|> Nil

type API = FromNamed ( Named "getUsers" := "user" :> Get '[JSON] [Int]
                 ::<|> Named "getUser"  := "user" :> Capture "id" Int :> Get '[JSON] Int
                 ::<|> Nil
                 )
```                 

The endpoints are defined in opposite orders, but this still typechecks and works as expected!
