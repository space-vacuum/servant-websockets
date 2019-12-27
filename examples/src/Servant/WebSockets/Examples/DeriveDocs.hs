module Servant.WebSockets.Examples.DeriveDocs where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API ((:<|>), (:>), JSON)
import Servant.Docs (ToSample(toSamples), HasDocs, samples)
import Servant.WebSockets.Docs (DeriveWebSocketDocs)

data Message = Message { from :: Text, body :: Text }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToSample Message where
  toSamples _ = samples [ Message { from = "joe", body = "hi" } ]

data ReceiveOnly
data SendOnly
data SendAndReceive

type MyWSAPI =
  "ws" :>
    (    "receive-only" :> ReceiveOnly
    :<|> "send-only"    :> SendOnly
    :<|> "send-receive" :> SendAndReceive
    )

myWSAPI :: Proxy MyWSAPI
myWSAPI = Proxy

deriving
  via DeriveWebSocketDocs () '( '[JSON], Message ) ReceiveOnly
  instance HasDocs ReceiveOnly

deriving
  via DeriveWebSocketDocs '( '[JSON], Message ) () SendOnly
  instance HasDocs SendOnly

deriving
  via DeriveWebSocketDocs
        '( '[JSON], Message )
        '( '[JSON], Message )
        SendAndReceive
  instance HasDocs SendAndReceive
