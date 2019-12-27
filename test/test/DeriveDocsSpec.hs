module DeriveDocsSpec where

import Test.Hspec

import Servant.WebSockets.Examples.DeriveDocs (myWSAPI)
import Servant.Docs (docs, markdown)

spec :: Spec
spec =
  describe "MyWSAPI docs" $ do
    it "should generate correct docs" $ do
      lines (markdown (docs myWSAPI)) `shouldBe`
        [ "## WEBSOCKET /ws/receive-only"
        , ""
        , "### Response:"
        , ""
        , "- Status code 1000"
        , "- Headers: []"
        , ""
        , "- Example (`application/json;charset=utf-8`, `application/json`):"
        , ""
        , "```javascript"
        , "{\"body\":\"hi\",\"from\":\"joe\"}"
        , "```"
        , ""
        , "## WEBSOCKET /ws/send-only"
        , ""
        , "### Request:"
        , ""
        , "- Example (`application/json;charset=utf-8`, `application/json`):"
        , ""
        , "```javascript"
        , "{\"body\":\"hi\",\"from\":\"joe\"}"
        , "```"
        , ""
        , "### Response:"
        , ""
        , "- Status code 1000"
        , "- Headers: []"
        , ""
        , "- No response body"
        , ""
        , "## WEBSOCKET /ws/send-receive"
        , ""
        , "### Request:"
        , ""
        , "- Example (`application/json;charset=utf-8`, `application/json`):"
        , ""
        , "```javascript"
        , "{\"body\":\"hi\",\"from\":\"joe\"}"
        , "```"
        , ""
        , "### Response:"
        , ""
        , "- Status code 1000"
        , "- Headers: []"
        , ""
        , "- Example (`application/json;charset=utf-8`, `application/json`):"
        , ""
        , "```javascript"
        , "{\"body\":\"hi\",\"from\":\"joe\"}"
        , "```"
        , ""
        ]
