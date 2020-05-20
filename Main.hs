{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson qualified as J
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as J
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as Map
import Data.Semigroup (First(..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Deriving.Aeson.Stock
import Data.Semigroup.Generic
import GHC.Clock
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HC
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment

data Config = Config
  { configToken :: Text
  , configDelay :: Double
  , configUrl :: Maybe String
  , configPort :: Int
  } deriving Generic
  deriving FromJSON via PrefixedSnake "config" Config

data WaiException = WaiException Status BL.ByteString
  deriving Show
instance Exception WaiException

data Summary = Summary
  { summaryTime :: First Double
  , summaryLink :: First Text
  , summaryText :: [Text]
  , summaryAuthors :: Set.Set Text
  } deriving Generic
  deriving Semigroup via GenericSemigroup Summary
type Summaries = Map.Map Text Summary

collect :: Double -> J.Value -> J.Parser Summaries
collect now = J.withObject "Slack" $ \obj -> do
  xs <- obj J..: "attachments"
  fmap Map.fromList $ forM xs $ \att -> do
    author <- att J..: "author_name"
    name <- att J..: "title"
    link <- att J..: "title_link"
    text <- att J..: "text"
    pure (name, Summary
      (First now)
      (First link)
      [text]
      (Set.singleton author))

post :: HC.Manager -> Maybe HC.Request -> Summaries -> IO ()
post man mreq content = do
  let attachments =
        [ [aesonQQ|
            { title: #{title}
            , title_link: #{link}
            , author_name: #{authors'}
            , text: #{T.intercalate "\n" (reverse texts)}
            , mrkdwn: true
            , mrkdwn_in: ["text"]
            }
            |]
        | (title, Summary _ (First link) texts authors) <- Map.toList content
        , let authors' = T.unwords $ Set.toList authors
        ]
      payload = [aesonQQ|{attachments: #{attachments}}|]
  case mreq of
    Just req -> do 
      resp <- HC.httpLbs req
        { HC.method = "POST"
        , HC.requestHeaders = [("Content-Type", "application/json")]
        , HC.requestBody = HC.RequestBodyLBS $ J.encode (payload :: J.Value)
        } man
      print $ HC.responseBody resp
    Nothing -> BL.putStrLn $ J.encode payload

main :: IO ()
main = getArgs >>= \[path] -> do

  Config{..} <- Yaml.decodeFileThrow path
  reqBase <- traverse HC.parseUrlThrow configUrl
  vQueue <- newTVarIO mempty
  man <- HC.newTlsManager

  _ <- forkIO $ forever $ do
    now <- getMonotonicTime
    mcontent <- atomically $ optional $ do
      content <- readTVar vQueue
      let (ready, remainder) = Map.partition
            (\s -> now - getFirst (summaryTime s) >= configDelay)
            content
      when (Map.null ready) retry
      writeTVar vQueue remainder
      pure ready
    forM_ mcontent $ post man reqBase
    threadDelay 1000000
  
  putStrLn $ "Listening on " <> show configPort
  Warp.run configPort $ \req sendResp -> case pathInfo req of
    [tk] | tk == configToken -> do
      body <- strictRequestBody req
      BL.putStrLn body
      obj <- either
        (throwIO . WaiException status400 . BB.toLazyByteString . BB.stringUtf8)
        pure
        $ J.eitherDecode body
      now <- getMonotonicTime
      case J.parseEither (collect now) obj of
        Left e -> putStrLn e
        Right v -> do
          atomically $ modifyTVar' vQueue (Map.unionWith (<>) v)
      sendResp $ responseBuilder status200 [] "ok"
    _ -> sendResp $ responseBuilder status404 [] "Not found"
    `catch` \(WaiException status msg) -> sendResp $ responseLBS status [] msg