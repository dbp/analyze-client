{-# LANGUAGE OverloadedStrings #-}

module Web.Analyze.Client (
       logWrapper
  ) where

import qualified Snap.Core as S (Request)
import Snap.Core (rqContextPath, rqPathInfo, rqMethod, getRequest, Method(..))
import Snap.Snaplet (Handler)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Network.HTTP.Conduit (Manager, parseUrl, Request(..), httpLbs)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (concat, append)
import qualified Data.ByteString.Char8 as B8 (pack)

logWrapper :: Manager -> ByteString -> Handler b v a -> Handler b v a
logWrapper man token h = do
  start <- liftIO getCurrentTime
  res <- h
  end <- liftIO getCurrentTime
  req <- getRequest
  liftIO $ forkIO (sendResult man token req start end)
  return res

sendResult :: Manager -> ByteString -> S.Request -> UTCTime -> UTCTime -> IO ()
sendResult man token req start end = do
    let time = milliseconds (diffUTCTime end start) :: Int
    initreq <- parseUrl "http://analyze.positionstudios.com/submit/visit"
    let url = B.append (rqContextPath req) (rqPathInfo req)
    let meth = methodtobs (rqMethod req)
    let httpreq = initreq { method = "POST"
                          , queryString = B.concat ["url="
                                                   , url
                                                   , "&render="
                                                   , B8.pack (show time)
                                                   , "&method="
                                                   , meth
                                                   , "&token="
                                                   , token]}
    void (httpLbs httpreq man)
  where milliseconds = floor . fromRational . (1000 *) . toRational
        methodtobs GET = "get"
        methodtobs POST = "post"
        methodtobs PUT = "put"
        methodtobs DELETE = "delete"
