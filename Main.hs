{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Lens
import           Control.Monad (when,forever)
import           Control.Monad.Catch (try)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (ask)
import           Control.Monad.State (modify)
import           Control.Monad.Trans.Resource
import           Data.Acid
import           Data.SafeCopy
import           Data.String.Utils
import           Data.Typeable
import           Language.Haskell.Interpreter (runInterpreter)
import           Mueval.ArgsParse
import           Mueval.Interpreter
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Tokens
import Common

-- Strip off the first world (which is assumed to be the screenname of the
-- bot).
getHaskellExpression :: T.Text -> T.Text
getHaskellExpression t =
    case T.breakOn " " $ T.strip t of
      (a, "") -> a
      (_, rest) -> T.strip rest

isHaskellPost :: T.Text -> Status -> Bool
isHaskellPost userName status =
    (T.isPrefixOf userName $ status ^. statusText) &&
    (status ^. statusUser ^. userScreenName) /= botScreenName

evalExpr :: String -> IO String
evalExpr e =
      case getOptions ["--expression", e] of
        Left t@(b, e) -> return $ show t
        Right opts -> do
          r <- runInterpreter (interpreter opts)
          case r of
              Left err -> return $ show err
              Right (e,et,val) -> do (out, b) <- getResult val
                                     return out

getResult :: (Functor m, MonadIO m) => String -> m (String, Bool)
getResult str = render 1024 str

statusToText :: Status -> T.Text
statusToText status = T.concat [ T.pack . show $ status ^. statusId
                                       , ": "
                                       , status ^. statusUser . userScreenName
                                       , ": "
                                       , status ^. statusText
                                       ]

evalExpression :: MonadIO m => Status -> m String
evalExpression status = do
    r <- liftIO $ evalExpr $ decodeHtml $ T.unpack $ getHaskellExpression $ status ^. statusText
    return $ take 140 r

{-TODO: Make this more comprehensive-}
decodeHtml :: String -> String
decodeHtml s =
    replace "&lt;" "<" $
    replace "&gt;" ">" $
    replace "&amp;" "&" $
    replace "&quot;" "\"" $
    replace "&#39;" "'" $ s

-- res <- call $ update "Hello World"
reply :: Integer -> T.Text -> APIRequest StatusesUpdate Status
reply i s =
  Web.Twitter.Conduit.update s & inReplyToStatusId ?~ i

postreply :: (MonadResource m, MonadLogger m) => Status -> Integer -> String -> TW m Status
postreply status i res = call (reply i $ (T.take 140 $
                                 T.concat ["@",
                                          status ^. statusUser ^. userScreenName,
                                          " ",
                                          T.pack res]))


{-Acid State database to keep track of replies-}
data TweetId = TweetId { tweetId :: Integer }
             deriving (Eq, Show, Typeable)
data LambdaTwitDb = LambdaTwitDb { allReplyIds :: [TweetId] }
                  deriving (Typeable)

allReplies :: Query LambdaTwitDb [TweetId]
allReplies = allReplyIds <$> ask

addReply :: TweetId -> Update LambdaTwitDb ()
addReply tweetId = modify go
  where go (LambdaTwitDb db) = LambdaTwitDb $ tweetId : db

{-The Acid State magic-}
deriveSafeCopy 0 'base ''TweetId
deriveSafeCopy 0 'base ''LambdaTwitDb
makeAcidic ''LambdaTwitDb ['allReplies, 'addReply]

conduitmain :: IO ()
conduitmain = do
  state <- openLocalState (LambdaTwitDb [])
  forever $ do
    {-TODO: Use Data.Configurator to read in the oauth keys without needing a recompile:
    - http://hackage.haskell.org/package/configurator-}
    runNoLoggingT . runTwitterFromEnv $ do
      sourceWithMaxId mentionsTimeline
           C.$= CL.isolate 100
           C.$$ CL.mapM_ $ \status -> do
               replies <- liftIO $ query state AllReplies
               if ((TweetId (status ^. statusId)) `elem` replies)
                 then do
                   liftIO $ putStrLn "Already replied to:"
                   liftIO $ T.putStrLn $ statusToText status
                   liftIO $ threadDelay $ 60 * 1000000
                 else do
                   when (isHaskellPost botScreenName status) $ do
                     liftIO $ T.putStrLn $ statusToText status
                     res <- evalExpression status
                     liftIO $ putStrLn res
                     postres <- try $ postreply status (status ^. statusId) res
                     case postres of
                       Left (FromJSONError e) -> liftIO $ print e
                       Left (TwitterErrorResponse s resH errs) ->
                         liftIO $ print errs
                       Left (TwitterStatusError s resH val) ->
                         liftIO $ print val
                       Right status -> liftIO $ print $ statusToText status
                   liftIO $ Data.Acid.update state (AddReply $ TweetId (status ^. statusId))
                   -- AA TODO: Better rate limiting, this probably blocks every tweet.
                   -- We should only wait for 60 seconds after each mentionsTimeline grab
                   liftIO $ threadDelay $ 60 * 1000000

main :: IO ()
main = conduitmain

{-TODO: Import lens:-}
{-https://twitter.com/relrod6/status/516785803100688384-}

