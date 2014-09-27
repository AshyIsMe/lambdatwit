{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Monad (forM_,guard,mplus,unless,when,forever)
import           Control.Monad.Error (catchError)
import           Control.Monad.Reader (ask)
import           Control.Monad.State (modify)
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.Default
import           Network.HTTP.Conduit
import           System.Environment
import           System.IO (hFlush, stdout)
import           Web.Authenticate.OAuth (OAuth(..), Credential(..))
import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Web.Authenticate.OAuth as OA
import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable

import Language.Haskell.Interpreter (runInterpreter)

import Mueval.Interpreter
import Mueval.ArgsParse

import Tokens


authorize :: (MonadBaseControl IO m, MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> Manager
          -> m Credential
authorize oauth getPIN mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

withCredential :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) => TW (ResourceT m) a -> m a
withCredential task = do
    cred <- liftIO $ withManager $ \mgr -> authorize tokens getPIN mgr
    _ <- liftIO $ putStrLn $ show cred
    let env = setCredential tokens cred def
    runTW env task
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine


isHaskellExpression :: T.Text -> T.Text -> Bool
isHaskellExpression myUserName post = T.isPrefixOf myUserName post

getHaskellExpression :: T.Text -> T.Text
getHaskellExpression t =
    case T.breakOn " " $ T.strip t of
      (a, "") -> a
      (_, rest) -> T.strip rest

isHaskellPost :: T.Text -> Status -> Bool
isHaskellPost userName status =
    T.isPrefixOf userName $ status ^. statusText

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

{-evalExpression :: MonadIO m => Status -> m ()-}
evalExpression :: MonadIO m => Status -> m String
evalExpression status = do
    r <- liftIO $ evalExpr $ T.unpack $ getHaskellExpression $ status ^. statusText
    {-return $ (show $ status ^. statusText ) ++ ": " ++ r-}
    return $ take 140 r

-- res <- call $ update "Hello World"
reply :: Integer -> T.Text -> APIRequest StatusesUpdate Status
reply i s =
  Web.Twitter.Conduit.update s & inReplyToStatusId ?~ i

--First Run:
firstrunMain :: IO ()
firstrunMain = runNoLoggingT . withCredential $ do
    liftIO . putStrLn $ "Copy the creds above into your Token.hs file."
    liftIO . putStrLn $ "Then swap out the main function in Main.hs and recompile."

nonconduitMain :: IO ()
nonconduitMain =
    let env = setCredential tokens creds def in
      runNoLoggingT . runTW env $ do
        liftIO . putStrLn $ "# your mentions timeline (up to 100 tweets):"
        mentions <- call mentionsTimeline
        let res = filter (isHaskellPost "@LambdaTwit") mentions
        liftIO . putStrLn $ "# Eval results:"
        mapM_ printExpression res
          where printExpression s = do
                  r <- evalExpression s
                  return $ putStrLn r

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
    let env = setCredential tokens creds def in
      runNoLoggingT . runTW env $ do
        {-liftIO . putStrLn $ "# your mentions timeline (up to 100 tweets):"-}
        sourceWithMaxId mentionsTimeline
             C.$= CL.isolate 100
             C.$$ CL.mapM_ $ \status -> do
                 replies <- liftIO $ query state AllReplies
                 if ((TweetId (status ^. statusId)) `elem` replies)
                   then do
                     liftIO $ putStrLn "Already replied to:"
                     liftIO $ T.putStrLn $ statusToText status
                   else do
                     liftIO $ T.putStrLn $ statusToText status
                     res <- evalExpression status
                     liftIO $ putStrLn res
                     postres <- postreply status (status ^. statusId) res
                     liftIO $ Data.Acid.update state (AddReply $ TweetId (status ^. statusId))
                     liftIO $ print postres

postreply :: (MonadResource m, MonadLogger m) => Status -> Integer -> String -> TW m Status
postreply status i res = call (reply i $ (T.take 140 $
                                 T.concat ["@",
                                          status ^. statusUser ^. userScreenName,
                                          " ",
                                          T.pack res]))


main :: IO ()
main = conduitmain
{-main = firstrunMain-}
{-main = nonconduitMain-}

