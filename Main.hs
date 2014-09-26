{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad (forM_,guard,mplus,unless,when)
import           Control.Applicative
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

import           System.Exit (exitFailure)
import qualified System.IO.UTF8 as UTF (putStrLn)

import Language.Haskell.Interpreter (runInterpreter)

import Mueval.Interpreter
import Mueval.ArgsParse

import Tokens

import Control.Monad.Trans.Reader 

import Debug.Trace

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
  update s & inReplyToStatusId ?~ i

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

conduitmain :: IO ()
conduitmain =
    let env = setCredential tokens creds def in
      runNoLoggingT . runTW env $ do
        {-liftIO . putStrLn $ "# your mentions timeline (up to 100 tweets):"-}
        sourceWithMaxId mentionsTimeline
             C.$= CL.isolate 100
             C.$$ CL.mapM_ $ \status -> do
                 liftIO $ T.putStrLn $ statusToText status
                 res <- evalExpression status
                 liftIO $ putStrLn res
                 let i = status ^. statusId
                 postres <- call (reply i $ (T.take 140 $ 
                                 T.concat ["@", 
                                          status ^. statusUser ^. userScreenName,
                                          " ",
                                          T.pack res]))
                 liftIO $ print postres

main :: IO ()
main = conduitmain
{-main = firstrunMain-}
{-main = nonconduitMain-}

