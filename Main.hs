{-# LANGUAGE OverloadedStrings,FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo,RecordWildCards,ScopedTypeVariables #-}

module Main where

import Data.Maybe
import Text.URI
import Control.Monad.Except
import Data.Time.Clock
import Text.Feed.Query
import Text.Feed.Types
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified System.AtomicWrite.Writer.LazyByteString as ABSL
import System.IO
import Network.HTTP.Req
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory
import System.FilePath.Posix ((</>))
import Control.Exception
import qualified Data.Map as Map
import System.IO.Error
import System.Exit
import Network.Mail.Mime (Mail)

import ReadFeed
import FeedState
import FeedConfig
import Utils
import AppMonad
import Email
import MailJSON

data GlobalArguments = GlobalArguments
  { configPath :: FilePath
  , statePath :: FilePath
  }

globalArgumentParser :: FilePath -> Parser GlobalArguments
globalArgumentParser defaultDir = do
  configPath <- strOption ( long "config" <> short 'C' <> value (defaultDir </> "config.json") )
  statePath <- strOption ( long "state" <> short 'S' <> value (defaultDir </> "state.json") )
  pure GlobalArguments {..}

runCommand :: Parser (GlobalArguments -> AppMonad ())
runCommand = pure $ mainRun

bulkSendEmail :: Config -> [Mail] -> AppMonad [Mail]
bulkSendEmail config mailList = do
  leftoverMail <- forM mailList $ \mail -> do
    catchError (sendEmail config mail >> return Nothing) $ \error -> do
      liftIO $ hPutStrLn stderr error
      return $ Just mail
  return $ catMaybes leftoverMail

mainRun :: GlobalArguments -> AppMonad ()
mainRun arguments = do
  config <- loadConfig $ configPath arguments
  state <- loadState $ statePath arguments

  -- Try to drain the outbox from last time.  Maybe we'll have better
  -- luck this time.
  let unsentMail = fmap unMailJSON $ outbox state
  leftoverMail <- fmap MailJSON <$> bulkSendEmail config unsentMail

  results <- forM (Map.toAscList $ configFeedConfigs config) $ \(key, feedConfig) -> do
    let feedState = fromMaybe emptyFeedState $ Map.lookup key (feedStates state)
    (newItems, newState) <- catchError (getNew feedConfig feedState) $ \error -> do
      liftIO $ hPutStrLn stderr error
      return ([], feedState)
    return ((key, newState), map (\item -> (key, feedConfig, item)) newItems)

  let newStateMap = Map.fromAscList $ map fst results
  let newItems = concat $ map snd results

  now <- liftIO getCurrentTime
  let newMail = mapMaybe (\(key, feedConfig, item) -> mailForItem config now key feedConfig item) newItems

  -- Journal the mails we intend to send so we can recover from a
  -- crash
  let newState = state
        { feedStates = newStateMap
        , outbox = (map MailJSON newMail) ++ leftoverMail
        }
  liftIO $ ABSL.atomicWriteFile (statePath arguments) $ serializeState newState

  failedMail <- fmap MailJSON <$> bulkSendEmail config newMail

  -- Now write out the state containing just the mail we failed to
  -- send!
  liftIO $ ABSL.atomicWriteFile (statePath arguments) $ serializeState newState { outbox = failedMail ++ leftoverMail }

initCommand :: Parser (GlobalArguments -> AppMonad ())
initCommand = do
  force <- flag False True ( long "overwrite" )
  pure $ mainInit force

mainInit :: Bool -> GlobalArguments -> AppMonad ()
mainInit overwrite arguments = do
  -- Ugh.  There's no easy way to do the equivalent of O_CREAT |
  -- O_EXCL.  So, let's just check if it exists before we write.
  when (not overwrite) $ do
    configExists <- liftIO $ doesFileExist $ configPath arguments
    when configExists $ throwError $ (configPath arguments) ++ " already exists"
    stateExists <- liftIO $ doesFileExist $ statePath arguments
    when configExists $ throwError $ (statePath arguments) ++ " already exists"
  liftIO $ ABSL.atomicWriteFile (configPath arguments) $ serializeConfig exampleConfig
  liftIO $ ABSL.atomicWriteFile (statePath arguments) $ serializeState emptyState
  liftIO $ putStrLn $ "Awesome!  Now go edit " ++ configPath arguments ++ " and add some feeds!"

allCommands :: Parser (GlobalArguments -> AppMonad ())
allCommands = hsubparser $
  ( command "run" $ info runCommand (progDesc "Run it!") )
  <> ( command "init" $ info initCommand (progDesc "Create file to get you started") )

topParser :: FilePath -> Parser (AppMonad ())
topParser home = do
  arguments <- globalArgumentParser home
  verb <- allCommands
  pure $ verb arguments

loadConfig :: (MonadIO m, MonadError String m) => FilePath -> m Config
loadConfig path = do
  let reader = withFile path ReadMode $ \h -> BSS.hGetContents h >>= return . Right
  let exceptionChecker e = if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing
  let catcher _ = return . Left $ "oh no, " ++ path ++ " doesn't exist"
  eitherByteString <- liftIO $ catchJust exceptionChecker reader catcher
  byteString <- liftEither eitherByteString
  liftEither $ parseConfigS byteString

loadState :: (MonadIO m, MonadError String m) => FilePath -> m State
loadState path = do
  let reader = withFile path ReadMode $ \h -> BSS.hGetContents h >>= return . Right . Just
  let exceptionChecker e = if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing
  let catcher _ = return . Right $ Nothing
  eitherByteString <- liftIO $ catchJust exceptionChecker reader catcher
  maybeByteString <- liftEither eitherByteString
  case maybeByteString of
    Just bs -> liftEither $ parseStateS bs
    Nothing -> return emptyState

maxAge :: NominalDiffTime
maxAge = nominalDay * (-14)

getNew :: (MonadIO m, MonadError String m, MonadHttp m) => FeedConfig -> FeedState -> m ([Item], FeedState)
getNew feedConfig oldState = do
  feed <- fetchFromURL . fromJust . mkURI . feedURL $ feedConfig
  now <- liftIO getCurrentTime
  let newItems = unseenItems oldState feed
  let newState = mkFeedStateFromFeed now feed
  let mergedState = mergeFeedStates newState oldState
  let cleanedState = ageOutEntriesFromFeedState (addUTCTime maxAge now) mergedState
  return (newItems, cleanedState)

mainErrorHandler :: String -> IO a
mainErrorHandler str = do
  hPutStrLn stderr str
  exitWith $ ExitFailure 1

main :: IO ()
main = do
  e <- runAppMonad main_
  either mainErrorHandler (const $ return ()) e

main_ :: AppMonad ()
main_ = do
  home <- liftIO $ getXdgDirectory XdgConfig "rss4email"
  let argParser = topParser home
  join $ liftIO $ execParser $ info (argParser <**> helper) idm
