module Taskwarrior.IO
  ( getTasks
  , saveTasks
  )
where

import           Taskwarrior.Task               ( Task )
import           Data.Text                      ( Text )
import           Control.Exception              ( throw )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Lazy                as LText
import qualified Data.Text.Lazy.Encoding       as LText
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Aeson                    as Aeson
import           System.Process                 ( readProcess
                                                , withCreateProcess
                                                , CreateProcess(..)
                                                , proc
                                                , StdStream(..)
                                                , waitForProcess
                                                )
import           Control.Monad                  ( void )
import           Control.Concurrent             ( threadDelay )
import           System.IO                      ( hClose )
import           System.Exit                    ( ExitCode(..) )

getTasks :: [Text] -> IO [Task]
getTasks args =
  withCreateProcess
      ((proc "task" (fmap Text.unpack . (++ ["export"]) $ args))
        { std_out = CreatePipe
        }
      )
    $ \_ stdout _ _ -> do
        stdout <- maybe
          (fail "Couldn‘t create stdout handle for `task export`")
          pure
          stdout
        input <- LBS.hGetContents stdout
        either fail return . Aeson.eitherDecode $ input

saveTasks :: [Task] -> IO ()
saveTasks tasks =
  withCreateProcess
      ((proc "task" ["import"]) { std_in = CreatePipe })
    $ \stdin _ _ process -> do
        stdin <- maybe (fail "Couldn‘t create stdin handle for `task import`")
                       pure
                       stdin
        LBS.hPut stdin . Aeson.encode $ tasks
        hClose stdin
        exitCode <- waitForProcess process
        if exitCode /= ExitSuccess then fail $ show exitCode else return ()
