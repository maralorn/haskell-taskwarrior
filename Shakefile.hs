import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions $ do
  ".travis.yml" %> \out -> do
    let config = "cabal.haskell-ci"
    let cabal  = "taskwarrior.cabal"
    need [config, cabal]
    cmd_ "haskell-ci -o" out "--config" config "travis" cabal
