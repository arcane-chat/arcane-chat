module Main where
  import Prelude
  import Control.Monad.Eff
  import Control.Monad.Eff.Console

  foreign import data GUI :: !

  type ConsoleEff = forall e. Eff (console :: CONSOLE | e) Unit

  foreign import uiApp :: (Int -> ConsoleEff) -> forall e. Eff (gui :: GUI | e) Unit

  foreign import cos :: Number -> Number

  angle :: Number
  angle = 1.0466

  logUserSelection :: Int -> ConsoleEff
  logUserSelection code = do
    log $ "foo" <> show code

  main = do
    uiApp logUserSelection
    