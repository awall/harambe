{-# LANGUAGE ScopedTypeVariables #-}

module Harambe.System(
    module Harambe.System.Event,
    module Harambe.System.Graphics,
    eventLoop,
    quit,
)
where

import Harambe.System.Event
import Harambe.System.Graphics
import Harambe.System.Private.Picture

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Exit(exitSuccess, exitFailure, ExitCode)
import Control.Monad(liftM)
import Control.Exception(catch, throwIO, throwTo, Exception, SomeException, evaluate)
import Control.Concurrent(myThreadId, ThreadId)

data Quit = Quit deriving (Show)
instance Exception Quit
quit :: IO a
quit = throwIO Quit >>= evaluate

eventLoop :: String -> a -> (a -> IO Picture) -> (IO () -> Event -> a -> IO a) -> (Float -> a -> IO a) -> IO ()
eventLoop title state render react advance = do
  thread <- myThreadId
  let quit = throwTo thread Quit >>= evaluate
  let react' e a =
        --catch
          (react quit (eventFromGloss e) a >>= evaluate)
          --(\(ex :: SomeException) -> quit)
  catch
    (G.playIO display G.black fps state render' react' advance')
    (\(ex :: SomeException) -> putStrLn "caught here")
  where
    fps = 60
    display = G.InWindow title (500, 500) (100, 100)
    
    render' = liftM pictureToGloss . render
    advance' s a = advance s a >>= evaluate
    pictureToGloss (GlossPicture p) = p

eventFromGloss :: G.Event -> Event
eventFromGloss (G.EventKey (G.Char c) _ _ _) | c >= 'a' && c <= 'z' =
  EventKey $ toEnum $ (fromEnum KeyA) + (fromEnum c - fromEnum 'a')

eventFromGloss (G.EventKey _ _ _ _) =
  EventKey KeyOther

eventFromGloss _ =
  EventOther