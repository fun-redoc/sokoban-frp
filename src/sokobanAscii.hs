{-# LANGUAGE PackageImports, RecursiveDo #-}
import Data.Maybe
import Data.Functor
import Control.Applicative
import Data.List
import Data.Char (toUpper,chr)
import Control.Monad (forever,when,unless,join)
import Control.Monad.Fix (fix)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (State,get,put)
import System.Exit (exitSuccess)
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin, stdout, hGetChar, hPutChar, fixIO )
{-import System.Console.ANSI-}
{-import Reactive.Banana-}
{-import Reactive.Banana.Frameworks.AddHandler-}
{-import Reactive.Banana.Frameworks-}

import Sokoban

clear = putStr "\ESC[2J"


keyToDir::Char->Dir
keyToDir c = case c of
              'h' -> moveLeft
              'l' -> moveRight
              'j' -> moveDown
              'k' -> moveUp
              _ -> moveStop

gameLoop ::  (IO Game -> IO ()) -> IO Game -> IO ()
gameLoop loop ioGame = do
              game<-ioGame
              let level = levelData game
              let finishLevel = checkLevel level
              let myLevelReader = levelReader game
              clear
              putStrLn ((show $ levelNumber game) ++ (show finishLevel))
              printLevel level
              when (finishLevel) (putStrLn $"WON Level "++(show $ levelNumber game)++" - press any key to continue")
              c<-hGetChar stdin
              let newGame = if finishLevel then do
                                                    nextLevel<-myLevelReader $ (levelNumber game) + 1
                                                    return $ game{levelData=nextLevel, levelNumber=((levelNumber game)+1)}
                                           else case c of
                                                  'q' -> return game
                                                  'r' -> do
                                                            repeatLevel<-myLevelReader $ levelNumber game
                                                            return $ game{levelData=repeatLevel}
                                                  _   -> return game{levelData=(moveMan (keyToDir c) level)}
              when (not ( c == 'q')) (loop $ newGame)

gameLoopState::(IO (State Game Bool) -> IO()) -> (IO (State Game Bool)) -> IO ()
gameLoopState = undefined -- TODO game loop with state?? maybe better

main ::  IO b
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  {-let game = initGame inlineLevelReader-}
  let game = initGame fileLevelReader
  initialLevel<-levelReader game 0
  fix gameLoop $ (return $ game{levelData=initialLevel})
  exitSuccess
