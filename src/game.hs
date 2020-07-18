{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)

width, height :: Int
width = 640
height = 480

type Pos = Point
data Player = Player {position::Pos}

initialPlayer :: Player
initialPlayer = Player (200,200)

playerSize::Float
playerSize = 20

withWindow :: Int -> Int -> String -> (GLFW.Window ->IO()) -> IO ()
withWindow windowWidth windowHeight title gameLoop = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <-GLFW.createWindow windowWidth windowHeight title Nothing Nothing
    case m of
      Nothing    -> return ()
      (Just win) -> do
                    GLFW.makeContextCurrent m
                    gameLoop win
                    GLFW.setErrorCallback $ Just simpleErrorCallback
                    GLFW.destroyWindow win
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False

outsideOfLimits :: (Float, Float) ->Float->Bool
--outsideOfLimits (xmon,ymon) size = False
outsideOfLimits (xmon,ymon) size = xmon > w/2 - size/2 ||
                                   xmon < -w/2 + size/2 ||
                                   ymon > h/2 - size/2 ||
                                   ymon < -h/2 + size/2 
   where
     w = fromIntegral width
     h = fromIntegral height
 
move :: (Bool,Bool,Bool,Bool) -> Player -> Float -> Player
move (True,    _,    _,    _) (Player (xpos,ypos)) increment = Player ((xpos-increment), ypos)
move (   _, True,    _,    _) (Player (xpos,ypos)) increment = Player ((xpos+increment),ypos)
move (   _,    _, True,    _) (Player (xpos,ypos)) increment = Player (xpos,(ypos+increment))
move (   _,    _,    _, True) (Player (xpos,ypos)) increment = Player (xpos,(ypos-increment))
-- move (False,False,False,False) (Player (xpos,ypos)) _ = Player (xpos,ypos) -- identity
move (False,False,False,False) player _ = player

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
movePlayer direction player increment 
  | outsideOfLimits (position (move direction player increment)) playerSize = player
  | otherwise = move direction player increment

renderFrame (Player (xpos,ypos)) window glossState = do
  displayPicture (width, height) white glossState 1.0 $ translate xpos ypos $ rectangleSolid playerSize playerSize 
  swapBuffers window

main :: IO()
main = do
  glossState <- initState
  withWindow width height "Game" $ \win -> do
      loop win initialPlayer glossState
      exitSuccess
      where
        loop window state glossState = do
          threadDelay 2000
          pollEvents
          k <- keyIsPressed window Key'Escape
          l <- keyIsPressed window Key'Left
          r <- keyIsPressed window Key'Right
          u <- keyIsPressed window Key'Up
          d <- keyIsPressed window Key'Down
--          putStrLn $ show (l,r,u,d)
          let newState = movePlayer (l,r,u,d) state 5
--          putStrLn $ show $ position newState
          renderFrame newState window glossState
          unless k $ loop window newState glossState

        
    
