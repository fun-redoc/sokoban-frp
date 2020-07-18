{-# LANGUAGE PackageImports, RecursiveDo #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless, join)
import Control.Monad.Fix (fix)
import Control.Applicative ((<*>),(<$>))
import System.Random
import Foreign.C.Types (CDouble(..))
import FRP.Elerea.Simple

width, height :: Int
width = 640
height = 480

type Pos                = Point
data Player             = Player {position::Pos}
data Monster            = Monster Pos MonsterStatus deriving Show
data MonsterStatus      = Wander Direction Int | Hunting deriving Show
data Direction          = WalkUp | WalkDown | WalkLeft | WalkRight deriving (Show, Enum, Bounded)

instance Random Direction where
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of (x,g')->(toEnum x, g')
  random        g = randomR (minBound, maxBound) g

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
outsideOfLimits (xmon,ymon) size = xmon > w/2 - size/2 ||
                                   xmon < -w/2 + size/2 ||
                                   ymon > h/2 - size/2 ||
                                   ymon < -h/2 + size/2
   where
     w = fromIntegral width
     h = fromIntegral height

move :: (Bool,Bool,Bool,Bool) -> Player -> Float -> Player
move (l,r,u,d) (Player(xpos,ypos)) increment =
  Player (xnew,ynew) where
    xnew = if l then xpos-increment else if r then xpos+increment else xpos
    ynew = if u then ypos+increment else if d then ypos-increment else ypos

movePlayer :: (Bool, Bool, Bool, Bool) -> Player -> Float -> Player
movePlayer direction player increment
  | outsideOfLimits (position (move direction player increment)) playerSize = player
  | otherwise = move direction player increment

renderFrame window glossState (Player (xpos,ypos)) = do
  displayPicture (width, height) white glossState 1.0 $ translate xpos ypos $ rectangleSolid playerSize playerSize
  swapBuffers window

readInput window directionAction = do
  pollEvents
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  directionAction (l,r,u,d)

main :: IO()
main = do
  (directionSignal, directionSignalSink) <- external (False,False,False,False)
  glossState <- initState
  withWindow width height "Game" $ \win -> do
    -- initGL width height
    randomGenerator<-newStdGen
    network <- start $ do
                          player <- transfer initialPlayer (\direction player->movePlayer direction player 5) directionSignal
                          -- randomNumber <- stateful (undefined, randomGenerator) (\(a,g)->(random g))
                          return $ fmap (renderFrame win glossState) player
                          return $ renderFrame win glossState <$> player
    fix $ \loop -> do
            readInput win directionSignalSink
            join network
            threadDelay 2000
            esc <- keyIsPressed win Key'Escape
            unless (esc) loop
    exitSuccess
