{-# LANGUAGE PackageImports, RecursiveDo #-}
-- TODO
-- OK smooth movement is still not perfect, crates move broken
-- OK undo fails when smooth movement is enabled
-- smooth movement sometimes leaks
-- window resize not implemented
-- key repeat would be good

import Debug.Trace

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss
import Graphics.Gloss.Rendering as G
import System.Exit ( exitSuccess )
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Fix (fix)
-- import Control.Applicative ((<$>),(<*>))
import FRP.Elerea.Simple
import Data.Maybe
import Sokoban2

type Pos = (Int,Int)
-- type Vec2 = (Float, Float)

data Textures = Textures { textures'Crate::Picture, textures'CrateOnStorage::Picture, textures'Wall::Picture }
data Input = Direction Dir | LevelUp | JumpToNextLevel | Undo | RestartLevel | NoInput deriving (Show,Eq)
data GameState = Start | Playing | LevelWon | GameOver deriving (Show, Eq)
data Game = Game{game'Field::GameField, game'Level::Int, game'State::GameState, game'Last::(Maybe GameField), game'History::[Maybe GameField]} deriving (Eq, Show)
data AnimationState = NoAnimation | Animate Int  deriving (Show, Eq)
data VisualGame = VisualGame{visualGame'AnimationState::AnimationState, visualGame'Walls::[(Float,Float)], visualGame'Storage::[(Float,Float)], visualGame'Crates::[(Float,Float)], visualGame'Man::(Maybe (Float,Float))}

isNear::(Float,Float)->(Float,Float)->Bool
isNear (x1,y1) (x2,y2) = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2) < 1.00

simulationSteps::Int -- should be power of 2
simulationSteps = 8

width, height :: Int
width = 1000
height = 700

objectWidth, objectHeight::Float
objectWidth = 50.0
objectHeight = 50.0

-- input
keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = False
isPress KeyState'Released = False
isPress _ = False


readInput :: Window -> (Input -> IO b) -> IO b
readInput window directionSink = do
  pollEvents
  -- waitEvents -- TODO waitEvents is bad, seek for a possibility to replace by pollEvents
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Up
  d <- keyIsPressed window Key'Down
  s <- keyIsPressed window Key'S
  restart <- keyIsPressed window Key'R
  n <- keyIsPressed window Key'N
  undo <- keyIsPressed window Key'U
  directionSink $ if restart then RestartLevel else if n then JumpToNextLevel else if s then LevelUp else if undo then Undo else Direction $ dir l r u d
    where
      dir l r u d = foldl (\acc (f, move)->if f then move else acc) NoMove $  zipWith (\f move->(f,move)) [l,r,u,d] [MoveLeft, MoveRight, MoveUp, MoveDown]

-- view
screenCoords::Pos->(Float,Float)
screenCoords (x,y) =  ((((fromIntegral x)*objectWidth)+(objectWidth/2)-w'''), ((h'''-(fromIntegral y)*objectHeight)-(objectHeight/2)))
  where
  w''',h'''::Float
  w''' = ((fromIntegral width)/2)
  h''' = ((fromIntegral height)/2)

renderField :: Textures -> VisualGame -> [Picture]
renderField textures vg  =
  wallPictures++storagePictures++cratePictures++manPicture
    where
    wallPictures = map (\(x,y)->(Color black $ (translate'  (x,y)) $ (textures'Wall textures))) (visualGame'Walls vg)
    storagePictures = map (\(x,y)->(Color yellow $ (translate'  (x,y)) $ rectangleSolid objectWidth objectHeight)) (visualGame'Storage vg)
    cratePictures = map (\(x,y)->(Color blue $ (translate'  (x,y)) $ (textures'Crate textures))) (visualGame'Crates vg)
    manPicture = if isJust man then [(Color magenta $ (translate'  $ fromJust man) $ rectangleSolid objectWidth objectHeight)] else []
    man = visualGame'Man vg
    translate' (x,y) = translate x y

renderGame :: Textures -> Window -> State -> GameState -> VisualGame -> IO ()
renderGame _ window glossState Start _ = do
  displayPicture (width, height) white glossState 1.0 $
     Pictures [Color (bright magenta) $ translate (-100) (-100) $ scale 0.2 0.2 $ text "Hallo Sokoban"]
  swapBuffers window
renderGame textures window glossState LevelWon vg= do
  displayPicture (width, height) white glossState 1.0 $
     Pictures $ (renderField textures vg ) ++ [Color (bright magenta) $ translate (-(fromIntegral width/3)) 0 $ scale 0.3 0.3 $ text $ "Finished Level "{-++(show $ game'Level game)-}]
  swapBuffers window
renderGame textures window glossState _ vg = do
 displayPicture (width, height) white glossState 1.0 $ Pictures $ renderField textures vg
 swapBuffers window

moveSmoothly::Int->(Float,Float)->(Float,Float)->(Float,Float)->(Float,Float)
moveSmoothly animationStep (ox,oy) (nx,ny) (vx,vy) = (vx+(nx-ox)/(fromIntegral simulationSteps),vy+(ny-oy)/(fromIntegral simulationSteps))

moveSharp::(Float,Float)->(Float,Float)->(Float,Float)->(Float,Float)
moveSharp  _ n _ = n

updateVisual::Input->Game->VisualGame->VisualGame
updateVisual Undo Game{game'Field=gf} vg = vg{ visualGame'AnimationState=NoAnimation
                                                ,visualGame'Crates = map screenCoords (gameField'Crates gf)
                                                ,visualGame'Man = fmap screenCoords (gameField'Man gf)
                                                }
updateVisual _ Game{game'Field=gf, game'Last=Nothing} _ = VisualGame{ visualGame'AnimationState=NoAnimation
                                                ,visualGame'Walls = map screenCoords (gameField'Walls gf)
                                                ,visualGame'Storage = map screenCoords (gameField'Storage gf)
                                                ,visualGame'Crates = map screenCoords (gameField'Crates gf)
                                                ,visualGame'Man = fmap screenCoords (gameField'Man gf)
                                                }
updateVisual (Direction NoMove) _ vg  =  vg
updateVisual (Direction _) Game{game'Field=gf, game'Last=Just lastGf} vg@VisualGame{visualGame'AnimationState=NoAnimation} =
                                              vg{visualGame'AnimationState=Animate 0
                                               }
updateVisual _ Game{game'Field=gf, game'Last=Just lastGf} vg@VisualGame{visualGame'AnimationState=Animate animationStep} =
  if animationStep < simulationSteps
    then
      vg{visualGame'AnimationState= Animate (animationStep+1)
        ,visualGame'Crates = zipWith3 (moveSmoothly animationStep) (map screenCoords (gameField'Crates lastGf)) (map screenCoords (gameField'Crates gf)) (visualGame'Crates vg)
        ,visualGame'Man    = (moveSmoothly animationStep)  <$> (screenCoords <$> (gameField'Man lastGf)) <*> (screenCoords <$> (gameField'Man gf)) <*> (visualGame'Man vg)
        }
    else
      vg{visualGame'AnimationState=NoAnimation
        ,visualGame'Crates = map screenCoords (gameField'Crates gf)
        ,visualGame'Man = fmap screenCoords (gameField'Man gf)
        }

updateVisual _ _ vg@VisualGame{visualGame'AnimationState=NoAnimation} = vg
updateVisual _ _ _  = undefined


-- update
undoOneMove::Game->Game
undoOneMove g@Game{game'Last=Nothing} =  g
undoOneMove g@Game{game'Field=current, game'Last=(Just last), game'History=(h:hs)} = {-trace ("current = "++(show g)++ " last = "++(show last)) $-} g{game'Field=last, game'Last=h, game'History=hs}
undoOneMove g = g

updateGameState::Input->Game->Game->Game
updateGameState LevelUp newGame@Game{game'Field=newLevelData} game@Game{game'State=Start} = newGame{game'State=Playing}
updateGameState (Direction NoMove)  _ game = game
updateGameState (Direction dir)  _ game@Game{game'Field=myLevelData,game'State=Playing, game'Last=last, game'History=hs} =
  if isWon myLevelData
    then game{game'State=LevelWon}
    else game{game'Field = moveMan dir myLevelData, game'Last=Just myLevelData, game'History=(last:hs)}
updateGameState LevelUp newGame@Game{game'Field=newLevelData} game@Game{game'State=LevelWon} = newGame{game'State=Playing}
updateGameState JumpToNextLevel newGame@Game{game'Field=newLevelData} _ = newGame
updateGameState RestartLevel newGame@Game{game'Field=newLevelData} _ = newGame{game'State=Playing}
updateGameState Undo _ game =  undoOneMove game
updateGameState  _ _ g = g

-- boilerplate
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

main :: IO()
main = do
-- read textures
  crateTexture <- trace "LOADING.." $ loadBMP "assets/crate1_diffuse_50x50.bmp"
  crateOnStorageTexture <- trace "LOADING.." $ loadBMP "assets/crate2_diffuse_50x50.bmp"
  wallTexture <- trace "LOADING.." $ loadBMP "assets/wall_50x50.bmp"
  traceIO "Loaded textures"
  let textures = Textures { textures'Crate=crateTexture, textures'CrateOnStorage=crateOnStorageTexture, textures'Wall=wallTexture }

-- initialise
  let currentLevel = 0
  initialLevel<-fileLevelReader currentLevel
  let initialGame = Game initialLevel currentLevel Start Nothing []

-- initialise signals
  (directionSignal, directionSink) <- external LevelUp
  (gameLevelSignal, gameLevelSink) <- external initialGame
  network <- start $ mdo
                          input<-transfer2 NoInput (\new old cum->if new==old then NoInput else new) directionSignal input' -- avoid repeating same signal unwillingly
                          currentGameState<-transfer2 initialGame  updateGameState input  gameLevelSignal
                          visual<- transfer2 VisualGame{visualGame'AnimationState=NoAnimation} updateVisual input currentGameState
                          input'<-delay NoInput directionSignal
                          return ((,,) <$> currentGameState <*> input <*> visual )


  glossState <- initState
  withWindow width height "Game" $ \win -> do
    -- initGL width height
-- the game Loop
    fix $ \loop -> do
            readInput win directionSink
            (currentGameState,input,visual)<-network
            -- traceIO ("-->"++(show input))
            -- traceIO $ show currentGameState
            renderGame textures win glossState (game'State currentGameState) visual
            when (((game'State currentGameState) == LevelWon) || (input == JumpToNextLevel) )  $ do
                                        let nextLevelNumber = (game'Level currentGameState) + 1
                                        nextLevel<-fileLevelReader  nextLevelNumber
                                        gameLevelSink $ Game nextLevel nextLevelNumber Start Nothing []

            threadDelay 20000
            esc <- keyIsPressed win Key'Escape
            when (not esc) loop
