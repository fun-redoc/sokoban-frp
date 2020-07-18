module Sokoban2 ( fileLevelReader
                 ,Dir(NoMove,MoveRight,MoveLeft,MoveUp,MoveDown)
                 ,gameFieldFromChars
                 ,GameField(gameField'Walls,gameField'Storage,gameField'Crates,gameField'Man)
                 ,isWon
                 ,moveMan
                 ,charsFromGameField
                 ,printGameField) where
import Debug.Trace
import Data.List

allIn::(Eq a)=>[a]->[a]->Bool
-- allIn [] [] = True
allIn [] _ = True
-- allIn (v:[]) (x:xs) = (v `elem` xs)
allIn (v:vs) xs = (v `elem` xs) && (vs `allIn` xs)

add :: (Num t, Num t1) => (t, t1) -> (t, t1) -> (t, t1)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

smul :: (Num t) => t -> (t, t) -> (t, t)
smul n (x2,y2) = (n*x2,n*y2)


splitOnEmptyString::[String]->[[String]]
splitOnEmptyString xs = splitOnEmptyString' xs [] []

splitOnEmptyString'::[String]->[String]->[[String]]->[[String]]
splitOnEmptyString' [] a b = b++[a]
splitOnEmptyString' (c:cs) a b
  | c == "" = splitOnEmptyString' cs [] (if a==[] then b else (b++[a]))
  | otherwise = splitOnEmptyString' cs (a++[c]) b

type Field2d a = [[a]]

testLevelChars:: Field2d Char
testLevelChars =  [ "#######",
                 "## * @#",
                 "#*. o #",
                 "#######"]

levelsChars :: Field2d Char
levelsChars =  [ "#######",
                 "## * @#",
                 "#*. o #",
                 "#######",
                 "",
                 "#######",
                 "## *o@#",
                 "#*.   #",
                 "#######",
                 "",
                 "#######", -- finished level
                 "## **@#",
                 "#**   #",
                 "#######"]


type Pos = (Int,Int)
type DirVec = (Int,Int)
stop,goRight,goLeft,goUp,goDown::(Int,Int)
stop = (0,0)
goRight = (1,0)
goLeft = (-1,0)
goUp = (0,-1)
goDown = (0,1)

data GameField = GameField { gameField'Walls::[Pos]
                            ,gameField'Storage::[Pos]
                            ,gameField'Crates::[Pos]
                            ,gameField'Man::(Maybe Pos)
} deriving (Eq, Show)

data Dir = NoMove | MoveRight | MoveLeft | MoveUp | MoveDown deriving (Show, Eq)


sortPos::[Pos]->[Pos]
sortPos = sortBy (\(x1,y1) (x2,y2)->if x1 `compare` x2 == EQ then y1 `compare` y2 else x1 `compare`x2)

initGameField::GameField
initGameField = GameField [] [] [] Nothing

isStorage::GameField->Pos->Bool
isStorage gf pos = elem pos (gameField'Storage gf)

isWall::GameField->Pos->Bool
isWall gf pos = elem pos (gameField'Walls gf)

isWon :: GameField -> Bool
isWon gf = ((gameField'Crates gf) `allIn` (gameField'Storage gf))

moveTransitive :: (Int, Int) -> (Int, Int) -> [Pos] -> GameField -> (Bool, [Pos])
moveTransitive pos dir ps gf = if null adjacentAndTransitive then (numAdjacent == 0,ps) else (True, map (\p->if p `elem` adjacentAndTransitive then p `add` dir else p) ps)
  where
  (numAdjacent, adjacentAndTransitive) = findAdjacentAndTransitive' (0,[])
    where
    findAdjacentAndTransitive' (n,res) = if isWall gf pos' then (n,[]) else if idx == Nothing then (n,res) else findAdjacentAndTransitive' (n+1,(pos':res))
      where
      pos' = pos `add` (n `smul` dir)
      idx = pos' `elemIndex` ps

moveMultipleCrates :: (Int, Int) -> (Int, Int) -> GameField -> (Bool, [Pos])
moveMultipleCrates (sx,sy) (dx,dy) gf = moveTransitive (sx,sy) (dx,dy) (gameField'Crates gf) gf

-- testMoveMan0 = charsFromGameField $ moveMan' stop $  gameFieldFromChars levelsChars 2
-- testMoveMan1 = charsFromGameField $ moveMan' goLeft $  gameFieldFromChars levelsChars 2
-- testMoveMan2 = charsFromGameField $ moveMan' goLeft $ moveMan' goLeft $  gameFieldFromChars levelsChars 2
-- testMoveMan3 = charsFromGameField $ moveMan' goRight $ moveMan' goLeft $ moveMan' goLeft $  gameFieldFromChars levelsChars 2
-- testMoveMan4 = charsFromGameField $ moveMan' goDown $ moveMan' goRight $ moveMan' goLeft $ moveMan' goLeft $  gameFieldFromChars levelsChars 2
-- testMoveMan5 = charsFromGameField $ moveMan' goUp $ moveMan' goDown $ moveMan' goRight $ moveMan' goLeft $ moveMan' goLeft $  gameFieldFromChars levelsChars 2

moveMan::Dir->GameField->GameField
moveMan NoMove gf = gf
moveMan MoveRight gf = moveMan' goRight gf
moveMan MoveLeft gf = moveMan' goLeft gf
moveMan MoveUp gf = moveMan' goUp gf
moveMan MoveDown gf = moveMan' goDown gf

moveMan'::DirVec->GameField->GameField
moveMan' (dx,dy) gf = result
  where
  manMovedPos = (\oldPos dir->let newPos = add oldPos dir in if isWall gf newPos then oldPos else newPos) <$> (gameField'Man gf) <*> (pure (dx,dy))
  (moveSuccess,cratesMoved) = maybeMoveMultipeCrates manMovedPos
  result = if moveSuccess then gf{gameField'Man=manMovedPos, gameField'Crates=cratesMoved} else gf
  maybeMoveMultipeCrates Nothing = (False, [])
  maybeMoveMultipeCrates (Just startPos) = moveMultipleCrates startPos (dx,dy) gf

charsFromGameField:: GameField -> [[Char]]
charsFromGameField gf = manLayer (gameField'Man gf)
  where
    manLayer Nothing = crateLayer
    manLayer (Just pos) = updateField (if isStorage gf pos then '+' else '@') pos crateLayer
    crateLayer = foldl (\field (x,y)->updateField (if isStorage gf (x,y) then '*' else 'o') (x,y) field) storageLayer (gameField'Crates gf)
    storageLayer = foldl (\field (x,y)->updateField '.' (x,y) field) wallLayer (gameField'Storage gf)
    wallLayer = foldl (\field (x,y)->updateField '#' (x,y) field) (emptyField ' ') (gameField'Walls gf)
    emptyField c = replicate height $ replicate width c
    (width, height) = (\(a,b)->(a+1,b+1)) $  foldl (\(ax,ay) (x,y)->(max ax x,max ay y)) (0,0) (gameField'Walls gf)
    updateField::a->Pos->[[a]]->[[a]]
    updateField val (x,y) fld = updateList (updateList val x (fld!!y)) y fld
    updateList val i ls = (take i ls )++[val]++(drop (i+1) ls)

gameFieldFromChars::Int->[[Char]]->GameField
gameFieldFromChars i css = fromRows (0::Int) (fromLevelChars css i) initGameField
  where
  fromLevelChars css' i' = snd $ foldl (\(ai,alc) cs->(if cs=="" then ai+1 else ai,if ai==i' then alc++[cs] else alc)) (0,[]) css'
  fromRows _ [] gf = gf
  fromRows y (r:rs) gf = fromRows (y+1) rs $ fromCols 0 y r gf
    where
    fromCols _ _ [] gf' = gf'
    fromCols x' y' (c:cs) gf' = fromCols (x'+1) y' cs $ fromCell x' y' c gf'
      where
      fromCell x'' y'' '#' gf'' = gf''{gameField'Walls=(x'',y''):(gameField'Walls gf'')}
      fromCell x'' y'' '.' gf'' = gf''{gameField'Storage=(x'',y''):(gameField'Storage gf'')}
      fromCell _   _   ' '  gf'' = gf'' -- Floor
      fromCell x'' y'' 'o' gf'' = gf''{gameField'Crates=(x'',y''):(gameField'Crates gf'')}
      fromCell x'' y'' '*' gf'' = gf''{gameField'Storage=(x'',y''):(gameField'Storage gf''),gameField'Crates=(x'',y''):(gameField'Crates gf'')}
      fromCell x'' y'' '@' gf'' = gf''{gameField'Man=Just (x'',y'')}
      fromCell x'' y'' '+' gf'' = gf''{gameField'Storage=(x'',y''):(gameField'Storage gf''),gameField'Man=Just (x'',y'')}
      fromCell _ _ _  _ = undefined

sortCrates::GameField->GameField
sortCrates gf@GameField{gameField'Crates=crates} = trace "sort crates" $ gf{gameField'Crates=sortPos crates}

fileLevelReader ::  Int -> IO (GameField)
fileLevelReader n = (fmap (gameFieldFromChars 0) (fileLevelReaderChars' n))
  where
      fileLevelReaderChars'::Int->IO [[Char]]
      fileLevelReaderChars' n' =
            readFile "sokoban_levels.txt" >>= (\t -> return $ splitOnEmptyString $ lines t) >>= (\xs->return (xs!!n'))


printGameField ::  GameField  -> IO ()
printGameField = print' . charsFromGameField  where
  print' (c:cs) = do putStrLn ("|"++c++"|")
                     print' cs
  print' [] = return ()
