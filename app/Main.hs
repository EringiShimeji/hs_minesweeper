{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Function (fix)
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC

isDebug :: Bool
isDebug = False

wWidth, wHeight :: (Num a) => a
wWidth = 640
wHeight = 480

window :: Display
window = InWindow "Minesweeper" (wWidth, wHeight) (100, 100)

main :: IO ()
main = do
  world <- generateNewWorld
  playIO window white 60 world drawWorld eventHandler stepWorld

mWidth, mHeight :: (Num a) => a
mWidth = 5
mHeight = 5

randomPosition :: GenIO -> IO Position
randomPosition gen = (,) <$> uniformR (0, mWidth - 1) gen <*> uniformR (0, mHeight - 1) gen

cSize :: (Num a) => a
cSize = fromIntegral $ if wWidth < wHeight then wWidth `div` size else wHeight `div` size
  where
    size = max mWidth mHeight

type Position = (Int, Int)

data SelectorAction = SAStop | SAUp | SADown | SALeft | SARight | SAOpen | SAFlag deriving (Eq)

moveSelector :: SelectorAction -> Position -> Position
moveSelector SAUp (x, y)
  | y + 1 < mHeight = (x, y + 1)
moveSelector SADown (x, y)
  | y - 1 >= 0 = (x, y - 1)
moveSelector SALeft (x, y)
  | x - 1 >= 0 = (x - 1, y)
moveSelector SARight (x, y)
  | x + 1 < mWidth = (x + 1, y)
moveSelector _ p = p

data GameState = InGame | GameOver | GameClear

data CellType = Mine | Empty deriving (Eq)

data CellState = CellState
  { _isOpen :: Bool,
    _isFlag :: Bool,
    _type :: CellType,
    _hint :: Int
  }

updateCell :: (a -> a) -> [[a]] -> Position -> [[a]]
updateCell f map' pos =
  [ [ if pos == (x, y) then f c else c
      | (x, c) <- zip [0 ..] $ map' !! y
    ]
    | y <- [0 ..]
  ]

updateCells :: (a -> a) -> [[a]] -> [Position] -> [[a]]
updateCells f = foldl (updateCell f)

data World = World
  { _state :: GameState,
    _map :: [[CellState]],
    _selector :: Position,
    _action :: SelectorAction,
    _using_mouse :: Bool,
    _restMinesCnt :: Int,
    _openedCnt :: Int
  }

mineAmount :: (Num a) => a
mineAmount = 5

adjacent :: Position -> [Position]
adjacent (x, y) = filter (\(x', y') -> x' >= 0 && x' < mWidth && y' >= 0 && y' < mHeight) [(x + dx, y + dy) | [dx, dy] <- sequence [[-1 .. 1], [-1 .. 1]]]

generateNewWorld :: IO World
generateNewWorld = do
  let initMap = replicate mHeight (replicate mWidth CellState {_isOpen = False, _isFlag = False, _type = Empty, _hint = 0})
  finalMap <- ($ (0, initMap)) . fix $ \loop (n, m) -> do
    if n == mineAmount
      then do
        return m
      else do
        pos <- withSystemRandom . asGenIO $ \gen -> do
          fix $ \loop' -> do
            p <- randomPosition gen
            if _type (m ! p) == Mine
              then loop'
              else pure p
        let minedMap = updateCell (\c -> c {_hint = 0, _type = Mine}) m pos
            nextMap = updateCells (\c -> c {_hint = _hint c + 1}) minedMap $ adjacent pos
        loop (n + 1, nextMap)

  pure $ World InGame finalMap (0, 0) SAStop True mineAmount $ mHeight * mWidth - mineAmount

-- like ! defined in Data.Array
(!) :: [[a]] -> Position -> a
l ! (x, y) = (l !! y) !! x

drawWorld :: World -> IO Picture
drawWorld World {..} = case _state of
  InGame ->
    pure $
      pictures
        [ pictures $ map (drawCell (greyN 0.6) . fst) $ filter (\(_, c) -> not $ _isOpen c) cellList,
          pictures $ map (drawCell blue . fst) $ filter (\(_, c) -> _isOpen c && not (_isFlag c)) cellList,
          pictures $ map (drawCell (withAlpha 0.2 red) . fst) $ filter (\(_, c) -> _isFlag c && not (_isOpen c)) cellList,
          pictures $ map (drawText . (\(pos, c) -> (show $ _hint c, pos))) $ filter (_isOpen . snd) cellList,
          -- DEBUG: show mines on debug mode
          pictures $
            if isDebug
              then
                [ pictures $ map (drawCell red . fst) $ filter (\(_, c) -> _type c == Mine) cellList,
                  pictures $ map (drawText . (\(pos, c) -> (show $ _hint c, pos))) cellList,
                  translate (-wWidth / 2 + 10) (-wHeight / 2 + 10) . scale 0.2 0.2 $ text ("REST: " ++ show _restMinesCnt)
                ]
              else [],
          -- DEBUG END
          pictures $ [drawCell green _selector | not _using_mouse]
        ]
    where
      moveToBoard = translate (-cSize * mWidth / 2) (-cSize * mHeight / 2)
      cell = moveToBoard $ polygon [(0, 0), (0, cSize), (cSize, cSize), (cSize, 0)]
      text' = moveToBoard . scale 0.5 0.5 . text
      drawCell c (x, y) = translate (fromIntegral x * cSize) (fromIntegral y * cSize) $ color c cell
      drawText (t, (x, y)) = translate (fromIntegral x * cSize) (fromIntegral y * cSize) $ text' t
      -- drawCell c (Right (x, y)) = translate (fromIntegral x * cSize) (fromIntegral y * cSize) $ color c cell
      cellList = [((x, y), _map ! (x, y)) | x <- [0 .. mWidth - 1], y <- [0 .. mHeight - 1]]
  GameOver ->
    pure $
      pictures
        [ translate (-270) 20 . scale 0.7 0.7 $ text "GAME OVER",
          -- translate (-100) (-50) . scale 0.3 0.3 $ text ("SCORE: " ++ show _score),
          translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
        ]
  GameClear ->
    pure $
      pictures
        [ translate (-270) 20 . scale 0.7 0.7 $ text "GAME Clear",
          -- translate (-100) (-50) . scale 0.3 0.3 $ text ("SCORE: " ++ show _score),
          translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
        ]

mViewLowerX, mViewLowerY, mViewUpperX, mViewUpperY :: (Fractional a) => a
mViewLowerX = -cSize * mWidth / 2
mViewLowerY = -cSize * mHeight / 2
mViewUpperX = -mViewLowerX
mViewUpperY = -mViewLowerY

toPos :: (Float, Float) -> Position
toPos (x, y) = (floor $ (x - mViewLowerX) / cSize, floor $ (y - mViewLowerY) / cSize)

eventHandler :: Event -> World -> IO World
eventHandler e w@World {..} = case _state of
  InGame -> case e of
    EventKey (SpecialKey KeyUp) Down _ _ -> pure $ w {_action = SAUp, _using_mouse = False}
    EventKey (SpecialKey KeyDown) Down _ _ -> pure $ w {_action = SADown, _using_mouse = False}
    EventKey (SpecialKey KeyLeft) Down _ _ -> pure $ w {_action = SALeft, _using_mouse = False}
    EventKey (SpecialKey KeyRight) Down _ _ -> pure $ w {_action = SARight, _using_mouse = False}
    EventKey (SpecialKey KeySpace) Down _ _ -> pure $ w {_action = SAOpen, _using_mouse = False}
    EventKey (SpecialKey _) Up _ _ -> pure $ w {_action = SAStop}
    EventKey (MouseButton LeftButton) Down _ (x, y) ->
      if x >= mViewLowerX && x <= mViewUpperX && y >= mViewLowerY && y <= mViewUpperY
        then pure $ w {_action = SAOpen, _using_mouse = True, _selector = toPos (x, y)}
        else pure w
    EventKey (Char 'f') Down _ _ -> pure $ w {_action = SAFlag, _using_mouse = False}
    EventKey (MouseButton RightButton) Down _ (x, y) ->
      if x >= mViewLowerX && x <= mViewUpperX && y >= mViewLowerY && y <= mViewUpperY
        then pure $ w {_action = SAFlag, _using_mouse = True, _selector = toPos (x, y)}
        else pure w
    _ -> pure w
  _ -> case e of
    EventKey (SpecialKey KeyEnter) Down _ _ -> generateNewWorld
    _ -> pure w

stepWorld :: Float -> World -> IO World
stepWorld _ w@World {..} = case _state of
  InGame -> do
    let pos = moveSelector _action _selector
        _c@CellState {..} = _map ! pos

    if _action == SAOpen && not _isOpen && not _isFlag
      then
        if _type == Mine
          then pure $ w {_state = GameOver}
          else
            let (nextMap, openCnt) = openAdj pos $ openCell pos
                cnt = _openedCnt - openCnt - 1
             in if cnt == 0
                  then pure $ w {_state = GameClear}
                  else pure $ w {_selector = pos, _action = SAStop, _map = nextMap, _openedCnt = cnt}
      else
        if _action == SAFlag
          then
            let cnt = _restMinesCnt + if _type == Mine then if _isFlag then 1 else -1 else 0
             in if cnt == 0
                  then pure $ w {_state = GameClear}
                  else pure $ w {_selector = pos, _action = SAStop, _map = flagCell pos, _restMinesCnt = cnt}
          else return $ w {_selector = pos, _action = SAStop}
    where
      openState c = c {_isOpen = True, _isFlag = False}
      flagState c = c {_isFlag = not $ _isFlag c}
      flagCell = updateCell flagState _map
      openCell = updateCell openState _map
      openAdj pos map' =
        if _hint (_map ! pos) == 0
          then
            let adj = filter (\p -> _type (map' ! p) == Empty && not (_isOpen (map' ! p))) $ adjacent pos
             in (updateCells openState map' adj, length adj)
          else (map', 0)
  GameOver -> pure w
  GameClear -> pure w
