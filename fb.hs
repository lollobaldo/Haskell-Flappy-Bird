module Main(main) where

import System.Random
import Debug.Trace
import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
--import Euterpea

data FbGame = FbGame{
    birdPos :: Float,
    birdVel :: Float,
    birdStatus :: Int,
    barSlit :: Float,
    barPos :: Float,
    rds :: [Float],
    score :: Int,
    dead :: Bool,
    pause :: Bool,
    gameMode :: Int} deriving Show

color_bar  = green
color_bird = red

force_up = 10
force_do = 3

fps = 60

pos_window = (100,100)

size_x_bar  = 75
size_x_bart = 95
size_y_bart = 40
size_a_bird = 18
size_a_bars = 8
size_a_slit = 200
size_x_window = 600
size_y_window = 800

speed_bar  = -200
speed_bird = 300
speed_delta = -40
speed_grav = 450

title = "Flappy Bird in Haskell!!!"

background :: Color
background = white

initialState :: [Float] -> FbGame
initialState fs = FbGame {
    birdPos=0,
    birdVel=0,
    birdStatus = 0,
    barSlit=0,
    barPos=0,
    rds=fs,
    score = 0,
    dead=False,
    pause=False,
    gameMode=0}

bg0 :: Picture
bg0 = unsafePerformIO . loadBMP $ "bg0.bmp"

bg1 :: Picture
bg1 = unsafePerformIO . loadBMP $ "bg1.bmp"

birdAlive :: Picture
birdAlive = scale 0.5 0.5 . unsafePerformIO . loadBMP $ "bird0.bmp"

birdDead :: Picture
birdDead = scale 0.5 0.5 . unsafePerformIO . loadBMP $ "bird1.bmp"

botBar :: Picture
botBar = scale 2 2 . unsafePerformIO . loadBMP $ "bot.bmp"

gameOver :: Picture
gameOver = scale 3 3 . unsafePerformIO . loadBMP $ "go.bmp"

pausePic :: Picture
pausePic = scale 5 5 . unsafePerformIO . loadBMP $ "pause.bmp"

topBar :: Picture
topBar = scale 2 2 . unsafePerformIO . loadBMP $ "top.bmp"

window :: Display
window = InWindow title (round size_x_window, round size_y_window) pos_window

trace' :: Show a => a -> a
trace' a = trace (show a) a

main :: IO ()
main = do
    r <- getStdGen
    play window background fps (initialState (randomRs ranTup r)) render handleKeys update
    --simulate window background fps (initialState (randomRs ranTup r)) render update
    --animate window background $ frame $ initialState (randomRs ranTup r)
  where
    ranTup = (0, (size_y_window -size_a_slit -300))
    update :: Float -> FbGame -> FbGame 
    update sec game
        | dead game = game
        | pause game = game
        | otherwise = detectCrushBar . detectCrushFloor . moveBar sec . movePlayer sec $ game

handleKeys :: Event -> FbGame -> FbGame
handleKeys (EventKey (SpecialKey KeySpace) (Down) _ _) game = game {birdVel = speed_bird}
handleKeys (EventKey (Char 'r') (Down) _ _) game = initialState (rds game)
handleKeys (EventKey (Char 'l') (Down) _ _) game = game {gameMode = 1}
handleKeys (EventKey (Char 'p') (Up) _ _) game =
    if (pause game)
        then game {pause=False}
        else game {pause=True}
handleKeys _ game = game

detectCrushFloor :: FbGame -> FbGame
detectCrushFloor game
    | birdPos game < -250 = game {birdStatus=3, dead=True}
    | otherwise = game

detectCrushBar :: FbGame -> FbGame
detectCrushBar game
    | (abs (barsP + (3*size_x_window/4))) < size_a_bird + size_a_bars
   && (birdP + size_a_bird > barsS + size_y_window/2 - 100
    || birdP - size_a_bird < barsS - size_a_slit + size_y_window/2 -100)
        = game {birdStatus=3, dead=True}
    -- | birdP == barsP = game {score = scoreNew}
    -- | abs(birdP-barsP) <3 = game {score = (score game)+1}
    | otherwise = game
  where
    birdP = birdPos game
    barsP = barPos game
    barsS = (-barSlit game)


debugger'' (a,b,c) = show a ++ " - " ++
    show b ++ " - " ++
    show c

render :: FbGame -> Picture
render game
    | dead game = pictures [standard, gameOver]
    | pause game = pictures [standard, pausePic]
    | otherwise = pictures [standard]
  where
    standard = pictures [
        bg0,
        bar (barPos game) (barSlit game),
        bg1,
        bird (birdPos game) (birdStatus game),
        scoreGen (score game)]

movePlayer :: Float -> FbGame -> FbGame
movePlayer sec game
    | gameMode game == 1 = game{birdPos = size_y_window/2 - 100 - barSlit game - size_a_slit/2}
    | otherwise = game {
    birdPos = p',
    birdVel = v',
    birdStatus = s'}
  where
    (p,v) = (birdPos game, birdVel game)
    (p', v', s') = (p + v'*sec, v - speed_grav*sec, if v'>0 then 1 else 0)

moveBar :: Float -> FbGame -> FbGame
moveBar sec game = game {
    barPos=p',
    barSlit=s',
    rds =r',
    score=sc'}
  where
    (p,  s,  r,  sc) = (barPos game, barSlit game, rds game, score game)
    (p', s', r', sc') --if p > -size_x_window then (p + speed_bar*sec , s, r) else (0,sNew,rNew)
        | p > -(size_x_window+size_a_bars*3) = (p + v*sec , s, r,sc)
        | otherwise           = (0,sNew,rNew,sc+1)
    v = fromIntegral $ speed_bar+speed_delta*sc
    sNew = head r
    rNew = tail r

bird :: Float -> Int -> Picture
bird y s = translate (0-size_x_window/4) y (sprite s)
  where
    sprite 0 = rotate ( 15) birdAlive
    sprite 1 = rotate (-15) birdAlive
    sprite 2 = birdDead
    sprite 3 = rotate 90 birdDead
--bird y = color color_bird $ translate (0-size_x_window/4) y $ circleSolid size_a_bird

bar :: Float -> Float -> Picture
bar x y = translate x (-y) barGen

debug :: FbGame -> Picture
debug game = pictures [text $ show (score game)]

-- take Int relative position of the bottom of the slit:
-- i.e.: 0: slit bottom at top + 100
barGen :: Picture
barGen = pictures [top',bot']
  where
    top' = translate (size_x_window/2 + 26) (size_y_window/2+200) topBar
    bot' = translate (size_x_window/2 + 26) (size_y_window/2-400-size_a_slit) botBar

scoreGen :: Int -> Picture
scoreGen int = translate (-225) 350 $ scale 3 3 $
                pictures[translate (-8) 0 $ dig d,
                    dig u]
  where
    dig i = bitmapSection (Rectangle (7*i,0) (7,10)) numsSprite
    u = int `mod` 10
    d = int `div` 10

numsSprite :: BitmapData
numsSprite = pic2bmp nums

nums :: Picture
nums = unsafePerformIO . loadBMP $ "num.bmp"

pic2bmp :: Picture -> BitmapData
pic2bmp (Bitmap bmpData) = bmpData