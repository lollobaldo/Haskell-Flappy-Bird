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
    birdSpeed :: Float,
    birdStatus :: Int,
    barSlit :: Float,
    barPos :: Float,
    barSpeed :: Float,
    bgPos :: Float,
    rds :: [Float],
    score :: Int,
--  0:Start, 1:Normal, 2:Pause, 3:Dead, 4:Cheat
    gameMode :: Int} deriving Show

color_bar  = green
color_bird = red

force_up = 10
force_do = 3

fps = 60

pos_window = (50,50)

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
speed_delta = -15
speed_grav = 500

title = "Flappy Bird in Haskell!!!"

background :: Color
background = white

initialState :: [Float] -> FbGame
initialState fs = FbGame {
    birdPos=0,
    birdSpeed=0,
    birdStatus = 0,
    barSlit=0,
    barPos=0,
    barSpeed=speed_bar,
    bgPos=0,
    rds=fs,
    score = 0,
    gameMode=0}


getSprite :: String -> FilePath
getSprite name = "sprites/" ++ name ++ ".bmp"

bg0 = unsafePerformIO . loadBMP . getSprite $ "bg0"
bg1 = translate 512 0 . unsafePerformIO . loadBMP . getSprite $ "bg1"
birdAlive = scale 0.5 0.5 . unsafePerformIO . loadBMP . getSprite $ "bird0"
birdDead = scale 0.5 0.5 . unsafePerformIO . loadBMP . getSprite $ "bird1"
botBar = scale 2 2 . unsafePerformIO . loadBMP . getSprite $ "bot"
fbPic = translate 0 200 . scale 3 3 . unsafePerformIO . loadBMP . getSprite $ "fb"
gameOver = scale 3 3 . unsafePerformIO . loadBMP . getSprite $ "go"
pausePic = scale 5 5 . unsafePerformIO . loadBMP . getSprite $ "pause"
tapPic = translate (-130) (-55) . scale 2 2 . unsafePerformIO . loadBMP . getSprite $ "tap"
topBar = scale 2 2 . unsafePerformIO . loadBMP . getSprite $ "top"

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
        | gameMode game == 1 || gameMode game == 4 = detectCrushBar . detectCrushFloor . moveBg sec . moveBar sec . movePlayer sec $ game
        | otherwise = game

handleKeys :: Event -> FbGame -> FbGame
handleKeys (EventKey (SpecialKey KeySpace) (Down) _ _) game
    | gameMode game == 0 = game {birdSpeed = speed_bird, gameMode = 1}
    | gameMode game == 1 = game {birdSpeed = speed_bird, gameMode = 1}
    | gameMode game == 2 = game {birdSpeed = speed_bird, gameMode = 1}
    | gameMode game == 3 = (initialState (rds game)) {birdSpeed = speed_bird, gameMode = 1}
    | gameMode game == 4 = game {birdSpeed = speed_bird, gameMode = 1}
handleKeys (EventKey (Char 'r') (Down) _ _) game = initialState (rds game)
handleKeys (EventKey (Char 'l') (Down) _ _) game
    | gameMode game == 4 = game {gameMode = 1}
    | otherwise = game {gameMode = 4}
handleKeys (EventKey (Char 'p') (Down) _ _) game
    | gameMode game == 2 = game {gameMode = 1}
    | otherwise = game {gameMode = 2}
handleKeys _ game = game

detectCrushFloor :: FbGame -> FbGame
detectCrushFloor game
    | birdPos game < -250 = game {birdStatus=3, gameMode=3}
    | otherwise = game

detectCrushBar :: FbGame -> FbGame
detectCrushBar game
    | (abs (barsP + (3*size_x_window/4))) < size_a_bird + size_a_bars
   && (birdP + size_a_bird > barsS + size_y_window/2 - 100
    || birdP - size_a_bird < barsS - size_a_slit + size_y_window/2 -100)
        = game {birdStatus=4, gameMode=3}
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
    | gameMode game == 0 = pictures [standard, fbPic, tapPic]
    | gameMode game == 2 = pictures [standard, pausePic]
    | gameMode game == 3 = pictures [standard, gameOver]
    | otherwise = pictures [standard]
  where
    standard = pictures [
        bg0,
        bar (barPos game) (barSlit game),
        bg (bgPos game),
        bird (birdPos game) (birdStatus game),
        --debug game,
        scoreGen (score game)]

movePlayer :: Float -> FbGame -> FbGame
movePlayer sec game
    | gameMode game == 4 = game{birdPos = size_y_window/2 - 100 - barSlit game - size_a_slit/2}
    | otherwise = game {
    birdPos = p',
    birdSpeed = v',
    birdStatus = s'}
  where
    (p,v) = (birdPos game, birdSpeed game)
    (p', v', s') = (p + v'*sec, v - speed_grav*sec, if v'>0 then 2 else 1)

moveBar :: Float -> FbGame -> FbGame
moveBar sec game = game {
    barPos=p',
    barSlit=s',
    barSpeed=v',
    rds=r',
    score=sc'}
  where
    (p,  s,  v,  r,  sc) = (barPos game, barSlit game, barSpeed game, rds game, score game)
    (p', s', v', r', sc') --if p > -size_x_window then (p + speed_bar*sec , s, r) else (0,sNew,rNew)
        | p > -(size_x_window+size_a_bars*3) = (p + v*sec , s, v+speed_delta*sec, r,sc)
        | otherwise           = (0,sNew,v+speed_delta*sec,rNew,sc+1)
    sNew = head r
    rNew = tail r

moveBg :: Float -> FbGame -> FbGame
moveBg sec game = game{bgPos=p'}
  where
    (p,v) = (bgPos game, barSpeed game)
    p'  | p > (-1024)  = p + (v-100)*sec
        | otherwise    = p + (v-100)*sec +1024

bird :: Float -> Int -> Picture
bird y s = translate (0-size_x_window/4) y (sprite s)
  where
    sprite 0 = birdAlive
    sprite 1 = rotate ( 15) birdAlive
    sprite 2 = rotate (-15) birdAlive
    sprite 3 = birdDead
    sprite 4 = rotate 90 birdDead
--bird y = color color_bird $ translate (0-size_x_window/4) y $ circleSolid size_a_bird

bar :: Float -> Float -> Picture
bar x y = translate x (-y) barGen

bg x = translate x 0 bg1

debug :: FbGame -> Picture
debug game = pictures [text $ show (barSpeed game)]

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
nums = unsafePerformIO . loadBMP . getSprite $ "num"

pic2bmp :: Picture -> BitmapData
pic2bmp (Bitmap bmpData) = bmpData