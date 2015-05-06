{-# LANGUAGE Arrows, OverloadedStrings #-}
module Main where

import FRP.Yampa

import Graphics
import Shapes
import Input

-- < Constants > ---------------------------------------------------------------

cubeX, cubeWidth, cubeHeight :: Double
cubeX      = 100
cubeWidth  = 30
cubeHeight = 30

pipeWidth, pipeGap :: Double
pipeWidth = 60
pipeGap   = 200

cubeColour, pipeColour, skyColour, groundColour :: Colour Double
cubeColour    = sRGB24 0xED 0xBA 0x00
pipeColour    = sRGB24 0x1A 0xAF 0x5D
skyColour     = sRGB24 0xAD 0xD4 0xF4
groundColour  = sRGB24 0xCE 0xB1 0x71

winHeight, winWidth :: Double
winHeight = 600
winWidth  = 300

groundHeight :: Double
groundHeight = winHeight / 8

g :: Double
g = 200

-- < Game State > --------------------------------------------------------------

data Cube = Cube { cubePos :: Double
                 , cubeVel :: Double
                 }

initCube :: Cube
initCube = Cube (winHeight / 2) 0

data Pipe = Pipe { pipePos    :: Double
                 , pipeHeight :: Double
                 }
initPipe :: Pipe
initPipe = Pipe winWidth 100

data Game = Game { gameCube :: Cube
                 , gamePipe :: Pipe
                 }

initGame :: Game
initGame = Game initCube initPipe

-- < Game logic > --------------------------------------------------------------

game :: SF AppInput Game
game = proc input -> do
    let cube = initCube
    let pipe = initPipe
    returnA -< Game cube pipe

-- < Rendering > ---------------------------------------------------------------

render :: Game -> Object
render (Game (Cube cubeY _) (Pipe pipeX pipeHeight)) = scene & colour_ skyColour
    where scene = scene_ [ground, cube, bottomPipe, upperPipe]
          ground = rectangle_ winWidth groundHeight & pos_ (0, groundHeight)
                                                    & colour_ groundColour
          cube = rectangle_ cubeWidth cubeHeight & pos_ (cubeX, cubeY)
                                                 & colour_ cubeColour
          bottomPipe = rectangle_ pipeWidth pipeHeight & pos_ (pipeX, pipeHeight)
                                                       & colour_ pipeColour
          upperPipe = rectangle_ pipeWidth (winHeight - pipeGap - pipeHeight) 
                                                    & pos_ (pipeX, winHeight)
                                                    & colour_ pipeColour

-- < Input handling > ----------------------------------------------------------

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

flapTrigger :: SF AppInput (Event ())
flapTrigger = proc input -> do
    mouseTap    <- lbp -< input
    spacebarTap <- keyPressed ScancodeSpace -< input
    returnA -< mouseTap `lMerge` spacebarTap


-- < Main Function > -----------------------------------------------------------

main :: IO ()
main = animate "Yampy cube" (round winWidth) (round winHeight)
                            (parseWinInput >>> ((game >>^ render) &&& handleExit))

