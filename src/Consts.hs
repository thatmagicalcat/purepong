module Consts (
    windowHeight,
    windowWidth,
    paddleMoveSpeed,
    paddleHeight, 
    paddleWidth,
    paddleColor,
    ballRadius,
    white,
    black
) where

import Foreign.C.Types

windowHeight = 800 :: CInt
windowWidth = 800 :: CInt
paddleMoveSpeed = 1000.0 :: CFloat 
paddleHeight = 100 :: CInt
paddleWidth = 30 :: CInt
paddleColor = white
ballRadius = 10 :: CFloat

white = 0xFFFFFFFF :: CUInt
black = 0 :: CUInt
