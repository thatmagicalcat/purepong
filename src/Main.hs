{-# LANGUAGE CApiFFI #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String

import qualified FFI as C
import Consts

main = do
    title <- newCString "hello world" 
    C.initWindow windowWidth windowHeight title
    C.setTargetFPS 60
    runGame initialState
    C.closeWindow

initialState = GameState {
    leftPaddleX = 20,
    rightPaddleX = (windowWidth - 20 - paddleWidth),
    paddleY = windowHeight `div` 2 - paddleHeight `div` 2,
    ballX = windowWidth `div` 2,
    ballY = windowHeight `div` 2,
    ballVX = 300,
    ballVY = 250
}

data GameState = GameState {
    leftPaddleX :: CInt,
    rightPaddleX :: CInt,
    paddleY :: CInt,
    ballX :: CInt,
    ballY :: CInt,
    ballVX :: CInt,
    ballVY :: CInt
}

draw :: GameState -> IO ()
draw state = do
    let lpx = leftPaddleX state
    let rpx = rightPaddleX state
    let py = paddleY state

    C.drawRectangle lpx py paddleWidth paddleHeight paddleColor
    C.drawRectangle rpx py paddleWidth paddleHeight paddleColor

    C.drawCircle (ballX state) (ballY state) ballRadius white 

updateGameState :: GameState -> IO GameState
updateGameState state = do
    up <- C.isKeyDown C.keyW
    down <- C.isKeyDown C.keyS
    dt <- C.getFrameTime

    -- Paddle
    let py = paddleY state 
    let npy = if up == 1 then
                  round $ fromIntegral py - paddleMoveSpeed * dt
              else if down == 1 then
                  round $ fromIntegral py + paddleMoveSpeed * dt
              else py

    let newPaddleY = if npy >= 0 && npy + paddleHeight <= windowHeight then npy else py

    -- Ball motion
    let bx = ballX state
    let by = ballY state
    let vx = ballVX state
    let vy = ballVY state

    let bx' = bx + round (fromIntegral vx * dt)
    let by' = by + round (fromIntegral vy * dt)

    -- Wall collision (top/bottom)
    let (vy', finalBy) =
            if by' - round ballRadius < 0 || by' + round ballRadius > windowHeight
                then (-vy, by)
                else (vy, by')

    -- Paddle collision
    let ballLeft   = bx' - round ballRadius
    let ballRight  = bx' + round ballRadius
    let ballTop    = by' - round ballRadius
    let ballBottom = by' + round ballRadius

    let lpX = leftPaddleX state
    let rpX = rightPaddleX state
    let paddleTop = newPaddleY
    let paddleBottom = newPaddleY + paddleHeight

    let hitLeftPaddle =
            ballLeft   <= lpX + paddleWidth &&
            ballRight  >= lpX &&
            ballBottom >= paddleTop &&
            ballTop    <= paddleBottom

    let hitRightPaddle =
            ballRight  >= rpX &&
            ballLeft   <= rpX + paddleWidth &&
            ballBottom >= paddleTop &&
            ballTop    <= paddleBottom

    let (vx', finalBx) =
            if hitLeftPaddle || hitRightPaddle
                then (-vx, bx)
                else if ballLeft < 0 || ballRight > windowWidth
                    then (-vx, bx)
                    else (vx, bx')

    return GameState {
        leftPaddleX = lpX,
        rightPaddleX = rpX,
        paddleY = newPaddleY,
        ballX = finalBx,
        ballY = finalBy,
        ballVX = vx',
        ballVY = vy'
    }

runGame :: GameState -> IO ()
runGame state = do
    cond <- C.windowShouldClose
    if cond == 0 then do
        state' <- updateGameState state

        C.beginDrawing 
        C.clearBackground black
        draw state'
        C.endDrawing

        runGame state'
    else return ()
