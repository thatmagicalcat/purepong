{-# LANGUAGE CApiFFI #-}

import Foreign
import Foreign.Storable
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

    let py = paddleY state 
    let newPaddleY = if up == 1 then
            round $ fromIntegral py - paddleMoveSpeed * dt
        else if down == 1 then
                round $ fromIntegral py + paddleMoveSpeed * dt
            else py

    -- Ball
    let bx = fromIntegral (ballX state)
    let by = fromIntegral (ballY state)
    let vx = ballVX state
    let vy = ballVY state
    let newBallX = round $ fromIntegral bx + fromIntegral vx * dt :: CInt
    let newBallY = round $ fromIntegral by + fromIntegral vy * dt :: CInt

    let (vx', bx') =
            if newBallX - round ballRadius < 0 || fromIntegral newBallX + ballRadius > fromIntegral windowWidth
                then (-vx, ballX state)
                else (vx, newBallX)

    let (vy', by') =
            if newBallY - round ballRadius < 0 || fromIntegral newBallY + ballRadius > fromIntegral windowHeight
                then (-vy, ballY state)
                else (vy, newBallY)

    return GameState {
        leftPaddleX = leftPaddleX state,
        rightPaddleX = rightPaddleX state,
        paddleY = newPaddleY,
        ballX = bx',
        ballY = by',
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
