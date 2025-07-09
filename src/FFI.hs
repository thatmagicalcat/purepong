{-# LANGUAGE CApiFFI #-}

module FFI (
    initWindow,
    windowShouldClose,
    closeWindow,
    beginDrawing,
    endDrawing,
    clearBackground,
    setTargetFPS,
    getFPS,
    getFrameTime,
    drawRectangle,
    isKeyDown,
    drawCircle,
    keyW,
    keyS
) where

import Foreign
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String

foreign import capi "raylib.h InitWindow" initWindow :: CInt -> CInt -> CString -> IO ()
foreign import capi "raylib.h WindowShouldClose" windowShouldClose :: IO CBool
foreign import capi "raylib.h CloseWindow" closeWindow :: IO ()
foreign import capi "raylib.h BeginDrawing" beginDrawing :: IO ()
foreign import capi "raylib.h EndDrawing" endDrawing :: IO ()
foreign import capi "raylib.h ClearBackground" clearBackground :: CUInt -> IO ()
foreign import capi "raylib.h SetTargetFPS" setTargetFPS :: CInt -> IO ()
foreign import capi "raylib.h GetFPS" getFPS :: IO CInt
foreign import capi "raylib.h GetFrameTime" getFrameTime :: IO CFloat
foreign import capi "raylib.h DrawRectangle" drawRectangle :: CInt -> CInt -> CInt -> CInt -> CUInt -> IO ()
foreign import capi "raylib.h IsKeyDown" isKeyDown :: CInt -> IO CBool
foreign import capi "raylib.h DrawCircle" drawCircle :: CInt -> CInt -> CFloat -> CUInt -> IO ()
foreign import capi "raylib.h value KEY_W" keyW :: CInt
foreign import capi "raylib.h value KEY_S" keyS :: CInt
