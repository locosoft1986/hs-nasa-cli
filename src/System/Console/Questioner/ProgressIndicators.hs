-- |
-- Module      :  System.Console.Questioner.ProgressIndicators
-- Description :  Provides progress indicators and spinners
-- Copyright   :  (c) Pedro Yamada
-- License     :  MIT
--
-- Maintainer  :  Pedro Yamada <tacla.yamada@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (not tested on multiple environments)
--
-- Shamefully steals ideas from modules like `Inquirer.js` and `go-spin`.
module System.Console.Questioner.ProgressIndicators
  where
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Control.Concurrent -- (MVar, ThreadId, forkIO, killThread, modifyMVar_,
                          --  newMVar, tryTakeMVar, threadDelay)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import System.Console.ANSI (clearLine, setCursorColumn)
import System.Console.Terminal.Size (size, Window(..))
import System.IO (BufferMode(NoBuffering), stdout)
import System.Console.Questioner.Util

-- ProgressIndicator type and utilities
-------------------------------------------------------------------------------

data ProgressIndicator = BarIndicator ThreadId (MVar Double)
                       | SpinnerIndicator ThreadId

stopIndicator :: ProgressIndicator -> IO ()
stopIndicator i = case i of
    (BarIndicator tid _) -> stopProgressIndicator' tid
    (SpinnerIndicator tid) -> stopProgressIndicator' tid
  where
    stopProgressIndicator' tid = do
        killThread tid
        clearLine
        setCursorColumn 0

updateIndicator :: ProgressIndicator -> Double -> IO ()
updateIndicator (BarIndicator _ c) i = putMVar c i
updateIndicator _ _ = return ()

-- ProgressBars
-------------------------------------------------------------------------------

newtype ProgressBarTheme = ProgressBarTheme (Double -> IO ())

progressBar :: ProgressBarTheme -> IO ProgressIndicator
progressBar (ProgressBarTheme render) = do
    mi <- newEmptyMVar
    render 0
    tid <- forkIO $ hWithBufferMode stdout NoBuffering $ forever $ do
        i <- takeMVar mi
        clearLine
        setCursorColumn 0
        render i
    putMVar mi 0
    return $ BarIndicator tid mi

-- Spinners
-------------------------------------------------------------------------------

type SpinnerTheme = String

type SpinnerColor = String -> String

data SpinnerConfig = SpinnerConfig {
  _spinnerTheme :: SpinnerTheme,
  _spinnerColor :: Maybe SpinnerColor
}

spinner :: SpinnerConfig -> Int -> String -> IO ProgressIndicator
spinner config interval prompt = SpinnerIndicator <$> forkIO (setup $ loop 0)
  where
    setup = hWithBufferMode stdout NoBuffering
    theme = _spinnerTheme config
    colorFn = case (_spinnerColor config) of
                    Just fn -> fn
                    Nothing -> id
    loop i = do
        clearLine
        setCursorColumn 0
        putStr $ " " <> (colorFn $ (spinnerState i):[]) <> " " <> prompt
        threadDelay interval
        loop $ i + 1

    -- TODO - parameterize
    themeLen = length $ theme
    spinnerState i = theme !! (i `mod` themeLen)

-- Boilerplate for easier usage (TODO - generate this with TH)
-------------------------------------------------------------------------------

simple1SpinnerTheme, simple2SpinnerTheme, simple3SpinnerTheme,
  simple4SpinnerTheme, simple5SpinnerTheme, simple6SpinnerTheme,
  simple7SpinnerTheme, simple8SpinnerTheme, simple9SpinnerTheme,
  dots1SpinnerTheme, dots2SpinnerTheme, dots3SpinnerTheme, dots4SpinnerTheme,
  dots5SpinnerTheme, dots6SpinnerTheme, dots7SpinnerTheme :: SpinnerTheme

simple1Spinner, simple2Spinner, simple3Spinner, simple4Spinner, simple5Spinner,
  simple6Spinner, simple7Spinner, simple8Spinner, simple9Spinner, dots1Spinner,
  dots2Spinner, dots3Spinner, dots4Spinner, dots5Spinner, dots6Spinner,
  dots7Spinner :: Int -> String -> IO ProgressIndicator

simple1SpinnerTheme = "|/-\\"
simple2SpinnerTheme = "◴◷◶◵"
simple3SpinnerTheme = "◰◳◲◱"
simple4SpinnerTheme = "◐◓◑◒"
simple5SpinnerTheme = "▉▊▋▌▍▎▏▎▍▌▋▊▉"
simple6SpinnerTheme = "▌▄▐▀"
simple7SpinnerTheme = "╫╪"
simple8SpinnerTheme = "■□▪▫"
simple9SpinnerTheme = "←↑→↓"
simple1Spinner = spinner (SpinnerConfig simple1SpinnerTheme Nothing)
simple2Spinner = spinner (SpinnerConfig simple2SpinnerTheme Nothing)
simple3Spinner = spinner (SpinnerConfig simple3SpinnerTheme Nothing)
simple4Spinner = spinner (SpinnerConfig simple4SpinnerTheme Nothing)
simple5Spinner = spinner (SpinnerConfig simple5SpinnerTheme Nothing)
simple6Spinner = spinner (SpinnerConfig simple6SpinnerTheme Nothing)
simple7Spinner = spinner (SpinnerConfig simple7SpinnerTheme Nothing)
simple8Spinner = spinner (SpinnerConfig simple8SpinnerTheme Nothing)
simple9Spinner = spinner (SpinnerConfig simple9SpinnerTheme Nothing)

dots1SpinnerTheme = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
dots2SpinnerTheme = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
dots3SpinnerTheme = "⠄⠆⠇⠋⠙⠸⠰⠠⠰⠸⠙⠋⠇⠆"
dots4SpinnerTheme = "⠋⠙⠚⠒⠂⠂⠒⠲⠴⠦⠖⠒⠐⠐⠒⠓⠋"
dots5SpinnerTheme = "⠁⠉⠙⠚⠒⠂⠂⠒⠲⠴⠤⠄⠄⠤⠴⠲⠒⠂⠂⠒⠚⠙⠉⠁"
dots6SpinnerTheme = "⠈⠉⠋⠓⠒⠐⠐⠒⠖⠦⠤⠠⠠⠤⠦⠖⠒⠐⠐⠒⠓⠋⠉⠈"
dots7SpinnerTheme = "⠁⠁⠉⠙⠚⠒⠂⠂⠒⠲⠴⠤⠄⠄⠤⠠⠠⠤⠦⠖⠒⠐⠐⠒⠓⠋⠉⠈⠈"
dots1Spinner = spinner (SpinnerConfig dots1SpinnerTheme Nothing)
dots2Spinner = spinner (SpinnerConfig dots2SpinnerTheme Nothing)
dots3Spinner = spinner (SpinnerConfig dots3SpinnerTheme Nothing)
dots4Spinner = spinner (SpinnerConfig dots4SpinnerTheme Nothing)
dots5Spinner = spinner (SpinnerConfig dots5SpinnerTheme Nothing)
dots6Spinner = spinner (SpinnerConfig dots6SpinnerTheme Nothing)
dots7Spinner = spinner (SpinnerConfig dots7SpinnerTheme Nothing)

simpleProgressBarTheme :: ProgressBarTheme
simpleProgressBarTheme = ProgressBarTheme $ \i -> do
    w <- fromMaybe (45 :: Int) <$> (fmap width <$> size)
    let blocks = floor ((fromIntegral w :: Double) * i) - 3
    putStr (replicate blocks '▉')

simpleProgressBar :: IO ProgressIndicator
simpleProgressBar = progressBar simpleProgressBarTheme
