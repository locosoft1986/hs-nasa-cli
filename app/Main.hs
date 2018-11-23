{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/xxczaki/nasa-cli/blob/master/cli.js
import Control.Concurrent
import System.Console.Questioner
import System.Console.Chalk
import Control.Monad.IO.Class
import Control.Arrow ((>>>))
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Network.HTTP.Simple as HS
import Network.Connection (TLSSettings (..))
import Data.ByteString.Lazy (ByteString, isSuffixOf, splitWith)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Control.Exception as E
import Text.XML.HXT.Core (runX, getAttrValue, getText, (/>))
import Text.HandsomeSoup (css, parseHtml)

type ResponseString = Response ByteString
type ResponseContent = ByteString

checkNasaResponse ::  ResponseString -> Maybe ResponseContent
checkNasaResponse rs = if (statusIsSuccessful . HS.getResponseStatus) rs
                   then Just $  HS.getResponseBody rs
                   else Nothing

handleException :: E.SomeException -> IO (Maybe ResponseString)
handleException _ = return Nothing

contentFromUrl :: String -> IO (Maybe ResponseContent)
contentFromUrl url = do
  request <- parseRequest url
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- newManager settings
  res <- E.handle handleException (Just <$> httpLbs request manager)
  return $ res >>= checkNasaResponse

hasJpeg :: String -> Bool
hasJpeg s = BC.pack ".jpg" `isSuffixOf` BC.pack s

getFirst :: [String] -> Maybe String
getFirst [] = Nothing
getFirst (x:_) = Just x

logInfo :: String
logInfo = blue "ℹ"

logSuccess :: String
logSuccess = green "✔"

logError :: String
logError = red "✖"

logWarning :: String
logWarning = yellow "⚠"

defaultSpinner :: String -> IO ProgressIndicator
defaultSpinner = spinner (SpinnerConfig dots1SpinnerTheme (Just cyan)) (1000 * 80)

extractInfo :: ResponseContent -> IO (Maybe (String, String))
extractInfo content = do
  let doc = parseHtml . BC.unpack $ content
  elems <- liftIO $ runX $ doc >>> css "p a" >>> getAttrValue "href"
  titles <- liftIO $ runX $ doc >>> css "center b" /> getText
  return $ (,) <$> (getFirst.filter hasJpeg) elems <*> getFirst titles


parseImageInfo :: String -> IO (Maybe (String, String))
parseImageInfo url = contentFromUrl url >>= maybe (return Nothing) extractInfo

-- extractUrl :: ResponseContent -> IO (Maybe String)
-- extractUrl content = do
--   let doc = parseHtml . BC.unpack $ content
--   elems <- liftIO $ runX $ doc >>> css "p a" >>> getAttrValue "href"
--   return $ (getFirst.filter hasJpeg) elems
--
-- parseContent :: String -> IO (Maybe String)
-- parseContent url = contentFromUrl url >>= maybe (return Nothing) extractUrl

writeImage :: String -> ResponseContent -> IO String
writeImage name content = do
  BC.writeFile name content
  return $ logSuccess ++ " Done ~"


downloadImage :: String -> IO String
downloadImage url = do
  let name = last . splitWith (== BS.c2w '/') $ BC.pack url
  c <- contentFromUrl url
-- maybe :: IO String -> (ResponseContent -> IO String) -> Maybe ResponseContent -> IO String
  maybe (return $ logError ++ " Please check your Internet Connection! ") (writeImage $ BC.unpack name) c

fetchImage :: (String, String) -> IO ()
fetchImage (img, title) = do
  let url = "https://apod.nasa.gov/apod/" ++ img
  s <- defaultSpinner " Hacked! We are sending you the media..."
  result <- downloadImage url
  stopIndicator s
  putStr result
  putStrLn $ dim $ " [" ++ title ++ "]"

fetchNasaImage :: IO ()
fetchNasaImage = do
  s <- defaultSpinner " Hacking to NASA servers..."
  img <- parseImageInfo "https://apod.nasa.gov/apod/ap181114.html"
  stopIndicator s
  maybe (putStrLn $ logInfo ++ " Today has no image :p") fetchImage img

main :: IO ()
main = do
  putStrLn $ (red "red" ++ blue " blue" ++ yellow " yellow" ++ green " green" ++ cyan " cyan")


  --   cabal install maybet

  -- import Control.Monad.Maybe

  -- f1 :: IO (Maybe Int)
  -- f1 = return . Just $ 1

  -- d2 :: Int -> IO (Maybe String)
  -- d2 = return . Just . show

  -- blah :: IO (Maybe (Int, String))
  -- blah = do
  --   runMaybeT $ do
  --   a <- MaybeT f1
  --   b <- MaybeT $ d2 a
  --   return (a,b)

  -- Or slightly rewritten:

  -- f1 :: MaybeT IO Int
  -- f1 = return 1
  -- -- f1 = fail "why oh why?!?"

  -- d2 :: Int -> MaybeT IO String
  -- d2 = return . show

  -- blah = do
  --   runMaybeT $ do
  --   a <- f1
  --   b <- d2 a
  --   return (a,b)

  -- Solution2:
  -- import Data.Maybe (maybe)

  -- test :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
  -- test v f = v >>= maybe (return Nothing) f


-- import Control.Concurrent
-- import System.Console.Questioner
-- import System.Console.Chalk
--
-- main :: IO ()
-- main = do
--     s <- spinner (SpinnerConfig dots1SpinnerTheme (Just blue)) (1000 * 80) (magenta "Loading...")
--     threadDelay (1000 * 10000) -- 10s
--     stopIndicator s
--     putStrLn "Done!"
