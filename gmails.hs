import qualified Network.HTTP.Conduit as HTTP (httpLbs, Request, parseUrl, applyBasicAuth, withManager, responseBody)
import qualified Data.ByteString.Char8 as BS (putStrLn, ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as BSLazy (unpack, putStrLn, ByteString)
import Data.Maybe (fromJust, isNothing)
import Text.Regex (mkRegex, matchRegex)
import qualified Data.Time.Clock.POSIX as Clock (getPOSIXTime, POSIXTime)

-- Could have used POSIXTime directly, but it not an instance of Show
type Time = Double
type Label = String
type Cache = [(Label,(Int, Time))]

data Request = Request { key :: Label
                       , username :: BS.ByteString
                       , password ::BS.ByteString
                       }

file = ".cache"
url = "https://gmail.com/mail/feed/atom/"

main = do ur <- userRequest
          cache <- cacheOfFile file
          maybeCacheHit <- return $ lookup (key ur) cache
          maybeCount <- return $ fmap fst maybeCacheHit
          count <- if isNothing maybeCount
                      then do maybeNetCount <- getFromGoogle ur
                              cacheToFile file $ (key ur, (fromJust maybeNetCount, 2)):cache
                              return $ fromJust maybeNetCount
                      else do return $ fromJust maybeCount
          print count

userRequest :: IO Request
userRequest = do
        label <- getLine
        (user,pass) <- getCredentials
        return $ Request label user pass

getFromGoogle :: Request -> IO (Maybe Int)
getFromGoogle req = HTTP.parseUrl (key req)
                      >>= HTTP.withManager . HTTP.httpLbs . auth req
                        >>= return . findCount . BSLazy.unpack . HTTP.responseBody
                where auth ur r = HTTP.applyBasicAuth  (username ur) (password ur) r


getCredentials :: IO (BS.ByteString, BS.ByteString)
getCredentials = do user <- getLine
                    pass <- getLine
                    return $ (p user, p pass)
                            where p = BS.pack

getCount :: HTTP.Request -> IO (Maybe Int)
getCount s = HTTP.withManager (HTTP.httpLbs s) >>=
        return . findCount . BSLazy.unpack . HTTP.responseBody

findCount :: String -> Maybe(Int)
findCount s = matchRegex regex s >>= return . read . (!! 0)
          where regex = mkRegex "<fullcount>([0-9+])</fullcount>"

cacheOfFile :: String -> IO Cache
cacheOfFile path = readFile path >>= return . read

cacheToFile :: String -> Cache -> IO()
cacheToFile path cache = writeFile path (show cache)

now :: IO Time
now = Clock.getPOSIXTime >>= return . realToFrac

eitherOfMaybe :: a -> Maybe b -> Either a b
eitherOfMaybe = flip maybe Right . Left

-- vim: set et:
