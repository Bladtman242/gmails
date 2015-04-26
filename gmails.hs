import qualified Network.HTTP.Conduit as HTTP (httpLbs, Request, parseUrl, applyBasicAuth, withManager, responseBody)
import qualified Data.ByteString.Char8 as BS (putStrLn, ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as BSLazy (unpack, putStrLn, ByteString)
import Data.Maybe (fromJust, isNothing)
import Text.Regex (mkRegex, matchRegex)
import Cache

type Label = String
data Request = Request { key :: Label
                       , username :: BS.ByteString
                       , password ::BS.ByteString
                       }

file = ".cache"
url = "https://gmail.com/mail/feed/atom/"
cacheMaxAge = 30 -- in seconds

main = do ur <- userRequest
          cache <- (loadCache file :: IO(Cache Label Int))
          maybeCacheHit <- lookupCache cacheMaxAge cache (key ur)
          count <- if isNothing maybeCacheHit
                      then do maybeNetCount <- getFromGoogle ur
                              nCache <- addCache (key ur, fromJust maybeNetCount) cache
                              persistCache file nCache
                              return $ fromJust maybeNetCount
                      else do return $ fromJust maybeCacheHit
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

-- vim: set et:
