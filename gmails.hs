{-# LANGUAGE OverloadedStrings #-}
import qualified Network.HTTP.Conduit as HTTP (httpLbs, Request, parseUrl, applyBasicAuth, withManager, responseBody, requestHeaders, setQueryString)
import qualified Data.ByteString.Char8 as BS (putStrLn, ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as BSLazy (unpack, putStrLn, ByteString)
import Data.Maybe (fromJust, isJust)
import Text.Regex (mkRegex, matchRegex)
import Cache
import qualified Network.Google.OAuth2 as GOAuth (OAuth2Client(OAuth2Client, clientId, clientSecret), exchangeCode, refreshTokens, formUrl, OAuth2Tokens(accessToken, expiresIn), validateTokens)
import Control.Exception.Base (catch)
import Control.Exception
import System.IO.Unsafe
import System.Environment (getArgs)

tokenFile =".tokens"
cacheFile = ".cache"
cacheMaxAge = 3000 -- in seconds
--url = "https://www.googleapis.com/gmail/v1/users/me/messages?fields=resultSizeEstimate&q=is%3Aunread+in%3A_itu&key={YOUR_API_KEY}}"
url = "https://www.googleapis.com/gmail/v1/users/me/messages"

type Label = String

-- google oauth client credentials (client as in application)
cid = "687455075182-q4h119ncarn5ckd9rnn6b5kjcp6kvktn.apps.googleusercontent.com"
secret = "F8cVQyu3KSl-biGRcYFbKNne"
client = GOAuth.OAuth2Client { GOAuth.clientId = cid, GOAuth.clientSecret = secret }

main = do label <- countLabel
          cache <- loadCache cacheFile
          maybeCacheHit <- lookupCache cacheMaxAge cache label
          count <- if isJust maybeCacheHit
                      then do return $ fromJust maybeCacheHit
                      else do toks <- getUserTokens
                              maybeNetCount <- countFromGoogle label toks
                              nCache <- addCache (label, fromJust maybeNetCount) cache
                              persistCache cacheFile nCache
                              return $ fromJust maybeNetCount
          print count

refreshUserToken :: GOAuth.OAuth2Tokens -> IO GOAuth.OAuth2Tokens
refreshUserToken toks = do timeToLive <- GOAuth.validateTokens toks
                           if timeToLive < 2
                           then GOAuth.refreshTokens client toks
                           else return toks

loadUserTokens :: IO GOAuth.OAuth2Tokens
loadUserTokens = readFile tokenFile >>= return . read

saveUserTokens :: GOAuth.OAuth2Tokens -> IO ()
saveUserTokens = writeFile tokenFile . show

getUserConsent :: IO GOAuth.OAuth2Tokens
getUserConsent = do
        let permissionUrl = GOAuth.formUrl client ["https://www.googleapis.com/auth/gmail.readonly"]
        putStrLn "gmails requires your permission to read your email data. You can obtain an access token via the following url. you can at any time revoke this token by visiting google.com/settings."
        putStrLn $ "Obtain the access token from: " ++ show permissionUrl
        putStrLn "and paste it here, then press enter: "
        authcode <- getLine
        GOAuth.exchangeCode client authcode

getUserTokens :: IO GOAuth.OAuth2Tokens
getUserTokens = tokenFromFile `catch` handle
        where tokenFromFile = loadUserTokens >>= refreshUserToken
              handle :: IOException -> IO GOAuth.OAuth2Tokens
              handle e = do tokens <- getUserConsent
                            saveUserTokens tokens
                            return tokens

countLabel :: IO String
countLabel = getArgs >>=
                \args -> return $
                        case length args of
                                0 -> "inbox"
                                2 -> if (args !! 0) == "-l"
                                        then args !! 1
                                        else error "Unkown flag: " ++ args !! 0
                                _ -> error "gmails accepts 0 or 2 paramaters"

countFromGoogle :: Label -> GOAuth.OAuth2Tokens -> IO (Maybe Int)
countFromGoogle label tokens = HTTP.parseUrl url
                      >>= HTTP.withManager . HTTP.httpLbs . prepRequest tokens label
                        >>= return . findCount . BSLazy.unpack . HTTP.responseBody
                where prepRequest tokens label req =
                        HTTP.setQueryString [("q", Just . BS.pack $ "is:unread " ++ "in:" ++ label)
                                            ,("in", Just $ BS.pack label)
                                            ,("access_token", Just . BS.pack $ GOAuth.accessToken tokens)
                                            ,("fields", Just "resultSizeEstimate")
                                            ]
                                            req

getCredentials :: IO (BS.ByteString, BS.ByteString)
getCredentials = do user <- getLine
                    pass <- getLine
                    return $ (p user, p pass)
                            where p = BS.pack

getCount :: HTTP.Request -> IO (Maybe Int)
getCount s = HTTP.withManager (HTTP.httpLbs s) >>=
        return . findCount . BSLazy.unpack . HTTP.responseBody

findCount :: String -> Maybe Int
findCount s = matchRegex regex s >>= return . read . first
          where regex = mkRegex "\"resultSizeEstimate\": ([0-9]+)"
                first = (!! 0)

-- vim: set et:
