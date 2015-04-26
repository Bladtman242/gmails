module Cache
( Cache
, emptyCache
, loadCache
, persistCache
, addCache
, lookupCache
) where

import qualified Data.Time.Clock.POSIX as Clock (getPOSIXTime, POSIXTime)

type Time = Double

newtype CacheEntry a b = Entry (a, (b, Time)) deriving (Show, Read)
newtype Cache a b = Cache [CacheEntry a b] deriving (Show, Read)

emptyCache :: Cache a b
loadCache :: (Read a, Read b) => FilePath -> IO (Cache a b)
persistCache :: (Show a, Show b) => FilePath -> Cache a b-> IO ()
lookupCache :: Eq k => Double -> Cache k v -> k -> IO (Maybe v)
addCache :: (Eq k, Read v, Show v) => (k,v) -> Cache k v -> IO(Cache k v)
now :: IO Time

emptyCache = Cache []

loadCache path = readFile path >>= return . read

persistCache path = writeFile path . show

lookupCache maxAge (Cache cache) key =
        do currentTime <- now
           return $ lookup key cache' >>=
                   valueOfEntry maxAge currentTime
                           where cache' = map (\(Entry (k,v)) -> (k,v)) cache

valueOfEntry :: Double -> Time -> (b, Time) -> Maybe b
valueOfEntry maxAge currentTime (v,t) =
         maybe' (realToFrac (currentTime-t) <= maxAge) v

addCache (k,v) (Cache cache) = do time <- now
                                  return $ Cache $ Entry(k, (v, time)):cache

now = Clock.getPOSIXTime >>= return . realToFrac

eitherOfMaybe :: a -> Maybe b -> Either a b
eitherOfMaybe = flip maybe Right . Left

maybe' c v = if c
                then Just v
                else Nothing

-- vim: set et:
