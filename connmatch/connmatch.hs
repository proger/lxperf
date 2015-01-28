{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  Main
----
---- `connmatch' parses the file that maps local IPs to hostnames and
---- a directory of lsof outputs where files are named exactly as
---- hostnames. `lsof' outputs are connected into a list of connections
---- which are used as graphviz edges.
---- The current implementation doesn't take any outgoing connections into
---- account yet.
----
-------------------------------------------------------------------------------
--

module Main where

import System.Environment (getArgs)
import System.Directory (getDirectoryContents)

import Control.Monad (join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Word (Word8)

newtype Process = Process BS.ByteString deriving (Show, Eq)
newtype Host = Host BS.ByteString deriving (Show, Eq)
newtype Addr = Addr (IP, BS.ByteString) deriving (Show, Ord, Eq)

-- | 'lsof' entry
data OpenFile = Connection Host Process Addr Addr 
                | Listen Host Process Addr
                deriving (Show, Eq)

data IP = IP BS.ByteString
        | Any 
        deriving (Show, Ord, Eq)

type IPMap = M.Map IP Host

-- | How an OpenFile links to the network: locally, connects somewhere else
-- or does not link at all (listening socket).
data Link = Link OpenFile | External | Empty deriving (Show)

sIP :: BS.ByteString -> IP
sIP "*" = Any
sIP s = IP s

sAddr :: BS.ByteString -> Addr
sAddr s = Addr (sIP ip, port)
    where
      [ip, port] = C.split ':' s

-- | Extract a source address from an 'OpenFile'.
openFileSrc :: OpenFile -> Addr
openFileSrc (Connection _ _ src _) = src
openFileSrc (Listen _ _ src) = src

-- | Checks if an address is IPv6, which is not supported.
addrNotIp6 :: BS.ByteString -> Bool
addrNotIp6 a = (BS.head a) /= (BS.head "[")

matchAddr :: IPMap -> Host -> Addr -> Addr
matchAddr ipmap (Host defaultHost) addr@(Addr (ip, port)) = maybe addr toAddr $ M.lookup ip ipmap
                                                                  where toAddr (Host host) = Addr (IP host, port)


sOpenFile :: IPMap
             -> String          -- ^ hostname that will turn into a 'Host'
             -> [BS.ByteString] -- ^ tokenized line of lsof: COMMAND PID USER FD  TYPE DEVICE SIZE/OFF NODE NAME (STATE)
             -> Maybe OpenFile
sOpenFile ipmap hostname [comm, _, _, _, _, _, _, "TCP", name, state]
  | addrNotIp6 name = let hostS = C.pack hostname
                          host = Host hostS
                          addr s = case sAddr s of -- cleanup ambiguous addresses
                                     Addr (IP "127.0.0.1", port) -> Addr (IP hostS, port)
                                     Addr (Any, port) -> Addr (IP hostS, port)
                                     a -> matchAddr ipmap host a
                          in case state of
                                "(LISTEN)" ->
                                  Just $ Listen host (Process comm) (addr name)
                                _ ->
                                  Just $ Connection host (Process comm) (addr src) (addr dst)
                                        where
                                          dst = C.drop 2 dst'
                                          (src, dst') = C.breakSubstring "->" name
  | otherwise = Nothing
sOpenFile _ _ _ = Nothing

srcMap :: [OpenFile] -> M.Map Addr OpenFile
srcMap = M.fromList . map (\c -> (openFileSrc c, c))

linkFiles :: [OpenFile] -> M.Map Addr OpenFile -> [(OpenFile, Link)]
linkFiles conns srcmap = [(c, pair c) | c <- conns]
    where
      pair (Listen _ _ _) = Empty
      pair conn@(Connection _ _ _ dst) = case M.lookup dst srcmap of
                                           Just f -> Link f
                                           Nothing -> External

mapFromIpaddrs :: BS.ByteString -> IPMap
mapFromIpaddrs = M.fromList . map ((\[a,b] -> (sIP b, Host a)) . C.words) . C.lines

lsofOpenFiles :: IPMap -> String -> BS.ByteString -> [OpenFile]
lsofOpenFiles ipmap h = catMaybes . map (sOpenFile ipmap h . C.words) . drop 1 . C.lines

data Stream = Stream Process Addr Process Addr deriving (Show, Eq)

stream :: IPMap -> (OpenFile, Link) -> Maybe Stream
stream ipmap connpair = uncurry link connpair
    where
      link c Empty = Nothing
      link c (Link l) = Just $ streamLink c l
      link (Connection h p src dst@(Addr (IP ipD, portD)))
           External = Just $ Stream p src (Process portD) dst

      streamLink (Connection hA pA srcA dstA)
                 (Connection hB pB srcB dstB) = Stream pA srcA pB srcB
      streamLink (Connection hA pA srcA dstA) (Listen hB pB srcB) = Stream pA srcA pB srcB
      streamLink (Listen hA pA srcA) (Connection hB pB srcB dstB) = Stream pA srcA pB srcB

data Edge = Edge BS.ByteString BS.ByteString deriving (Show)

instance Eq Edge where
    (Edge a1 a2) == (Edge b1 b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

edge (Stream (Process commA) (Addr (IP addrA, _)) (Process commB) (Addr (IP addrB, _))) = Edge (C.concat [addrA, ":", commA]) (C.concat [addrB, ":", commB])

isJustListen (Just (Listen _ _ _)) = True
isJustListen _ = False

inputs :: String -> String -> IO (IPMap, [OpenFile])
inputs ipaddrPairsFile lsofPath = do
    ipmap <- BS.readFile ipaddrPairsFile >>= return . mapFromIpaddrs
    _:_:lsofs <- getDirectoryContents lsofPath
    connections <- mapM (\f -> BS.readFile (lsofPath ++ "/" ++ f) >>= return . lsofOpenFiles ipmap f) lsofs >>= return . concat
    return (ipmap, connections)

graph :: IPMap -> [OpenFile] -> [Edge]
graph ipaddrs connections = L.nub $ map edge $ catMaybes $ map (stream ipaddrs) pairs
    where
      srcmap = srcMap connections
      pairs = linkFiles connections srcmap
      -- nice to have: actually show lonely nodes (i.e. daemons that
      -- nobody's connected to) and connections that go outside

graphStr edges = iolist [["graph G {\n"], map render edges, ["}\n"]]
    where
      iolist = C.concat . join
      render (Edge a b) = C.concat ["\"", a, "\" -- \"", b, "\";\n"]

main :: IO ()
main = do
    [ipaddrPairsFile, lsofPath] <- getArgs
    (ipaddrs, connections) <- inputs ipaddrPairsFile lsofPath

    C.putStrLn $ graphStr $ graph ipaddrs connections 
