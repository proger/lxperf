{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)

import System.Process (shell, createProcess, waitForProcess, StdStream(..), CreateProcess(..))
import System.IO
import System.IO.Error
import System.Exit (exitSuccess)
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Either (rights, lefts)
import Data.List (transpose)

import qualified Text.PrettyPrint.Boxes as P
import Text.PrettyPrint.Boxes ((//), (<+>))

-- * commands

shell' s = (shell s){ std_out = CreatePipe, std_err = Inherit, std_in = Inherit }

#if defined(linux_HOST_OS)
ss = do
  (_, Just hout, _, ph) <- createProcess (shell' "ss -n -ep -t -o state established")
  return (hout, Just ph)
#else
ss = do
  out <- openFile "sample.txt" ReadMode
  return (out, Nothing)
#endif

consume p f = do
  (handle, mph) <- p
  contents <- B.hGetContents handle
  ret <- evaluate $ f contents
  case mph of
      Just ph -> const () <$> waitForProcess ph
      Nothing -> return ()
  hClose handle
  return ret

-- * aggregations

count = foldr (\x -> Map.insertWith (+) x 1) Map.empty

-- * parsing

isSep = return (||) `ap` A8.isEndOfLine `ap` A8.isHorizontalSpace
skip = A.skipWhile A8.isEndOfLine
sep = A.skipWhile isSep
word = A.takeWhile (not . isSep)
field = word <* sep
manyWords = (some (A8.satisfy A8.isAlpha_ascii)) `A8.sepBy1` (A8.char ' ')

commad = (`A8.sepBy1` ",")
quoted = A8.char '"' *> A8.takeWhile (/= '"') <* A8.char '"'

triple = "(" *> go <* ")" where go = do
                                      comm <- quoted <* ","
                                      pid <- many "pid=" *> A8.decimal <* ","
                                      _ <- many "fd=" *> A8.decimal
                                      return $ User comm pid

-- * socket users

data User = User ByteString Integer {- comm pid -}
          deriving (Show, Ord, Eq)

users = concat . rights . fmap line . B.lines
  where
    line :: ByteString -> Either String [User]
    line c | "Recv-Q" `B.isPrefixOf` c = Left "skip"
    line s = A.parseOnly parser s

    parser = do
        _recvq <- field
        _sendq <- field
        _local <- field
        _peer <- field
        eitherUser1 <- A.eitherP user field
        eitherUser2 <- A.eitherP user field
        eitherUser3 <- A.eitherP user field
        skip
        let eithers = [eitherUser1, eitherUser2, eitherUser3]
        return $ case lefts eithers of
                     [] -> fail "couldn't find users"
                     x:_ -> x
      where user = "users:(" *> commad triple <* ")"

agg = count
connStats = Map.toList . agg . users 

-- * limits

data LimitValue = Unlimited | Value Integer deriving (Show)
data Limit = Limit
           { limitTag :: String
           , limitSoft :: LimitValue
           , limitHard :: LimitValue
           , limitUnit :: String
           } deriving (Show)

limitValue = ("unlimited" *> pure Unlimited) <|> (Value <$> A8.decimal)

limitLine = do
  tag <- manyWords <* sep
  soft <- limitValue <* sep
  hard <- limitValue <* sep
  unit <- field <* skip
  return $ Limit (unwords tag) soft hard (B.unpack unit)

limits pid =
  let
#if defined(linux_HOST_OS)
    limitFile = "/proc/" ++ show pid ++ "/limits"
#else
    limitFile = "limits.txt"
#endif
  in withFile limitFile ReadMode $ \handle -> do
    limits <- map (A.parseOnly limitLine) . tail . B.lines <$> B.hGetContents handle
    return $ rights limits

limitNoFile = head . filter (\Limit{..} -> limitTag == "Max open files")

showLimit1 (Limit _ soft hard _) = show' soft ++ "/" ++ show' hard
  where
    show' Unlimited = "unlimited"
    show' (Value x) = show x

-- * printing

toBox :: [[String]] -> P.Box
toBox rows = P.hsep 2 P.left dat
  where
    dat = map (P.vcat P.left . map P.text) (transpose (["PROC", "CONNS", "FDLIMIT"]:rows))

main = do
  forever $ do
    now <- getPOSIXTime
    print now
    stats <- consume ss connStats
    rows <- mapM (\(u@(User _ pid), count) -> limitNoFile <$> limits pid >>= \l -> return [show u, show count, showLimit1 l]) stats
    P.printBox . toBox $ rows
    threadDelay (10^6)
