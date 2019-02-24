{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import           Network.StatsD.Datadog (withDogStatsD, defaultSettings, send, metric, MetricType(Gauge), MetricName(MetricName))
import           Network.NTP.Control (queryHost)
import           Network.NTP.Control.Packet (emptyPacket, op, associationID, data_, Op(ReadStatus, ReadVariables))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import           Data.Binary.Get (Get, runGet, getWord16be)
import           Data.Word           (Word16)
import           Control.Applicative ()
import           Control.Monad (void, forever)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Text.Megaparsec (Parsec, someTill, parse, (<|>), many, label, takeWhile1P, anySingle)
import           Text.Megaparsec.Char (string)
import           Data.Void (Void)
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (fromJSON, Value(String, Object), Result(Success))
import           Types (Variables(offset))
import           Control.Concurrent     (threadDelay)
import           Data.String            (IsString, fromString)
import           Data.Maybe (mapMaybe)

instance IsString MetricName where
  fromString = MetricName . T.pack

main :: IO ()
main = do
  withDogStatsD defaultSettings $ \client -> do
    forever $ do
      offset <- getAverageOffset
      send client $ metric "ntpd.avg_offset" Gauge offset
      threadDelay (1000 * 1000 * 10)

getAverageOffset :: IO Double
getAverageOffset = do
  packet <- queryHost "127.0.0.1" (emptyPacket { op = ReadStatus })
  let
    peers = runGet test (BSL.fromStrict $ data_ packet)
  let
    go :: (Word16, Word16) -> IO (Maybe Variables)
    go (id, _) = do
      packet2 <- queryHost "127.0.0.1" (emptyPacket { op = ReadVariables, associationID = id })
      let res2 = parse p2 "filename" (T.decodeUtf8 (data_ packet2))
      case res2 of
        Right (Success res2') -> pure $ Just res2'
        Left fail -> do
          print fail
          pure Nothing
  results' <- mapM go peers
  let results = mapMaybe id results'
  --doNtpq results
  let
    avgOffset = (foldr (\result sum -> sum + (read $ offset result)) 0.0 results) / (fromIntegral $ length results)
  print avgOffset
  return avgOffset

getpair :: Get (Word16, Word16)
getpair = (,) <$> getWord16be <*> getWord16be

test :: Get [ (Word16, Word16) ]
test = do
  res <- many getpair
  pure $ reverse res

test2 :: BS.ByteString
test2 = "srcadr=158.69.125.231, srcport=123, dstadr=192.168.2.15, dstport=123,\r\nleap=0, stratum=2, precision=-25, rootdelay=25.330, rootdisp=35.202,\r\nrefid=142.66.101.13, reftime=0xdeca6eed.1ef84caa,\r\nrec=0xdeca7154.34fc251b, reach=0xff, unreach=0, hmode=3, pmode=4,\r\nhpoll=6, ppoll=6, headway=260, flash=0x0, keyid=0, offset=-0.114,\r\ndelay=41.597, dispersion=4.463, jitter=0.366, xleave=0.041,\r\nfiltdelay= 41.72 43.03 42.71 43.06 42.88 42.00 43.09 41.60,\r\nfiltoffset= -0.11 0."


type Parser = Parsec Void T.Text

p2 :: Parser (Result Variables)
p2 = do
  list <- many $ label "p3" p3
  let map = Object $ HM.fromList list
  pure (fromJSON $ map)

p3 :: Parser (T.Text, Value)
p3 = do
  key <- someTill anySingle (string "=")
  value <- takeWhile1P (Just "value") (\char -> case char of
    ',' -> False
    _ -> True)
  (void . string) ", " <|> (void.string) ",\r\n" <|> pure ()
  pure (T.pack key, String value)

dotest :: IO ()
dotest = do
  BSC.putStrLn test2
  let res2 = parse p2 "filename" (T.decodeUtf8 test2)
  print res2
