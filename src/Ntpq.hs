{-# LANGUAGE OverloadedStrings #-}

module Ntpq (doNtpq) where

import Formatting
import Formatting.Formatters
import Types
import Data.Text
import qualified Data.Text.IO as T

formatString :: Format r (String -> String -> String -> String -> String -> String -> String -> String -> r)
formatString = "_" % right 18 ' ' % " " % right 15 ' ' % " " % string % " ? ?? ??    " % string % " " % left 7 ' ' % " " % left 8 ' ' % " " % left 7 ' ' % " = " % string

doNtpq :: [Foo] -> IO ()
doNtpq fs = do
  let
    go f = do
      T.putStrLn $ sformat formatString (srcadr f) (refid f) (stratum f) (reach f) (delay f) (offset f) (jitter f) (keyid f)
  T.putStrLn "     remote           refid      st t when poll reach   delay   offset  jitter"
  T.putStrLn "=============================================================================="
  mapM_ go fs
