{-# LANGUAGE ScopedTypeVariables #-}
module Carnifex.Reltime
  (durationParser
  ) where

import Data.Char
import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (pack)

durationParser :: forall a . (Fractional a) => Parser a
durationParser = do
  sign <- (char '-' >> return (-1)) <|> return 1
  (_, h) <- runScanner 0 (\s c -> (+10*s) <$> charToNum c)
  char ':'
  ss <- count 2 $ satisfy isDigit
  char ':'
  mm <- count 2 $ satisfy isDigit
  let read = Prelude.read :: String -> Int
      m = fromIntegral $ read mm :: a
      s = fromIntegral $ read ss :: a
  (us :: Int) <- (char '.' >> read <$> count 3 (satisfy isDigit)) <|> return 0
  return $! sign * ((fromIntegral us)/1e3 + s + 60*(m + 60*h))


charToNum :: (Fractional a) => Char -> Maybe a
charToNum c =
  if isDigit c
    then Just $ fromIntegral $ fromEnum c - fromEnum '0'
    else Nothing
