{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib (runAlex, parseTL)

main :: IO ()
main = do
  print $ runAlex "inputMediaUploadedPhoto#1e287d04 flags:# spoiler:flags.2?true file:InputFile stickers:flags.0?Vector<InputDocument> ttl_seconds:flags.1?int = InputMedia;" parseTL