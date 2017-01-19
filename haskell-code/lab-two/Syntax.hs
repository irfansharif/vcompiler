module Syntax where

data Program = Program [Waveform] 
             deriving (Show)

data Waveform = Waveform ID Bits 
              deriving (Show)

data ID = ID String 
        deriving (Show)

data Bits = Bits [Int] 
          deriving (Show)
