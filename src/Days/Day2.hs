{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days.Day2 where

import Data.List.Split (chunksOf, splitPlaces)
import RIO
import qualified RIO.List as L
import RIO.List.Partial ((!!))
import qualified RIO.Text as T
import Text.Pretty.Simple (pPrint)

type State = [Integer]

type Quartet = (Integer, Integer, Integer, Integer)

data Op = Add | Mult | End
  deriving (Eq)

type Pos = Integer

data Instruction = Instruction
  { op :: Op,
    src0 :: Pos,
    src1 :: Pos,
    dest :: Pos
  }

decodeOp :: Integer -> Op
decodeOp 1 = Add
decodeOp 2 = Mult
decodeOp 99 = End
decodeOp _ = error "Operation unknown"

decode :: Quartet -> Instruction
decode (a, b, c, d) =
  Instruction
    { op = decodeOp a,
      src0 = b,
      src1 = c,
      dest = d
    }

bitsToQuartet :: [Integer] -> Quartet
bitsToQuartet is@(o : ns) = case decodeOp o of
  End -> case ns of
    [a, b, c] -> (o, a, b, c)
    _ -> (o, 0, 0, 0)
  _ -> case ns of
    [a, b, c] -> (o, a, b, c)
    _ -> error ("Dont know how to parse " ++ show is)
bitsToQuartet ns = error ("Dont know how to parse " ++ show ns)

getInstructions :: State -> [Instruction]
getInstructions ns = decode . bitsToQuartet <$> chunksOf 4 ns

writeAt :: State -> Integer -> Integer -> State
writeAt s k x = case L.splitAt (fromInteger k) s of
  (ns, _ : ms) -> ns ++ [x] ++ ms
  (ns, []) -> ns ++ [x]

executeInstruction :: State -> Instruction -> State
executeInstruction s Instruction {op = o, src0 = i, src1 = j, dest = k} =
  case o of
    Add -> writeAt s k (a + b)
    Mult -> writeAt s k (a * b)
    End -> s
  where
    a = s !! fromInteger i
    b = s !! fromInteger j

execute :: State -> Integer -> State
execute oldState pointer =
  if o == End
    then oldState
    else
      ( let newState = executeInstruction oldState i
         in execute newState (pointer + 1)
      )
  where
    i@Instruction {op = o} = getInstructions oldState !! fromInteger pointer

executeIntCode :: State -> State
executeIntCode ns = execute ns 0
