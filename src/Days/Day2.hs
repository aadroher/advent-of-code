{-# LANGUAGE OverloadedStrings #-}

module Days.Day2 where

import Data.List.Split (chunksOf, splitOn)
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T

-- import Text.Pretty.Simple (pPrint)

type State = [Integer]

type Quartet = (Integer, Integer, Integer, Integer)

data Op = Add | Mult | End
  deriving (Eq)

type Pos = Integer

data Instruction = Instruction
  { op :: Op,
    nounAddr :: Pos,
    verbAddr :: Pos,
    resultAddr :: Pos
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
      nounAddr = b,
      verbAddr = c,
      resultAddr = d
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
executeInstruction s Instruction {op = o, nounAddr = i, verbAddr = j, resultAddr = k} =
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

setInitialState :: State -> Integer -> Integer -> State
setInitialState (res : _ : _ : ns) noun verb = res : noun : verb : ns
setInitialState _ _ _ = error "State does not have valid format"

getResult :: State -> Integer -> Integer -> Integer
getResult s noun verb =
  let initialState = setInitialState s noun verb
   in head $ executeIntCode initialState

resultIn :: State -> Integer -> Integer -> Integer -> Bool
resultIn m s v expectedResult = getResult m s v == expectedResult

loadData :: FilePath -> IO [Integer]
loadData f = do
  fileContents <- readFileUtf8 f
  pure $ read <$> splitOn "," (T.unpack fileContents)

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult p = do
  let initialNoun = 12
  let initialVerb = 2
  initialMemory <- loadData p
  let result = getResult initialMemory initialNoun initialVerb
  pure $ (T.pack . show) result

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult p = do
  initialMemory <- loadData p
  let expectedResult = 19690720
  let ns = [0 .. 99]
  let cs = [(x, y) | x <- ns, y <- ns]
  let matches = \(x, y) -> resultIn initialMemory x y expectedResult
  case L.find matches cs of
    Nothing -> error "Solution not found"
    Just (noun, verb) -> pure $ (T.pack . show) (100 * noun + verb)
