module Day02.Day02 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

data Instruction
    = Up
    | Down
    | Forward
    deriving Show

pInstruction :: Parser (Instruction, Int)
pInstruction = do
   inst <- (Up <$ string "up") <|> (Down <$ string "down") <|> (Forward <$ string "forward")
   space
   num <- L.decimal
   return (inst, num)




part1 = do
    fileLines <- lines <$> readFile "src/Day02/input.txt"
    case mapM (parse pInstruction "") fileLines of
      Left _ -> error "error"
      Right x -> print (uncurry (*) (foldl interpret (0, 0) x))
    where
        interpret ::  (Int, Int) -> (Instruction, Int) -> (Int, Int)
        interpret (accX, accY) (Up, val) = (accX, accY - val)
        interpret (accX, accY) (Down, val) = (accX, accY + val)
        interpret (accX, accY) (Forward, val) = (accX + val, accY)

part2 = do
    fileLines <- lines <$> readFile "src/Day02/input.txt"
    case mapM (parse pInstruction "") fileLines of
      Left _ -> error "error"
      Right instructions -> let (x, y, _) = foldl interpret (0, 0, 0) instructions
                            in print $ x * y
                
    where
        interpret ::  (Int, Int, Int) -> (Instruction, Int) -> (Int, Int, Int)
        interpret (accX, accY, aim) (Up, val) = (accX, accY, aim - val)
        interpret (accX, accY, aim) (Down, val) = (accX, accY, aim + val)
        interpret (accX, accY, aim) (Forward, val) = (accX + val, accY + (val * aim), aim)





