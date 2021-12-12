module Day2 where

type Magnitude = Integer
type HorizontalPosition = Integer
type Depth = Integer
type Aim = Integer
type State = (HorizontalPosition, Depth, Aim)
type Stepper = State -> Command -> State
data Direction = Forward | Up | Down
data Command = Command Direction Magnitude

parseDirection :: String -> Direction
parseDirection "forward" = Forward
parseDirection "up" = Up
parseDirection "down" = Down
parseDirection x = error ("unrecognised direction: " ++ show x)

readDirectionMagnitude :: [String] -> Command
readDirectionMagnitude [direction, magnitude] = Command (parseDirection direction) (read magnitude)
readDirectionMagnitude x = error ("unrecognised dir,mag pair: " ++ show x)

parse :: String -> [Command]
parse = map (readDirectionMagnitude . words) . lines

solve :: Stepper -> String -> Integer 
solve stepper = (\(hPos, depth, _) -> hPos * depth) . run stepper (0, 0, 0) . parse 

part1 :: String -> Integer
part1 = solve part1Step

part1Step :: State -> Command -> State
part1Step (hPos, depth, aim) (Command direction magnitude) = case direction of
  Forward -> (hPos + magnitude, depth, aim)
  Up -> (hPos, depth - magnitude, aim)
  Down -> (hPos, depth + magnitude, aim)

run :: (State -> Command -> State) -> State -> [Command] -> State
run = foldl

part2 :: String -> Integer 
part2 = solve part2Step

part2Step :: State -> Command -> State
part2Step (hPos, depth, aim) (Command direction magnitude) = case direction of
  Forward -> (hPos + magnitude, depth + (aim * magnitude), aim)
  Up -> (hPos, depth, aim - magnitude)
  Down -> (hPos, depth, aim + magnitude)