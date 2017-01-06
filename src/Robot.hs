module Robot (
robot,
place,
move,
right,
left,
report,
Robot(..),
Direction(..)
) where

import Control.Monad.State

data Direction = North | East | South | West deriving (Enum,Show,Eq)

type X = Integer
type Y = Integer

data Robot = Robot{p::Bool,x::X, y::Y,d::Direction} deriving (Eq,Show)

maxX = 5
maxY = 5

minX = 0
minY = 0

-- Robot identity function
robot :: Robot
robot = Robot False minX minY North

-- Checks a tabletop boundaries
-- Places a robot on a tabletop by returning a State Robot or ()
-- Initializes a new state inside monad or overwrites a current one
place :: X -> Y -> Direction -> State Robot ()
place x y d = if (x < minX || x > maxX || y < minY || y > maxY ) then return () else put(Robot True x y d)

-- Handles right rotation by applying pattern matching then cloning a current state
-- Modifies Direction depending on a current state
right :: State Robot ()
right = modify (\r -> case r of
    Robot False _ _ _  -> r
    Robot True _ _ North -> r { d = East }
    Robot True _ _ West ->  r { d = North }
    Robot True _ _ South -> r { d = West }
    Robot True _ _ East ->  r { d = South })

-- Handles left rotation by applying pattern matching then cloning a current state
-- Modifies Direction depending on a current state
left :: State Robot ()
left = modify (\r -> case r of
    Robot False _ _ _  -> r
    Robot True _ _ North -> r { d = West }
    Robot True _ _ West ->  r { d = South }
    Robot True _ _ South -> r { d = East }
    Robot True _ _ East ->  r { d = North })

-- Handles movement by applying pattern matching then cloning a current state
-- Increases or decreases X or Y depending on a current state
move :: State Robot ()
move = modify (\r -> case r of
    Robot False _ _ _ -> r
    Robot True _ y North -> if (y >= maxY ) then r else r { y = y + 1 }
    Robot True x _ West -> if (x <= minX ) then r else r { x = x - 1 }
    Robot True _ y South -> if (y <= minY ) then r else r { y = y - 1 }
    Robot True x _ East -> if (x >= maxX ) then r else r { x = x + 1 })

-- Shows a current state of a robot
report :: State Robot String
report = do r <- get
            return (case r of
                        Robot False _ _ _  -> "Robot is NOT placed"
                        Robot True x y d -> show(r))