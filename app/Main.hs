module Main where

import Robot
import Control.Monad.State

-- This is an example of robot movements
-- I made a decision to stick to a library style implementation. Therefore, this is how a client code should look like
p1 = do place 0 0 North
        right
        move
        move
        p <- report
        return p

(st, r) = runState p1 robot

main :: IO ()
main = putStrLn st

-- I carefully studied a toy robot specification, and
-- I covered all possible use cases through tests that I created in /test/Spec.hs

-- This is a standart stack project. So, it supports all standart stack console commands.
-- Use <stack test> to run tests
-- Use <stack build> to compile this project
-- In order to run this example use the following command after <stack build> ;)
-- <./robot-exe>

-- Here is an example path structure where <robot-exe> should be, it might vary because of OS and compiler versions
-- </robot/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/bin>
