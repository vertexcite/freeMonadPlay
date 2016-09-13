-- Based on this blog: http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad.Free
import Control.Exception

data Toy b next = Output b next | Input (b -> next) | Bell next | Done deriving (Functor)

input :: Free (Toy a) a -- Input was not in the original article
input = liftF (Input id)

output :: a -> Free (Toy a) ()
output x = liftF (Output x ())

bell :: Free (Toy a) ()
bell     = liftF (Bell ())

done :: Free (Toy a) r
done     = liftF  Done

-- Interpreter of IO for listing program (under a certain input scenario)
showProgram :: (Show a, Show r) => Free (Toy a) r -> a -> String
showProgram (Free (Input f   )) y = "input gets " ++ show y ++ "\n" ++ showProgram (f y) y
showProgram (Free (Output a x)) y = "outputs " ++ show a ++ "\n" ++ showProgram x y
showProgram (Free (Bell x    )) y = "bell\n" ++ showProgram x y
showProgram (Free Done        ) _ = "done\n"
showProgram (Pure r           ) _ = "return " ++ show r ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> a -> IO ()
pretty y = putStr . showProgram y

-- Interpreter of DSL for IO
ringBell :: IO ()
ringBell = putStrLn "BELL"

interpret :: (Show b, Read b) => Free (Toy b) r -> IO ()
interpret (Free (Input f   )) = readLn >>= interpret . f
interpret (Free (Output b x)) = print b  >> interpret x
interpret (Free (Bell     x)) = ringBell >> interpret x
interpret (Free  Done       ) = return ()
interpret (Pure r           ) = throwIO (userError "Improper termination")

-- Small little example program
subroutine :: Free (Toy Char) ()
subroutine = do
  x <- input
  output x

program :: Free (Toy Char) Int
program = do
  subroutine
  bell
  done


-- Demo Monad Laws hold
l1 = showProgram (return 'A' >>= output) '_' == showProgram (output 'A') '_'
l2 = showProgram (output 'A' >>= return) '_' == showProgram (output 'A') '_'
l3lhs = showProgram ((output 'A' >> done) >> output 'C') '_'
l3rhs = showProgram (output 'A' >> (done >> output 'C')) '_'
l3 = l3lhs == l3rhs

-- Main
main :: IO ()
main = do
  putStrLn "Listing of program"
  pretty program 'x'
  putStrLn ""

  putStrLn "Run program requires user input, exception unless of form 'x' (where x can be any single char, and single quotes are required)."
  interpret program
  putStrLn ""

  putStrLn $ "Monad laws test result: " ++ if and [l1, l2, l3] then "pass" else "fail"
