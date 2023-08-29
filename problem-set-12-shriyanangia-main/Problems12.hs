{-# LANGUAGE FlexibleContexts #-}
module Problems12 where

import Control.Monad
import Control.Monad.State

--------------------------------------------------------------------------------
-- Some test data

hollow :: String
hollow =
    "This is the dead land\n\
    \This is cactus land\n\
    \Here the stone images\n\
    \Are raised, here they receive\n\
    \The supplication of a dead man's hand\n\
    \Under the twinkle of a fading star."

fiftyFour =
    "I like a look of agony,\n\
    \Because I know it's true\n\
    \Men do not sham Convulsion,\n\
    \Nor simulate, a Throe\n\
    \\n\
    \The Eyes glaze once and that is death\n\
    \Impossible to feign\n\
    \The Beads upon the Forehead\n\
    \By homely Anguish strung."

{-------------------------------------------------------------------------------

CS:3820 Fall 2022 Problem Set 12
================================

This problem concerns using monads.  In particular, we're going to focus on the
state and IO monads.  The *state monad* encapsulates stateful operations---that
is to say, operations that can both read from and write to some implicit shared
value.  The *IO* monad captures input/output operations, such as reading from or
writing to files or the console.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Problem 1
---------

For the first part, you need to write an operation, in the state monad, that
adds a running word total to a string.  The idea is basically that, given an
input string like

    Men do not sham Convulsion,

We want to produce an output string like

    Men do not sham Convulsion,                                                  (5)

where:

 - 5 is the running total number of words after this line
 - the count is *right*-aligned at column 80

So, the output string before would be correct *if it were the first string*.
However, if we'd already seen 7 characters, then the correct output string
would be

    Men do not sham Convulsion,                                                 (12)

The challenge, of course, is knowing how many words we'd seen already---and
recording the new total going forward.  This is where the *state* monad comes
in.  In addition to the normal monad operations (`return` and `>>=`, and `do`
notation), state monads provide operations `get` and `put`.  The `get` operation
retrieves a value stored within the monad, while the `put` value updates it.

Your `addConst` function will live in a monad where the only state is the
running total of characters.  Your function should not just insert the new
running total in the returned string, but also update the running total in the
monad.

The type of `addCount` is *parametric* over an arbitrary monad that satisfies
the `MonadState Int` type class.  This ensures that *all* you can rely on are
the monad and state operations, not any details of a particular implementation
of the state monad.

The `Data.Text` module includes functions that might seem to help with
right-aligning the count; while you can use them, if you try hard enough, it's
easier to just use the list functions we've seen already.  Don't forget about
`replicate`!  You may assume that no line will be long enough that the text of
the line and the running total would overlap.

-------------------------------------------------------------------------------}

addCount :: MonadState Int m => String -> m String
addCount s = do 
    variable <- get
    put (variable + countWords)
    variable <- get 
    let numSpaces
            | variable < 10 = concat (replicate (77 - length s) (" "))
            | variable < 100 = concat (replicate (76 - length s) (" "))
            | otherwise    = concat (replicate (75 - length s) (" "))
    return (s ++ numSpaces ++ "(" ++ (show variable) ++ ")" )
    where countWords = length (words s) 



    
-- >>> runState (addCount "She should have died hereafter;") 0
-- ("She should have died hereafter;                                              (5)",5)
-- >>> runState (addCount "She should have died hereafter;") 16
-- ("She should have died hereafter;                                             (21)",21)

{-------------------------------------------------------------------------------

Problem 2
---------

For this problem, you should write a function that adds the running word count
to a *list* of strings.  Of course, you should use your solution to part 1.
However, there's one additional catch: you should only print the running total
on lines that aren't blank!  (You don't have to worry about non-printing
characters: a line of spaces or tab characters is not blank.)  You might find
the `mapM` function, of type

    Monad m => (a -> m b) -> [a] -> m [b]

helpful.

-------------------------------------------------------------------------------}
{- -- maps a monadic function across a list, discarding the results
mapM  :: Monad m => (a -> m b) -> [a] -> m ()
mapM = undefined -}

addCounts :: MonadState Int m => [String] -> m [String]
addCounts = mapM ac where
    ac [] = return [] 
    ac x = addCount x
-- >>> runState (addCounts (take 2 (lines hollow))) 0
-- (["This is the dead land                                                        (5)","This is cactus land                                                          (9)"],9)


{-------------------------------------------------------------------------------

Problem 3
---------

Finally, you should write a function that, given an input string, prints that
string to standard output with running character totals.  For example, given the
poem above, your function should behave as follows:

*Problem7> printLines fiftyFour
I like a look of agony,                                                      (6)
Because I know it's true --                                                 (12)
Men do not sham Convulsion,                                                 (17)
Nor simulate, a Throe --                                                    (22)

The Eyes glaze once -- and that is death                                    (31)
Impossible to feign                                                         (34)
The Beads upon the Forehead                                                 (39)
By homely Anguish strung.                                                   (43)

Because your solution runs in the IO monad, you won't be able to execute it in a
comment within this file.  Instead, you'll need to use the Haskell interpreter
from the command line.  Give the command `cabal repl` to start the interpreter
with this file loaded.

You may find the following functions helpful:
    putStrLn :: String -> IO ()
    -- prints the input string, followed by a newline
    mapM_    :: Monad m => (a -> m b) -> [a] -> m ()
    -- maps a monadic function across a list, discarding the results
    runState :: MonadState s m => m a -> s -> (a, s)
    -- executes a computation in the state monad, requiring an initial state
    -- and returning the final result and the final state.

-------------------------------------------------------------------------------}
{- -- prints the input string, followed by a newline
putStrLn :: String -> IO ()
putStrLn = undefined

-- executes a computation in the state monad, requiring an initial state
-- and returning the final result and the final state
runState :: MonadState s m => m a -> s -> (a, s)
runState = undefined
 -}
printLines :: String -> IO ()
printLines s = mapM_ putStrLn (flip evalState 0 (addCounts (lines s)))
