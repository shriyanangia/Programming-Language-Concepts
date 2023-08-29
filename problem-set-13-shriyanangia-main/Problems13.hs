module Problems13 where
import Bird.Parser
{-------------------------------------------------------------------------------
CS:3820 Fall 2022 Problem Set 13
=============================================
This problem applies parser combinators to several parsing problems.
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
Problem 1
---------
The first problem is to parse strings of equal numbers of 'a's and 'b's.  For
example, the strings "aaabbb" and "ab" should parse, while the string "aaaabb"
does not.  You should include the empty string as having an equal number of 'a's
and 'b's.  Note that your parser may not consume all the input!  For example,
given a string "aaabbbb", your parser should consume "aaabbb" and leave the
final 'b' on the input.
-------------------------------------------------------------------------------}
balanced :: Parser [Char]
balanced = option "" helperFunct 
    where
        helperFunct :: Parser [Char]
        helperFunct = do
            char 'a'
            result <- balanced
            char 'b'
            return ('a': result ++ "b")
-- >>> runParser balanced "aaabbb"
-- [("aaabbb","")]
-- >>> runParser balanced "aaaaaaabbbbbbbbb"
-- [("aaaaaaabbbbbbb","bb")]
-- >>> runParser balanced "ccdd"
-- [("","ccdd")]
-- >>> runParser balanced "aaaaaabbbbb"
-- [("","aaaaaabbbbb")]
{-------------------------------------------------------------------------------
Problem 2
---------
The second problem is to write a parser combinator `manyn` that parses a fixed
number of repetitions of its input parser.  For example, the parser `manyn 3
(char 'a')` would succeed on input "aaa", but fail on input "aa".
-------------------------------------------------------------------------------}
manyn :: Int -> Parser a -> Parser [a]
manyn n p
    | n == 0 = return []
    | otherwise = manynHelper 
    where 
        manynHelper = do 
            r <- p
            result <- manyn (n-1) p
            return (r: result)
{-------------------------------------------------------------------------------
Problem 3
---------
The third problem is to write a parser `balanced3` that parses strings of equal
numbers of 'a's, 'b's, and 'c's. For example, the strings "abc", "aaabbbccc",
and "" all parse, while the string "aaabbcc" and "aaabbbcc" do not.  The `manyn`
combinator from the previous problem should prove useful in writing the
`balanced3`.
-------------------------------------------------------------------------------}
{-
balanced :: Parser [Char]
balanced = option "" helperFunct 
    where
        helperFunct :: Parser [Char]
        helperFunct = do
            char 'a'
            result <- balanced
            char 'b'
            return ('a': result ++ "b")
-}

balanced3 :: Parser [Char]

{- ROUGH WORK I COMMENTED OUT
balanced3 = option "" helperFunct2
    where 
        helperFunct2 :: Parser [Char]
        helperFunct2 = do 
            char 'a'
            result <- manyn 1 (char 'a') --result is of type [Char]
            --result2 <- manyn (length result) (char 'a')
            char 'b'
            result3 <- (manyn (length result) (char 'b'))
            char 'c'
            return ("a" ++ "b" ++ "c")
-}

balanced3 = option "" $
    do 
        --char 'a'
        result <- many (helperFunct2('a'))
        --(manyn (length result) (char 'a'))            
        --char 'b'
        result2 <- (manyn (length result) (helperFunct2('b')))
        --char 'c'            
        result3 <- (manyn (length result) (helperFunct2('c')))

        return ((replicate (length result) ('a')) ++ (replicate (length result) ('b')) ++ (replicate (length result) ('c')))

           -- manyn 3 (char 'a')

helperFunct2 :: Char -> Parser Char
helperFunct2 a = do 
    char a
    return a


-- replicate the strings "a", "b", and "c"
-- each has the length of the of the corresponding sequence of repeated characters
-- This is precisely length result
-- replicate :: Int -> a -> [a]


-- >>> runParser balanced3 "aaabbbccc"
-- [("aaabbbccc","")]
-- >>> runParser balanced3 "aaaabbbccc"
-- [("","aaaabbbccc")]
-- >>> runParser balanced3 "aaabbbcccc"
-- [("aaabbbccc","c")]
-- >>> runParser balanced3 "dddeeefff"
-- [("","dddeeefff")]
{-------------------------------------------------------------------------------
Problem 4
---------
The final task is to define a parser for floating point numbers, following this
pattern:
    (-)?{digit}*(.{digit}+)?
That is, the following are all valid examples of float point numbers:
    4
    4.1
    -4
    -4.1
    .2
    -.34
    -0
    -0.0
The following are not
    4.
    --4
    -
    -.
Your parser should return the value parsed, as a Haskell `Double`
-------------------------------------------------------------------------------}

float :: Parser Double
float = undefined
{- ROUGH WORK I COMMENTED OUT 
float = do 
    ns <- many1 digit 
    --char '.'
    deci <- many1 digit 
    return (number[ns] ++ deci)
        where
            number [] = 0
            number (n: ns) = (n/10) + number ns

-}


{-
nat, natural :: Parser Int
nat = do ds <- many1 digit
         return (number (reverse ds))
    where number [] = 0
          number (d:ds) = d + 10 * number ds
natural = token nat
int :: Parser Int
int = token (do f <- option id (string "-" >> return negate)
                n <- nat
                return (f n))
-}

-- >>> runParser float "25"
-- [(25.0,"")]
-- >>> runParser float "2.5"
-- [(2.5,"")]
-- >>> runParser float ".5"
-- [(0.5,"")]
-- >>> runParser float "-2.5"
-- [(-2.5,"")]
-- >>> runParser float "-.2551"
-- [(-0.2551,"")]
-- >>> runParser float "4."
-- [(4.0,".")]
-- >>> runParser float "-ab3"
-- []
