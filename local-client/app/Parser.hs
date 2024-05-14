module Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..), ap, liftM)

newtype Parser t a = Parser {parse :: [t] -> [(a, [t])]}

result :: a -> Parser t a
result val = Parser $ \inp -> [(val, inp)]

instance Monad (Parser t) where
  p >>= f = Parser $ \inp ->
    concat [parse (f v) inp' | (v, inp') <- parse p inp]

instance Applicative (Parser t) where
  pure = result
  (<*>) = ap

instance Functor (Parser t) where
  fmap = liftM

instance Alternative (Parser t) where
  empty = zero
  p <|> q = Parser $ \inp -> case parse (p `plus` q) inp of
    [] -> []
    (x : _) -> [x]

instance MonadPlus (Parser t) where
  mzero = zero
  mplus = plus

zero :: Parser t a
zero = Parser $ const []

plus :: Parser t a -> Parser t a -> Parser t a
plus p q = Parser $ \inp -> parse p inp ++ parse q inp

then' :: (a -> b -> c) -> Parser t a -> Parser t b -> Parser t c
then' combine p q =
  p >>= \x ->
    q >>= \xs ->
      result $ combine x xs

tokenMatching :: (t -> Bool) -> Parser t t
tokenMatching c = Parser parseCond
 where
  parseCond [] = []
  parseCond (x : xs) = if c x then [(x, xs)] else []

tokenMatchingInto :: (t -> Bool) -> a -> Parser t a
tokenMatchingInto c = (tokenMatching c >>) . result

many' :: Parser t a -> Parser t [a]
many' p =
  do
    x <- p
    xs <- many' p
    return (x : xs)
    <|> return []

remainder :: Parser t [t]
remainder = many' . tokenMatching $ const True
