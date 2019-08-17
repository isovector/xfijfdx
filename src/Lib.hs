{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Bifunctor
import Data.Char
import Control.Lens
import Data.Tuple

type Parser s a = Prism' s (a, s)


ffmap :: (Functor f, Bifunctor p) => (a -> b) -> f (p a c) -> f (p b c)
ffmap f = fmap (first f)

space :: Parser String ()
space = prism' ((' ' :) . snd)
               ( ffmap (const ())
               . preview (many $ satisfy isSpace)
               )

anyChar :: Parser String Char
anyChar = prism' (uncurry (:)) (uncons)

satisfy :: (a -> Bool) -> Parser [a] a
satisfy f = prism' (uncurry (:)) $ \zs ->
  case uncons zs of
    z@(Just (a, as)) | f a -> z
    _                      -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

many :: Parser s a -> Parser s [a]
many p =
  prism'
    (\(la, s) -> foldr (curry $ review p) s la)
    (\s ->
      case preview p s of
        Just (a, s') -> do
          (as, s'') <- preview (many p) s'
          pure $ (a : as, s'')
        Nothing -> pure ([], s)
      )

pprint :: Monoid s => Parser s a -> a -> s
pprint p = review p . (, mempty)

-- apP :: Parser s (a -> b) -> Parser s a -> Parser s b
-- apP f a = prism' (\(b, s) -> rev) undefined

