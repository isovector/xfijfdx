{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Lib where

import Data.List hiding (uncons)
import Control.Monad
import Data.Foldable
import Control.Arrow hiding (first, second)
import Data.Bifunctor
import Data.Char
import Control.Lens hiding (cons)
import Data.Tuple

type Parser s a = Iso' (Maybe s) (Maybe (a, s))


ffmap :: (Functor f, Bifunctor p) => (a -> b) -> f (p a c) -> f (p b c)
ffmap f = fmap (first f)

-- space :: Parser String ()
-- space = prism' ((' ' :) . snd)
--                ( ffmap (const ())
--                . preview (many $ satisfy isSpace)
--                )

cons :: (a, [a]) -> [a]
cons = uncurry (:)

isoM :: Iso' a b -> Iso' (Maybe a) (Maybe b)
isoM = mapping

anyChar :: Parser String Char
anyChar = iso (>>= uncons) (fmap cons)

isoP
    :: (Monad m)
    => (a -> m b)
    -> (b -> m a)
    -> Iso' (m a) (m b)
isoP f g = iso (>>= f) (>>= g)

satisfy :: (a -> Bool) -> Parser [a] a
satisfy f = isoP fuck you_chris
  where
    fuck (a : as) = do
      guard $ f a
      pure (a, as)

    you_chris (a, as) = do
      guard $ f a
      pure (a : as)

char :: Eq a => a -> Parser [a] a
char c = satisfy (== c)

many :: Monoid s => Parser s a -> Parser s [a]
many p = isoP fuck you_chris
  where
    fuck s = do
      (a, s')   <- view p $ Just s
      (as, s'') <- view (many p) $ Just s'
      pure ((a : as), s'')

    you_chris (as, s) =
      fmap ((<> s) . fold)
        $ traverse (review p . Just . (, mempty)) as

-- tupleP :: Monoid s => Parser s a -> Parser s b -> Parser s (a, b)
-- tupleP pa pb =
--   prism' (\((a, b), s) ->
--     let s' = review pb (b, s)
--      in review pa (a, s')
--     ) $ \s -> do
--   (a, s')  <- preview pa s
--   (b, s'') <- preview pb s'
--   pure ((a, b), s'')


-- firstP :: Prism' s (a, b) -> Iso' a a' -> Prism' s (a', b)
-- firstP p i = prism' (\z -> review p $ z & _1 %~ view (from i)) $ \s -> do
--   z <- preview p s
--   pure $ z & _1 %~ view i


string :: Eq a => [a] -> Parser [a] [a]
string str = isoP (fmap (str, ) . stripPrefix str)
  \(str', lo) -> do
      guard $ str == str'
      pure $ str' <> lo

-- apP :: Parser s (AnIso' a b) -> Parser s a -> Parser s b
-- apP pf pa =
--   isoP (\s -> do
--     (f, s')  <- view pf $ Just s
--     (a, s'') <- view pa $ Just s'
--     pure (view (cloneIso f) a, s'')
--     )
--   _

appP
    :: Iso' s (s', AnIso' a b)
    -> Iso' s' (s'', a)
    -> Iso' s (s'', b)
appP pf pa = iso undefined $ \(s'', b) -> _
