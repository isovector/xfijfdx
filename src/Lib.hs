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
import Control.Applicative ((<|>))
import Data.Tuple

type Parser s a = Iso' (Maybe s) (Maybe (a, s))

ffmap :: (Functor f, Bifunctor p) => (a -> b) -> f (p a c) -> f (p b c)
ffmap f = fmap (first f)

space :: Parser String ()
space = isoP to' from'
  where
    to' :: String -> Maybe ((), String)
    to' (' ' : rest) = Just ((), rest)
    from' :: ((), String) -> Maybe String
    from' ((), s) = Just $ s <> " "
  --   to' = ((' ' :) . snd)
  --              ( ffmap (const ())
  --              . preview (many $ satisfy isSpace)
  --              )
  --   from' = ((' ' :) . snd)

takeLeft :: Iso' (Maybe ((a, ()), s)) (Maybe (a, s))
takeLeft = mapping (swapped . mapping (iso fst (,()))) . mapping swapped

-- takeRight :: Iso' (Maybe ((), a)) (Maybe a)
-- takeRight = mapping swapped . takeLeft

-- isoJoin :: Iso' (Iso' a b, a) b


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

tupleP :: forall s a b. Monoid s => Parser s a -> Parser s b -> Parser s (a, b)
tupleP pa pb = isoP to' from'
  where
    to' :: s -> Maybe ((a, b), s)
    to' s = do
        (a, s') <- Just s ^. pa
        (b, s'') <- Just s' ^. pb
        return ((a, b), s'')

    from' :: ((a, b), s) -> Maybe s
    from' ((a, b), s) = do
        s' <- review pb (Just (b, s))
        review pa (Just (a, s'))

eitherP :: forall s a b. Monoid s => Parser s a -> Parser s b -> Parser s (Either a b)
eitherP pa pb = isoP to' from'
  where
    to' :: s -> Maybe (Either a b, s)
    to' s = (first Left <$> Just s ^. pa) <|> (first Right <$> Just s ^. pb)
    from' :: (Either a b, s) -> Maybe s
    from' (Left a, s) = review pa (Just (a, s))
    from' (Right b, s) = review pb (Just (b, s))

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

-- appP
--     :: Iso' s (s', AnIso' a b)
--     -> Iso' s' (s'', a)
--     -> Iso' s (s'', b)
-- appP pf pa = iso undefined $ \(s'', b) -> _
