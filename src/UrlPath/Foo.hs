{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- | An attempt at a more general @UrlPath@

module UrlPath.Foo where

import Data.String
import Data.Monoid

-- | The goal is to make string an instance of Url, with UrlPath still also 
-- being an instance, while having the monadic deployment mechanism still be 
-- flat - when deployment happens via @runUrlString@, it should be OK in 
-- figuring out what to make out of it.
--
-- We need to be able to add @UrlString@ to the context of any @IsString@ 
-- /implicitly/.



-- | What makes a @Url@? The ability to be rendered to a final string.
class RelativeUrl a where
  url :: ( IsString b
         , Monoid b ) => a -> b

instance ( IsString a
         , Monoid a ) => RelativeUrl a where
  url :: ( IsString b
         , Monoid b
         , a ~ b ) => a -> b
  url = id

data UrlString a where
  UrlString :: IsString a =>
               a -- ^ Target
            -> [(a, a)] -- ^ GET parameters and values
            -> UrlString a

instance Show a => Show (UrlString a) where
  show (UrlString t []) = show t
  show (UrlString t ((a,b):xs)) = show t <> "?" <> show a <> "=" <> show b <>
    (foldl (\acc (x,y) -> acc <> "&" <> show x <> "=" <> show y) "" xs)

instance RelativeUrl (UrlString a) where
  url :: ( IsString b
         , Monoid b
         , a ~ b ) => UrlString a -> b
  url (UrlString t []) = t
  url (UrlString t ((a,b):xs)) = t <> "?" <> a <> "=" <> b <>
    (foldl (\acc (x,y) -> acc <> "&" <> x <> "=" <> y) "" xs)

{-
(<?>) :: IsString a =>
         a
      -> (a, a)
      -> UrlString

(<&>) :: IsString a =>
         UrlString
      -> (a, a)
      -> UrlString
-}
