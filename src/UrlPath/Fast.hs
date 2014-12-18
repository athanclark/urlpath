{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module UrlPath.Fast where

import Data.String
import Data.Monoid


(</>) :: ( IsString a
         , Monoid a ) => a -> a -> a  
(</>) !x !y = x <> "/" <> y

infixl 6 </>


(<?>) :: ( IsString a
         , Monoid a ) => a -> (a,a) -> a
(<?>) !x (!a,!b) = x <> "?" <> a <> "=" <> b

infixl 7 <?>


(<&>) :: ( IsString a
         , Monoid a ) => a -> (a,a) -> a
(<&>) !x (!a,!b) = x <> "&" <> a <> "=" <> b

infixl 7 <&>
