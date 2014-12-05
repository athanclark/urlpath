{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Path where

import qualified Data.Text as T
import Control.Monad.Reader.Class


-- | This is a utility typeclass that gives us a unified way to render a 
-- (potentially relative) path, either as a @Data.Text.Text@, or as @Chunks@.
class RelativePath a where
  -- | Render the path-like object to a @Data.Text.Text@
  renderRelative :: a -> T.Text

instance RelativePath T.Text where
  renderRelative = id

instance RelativePath Chunks where
  renderRelative c | params c == [] = target c
           | otherwise      = (target c) `T.append`
                              "?" `T.append` (renderChunks $ params c)
    where
      renderChunks :: [Chunk] -> T.Text
      renderChunks [] = ""
      renderChunks [x] = key x `T.append` "=" `T.append` val x
      renderChunks (x:xs) =
                   key x
        `T.append` "="
        `T.append` val x
        `T.append` "&" 
        `T.append` renderChunks xs

-- | Path with a base target and a list of GET parameters
data Chunks = Chunks { target :: T.Text -- ^ Target, without GET Parameters
                     , params :: [Chunk] -- ^ GET Parameters
                     }
  deriving (Show, Eq) 

-- | A single GET parameter
data Chunk = GETParam { key :: T.Text -- ^ GET parameter Key
                      , val :: T.Text -- ^ GET parameter Value
                      }
  deriving (Show, Eq)


-- * Combinators for constructing Chunks

-- | Lift a target path into @Chunks@, with GET parameters
--
-- > Path> render $ "foo" <?> ("bar", "baz") <&> ("qux","weird")
-- > "foo?bar=baz&qux=weird"
(<?>) :: T.Text -> (T.Text, T.Text) -> Chunks
target <?> (k,v) = Chunks target [GETParam k v]

infixl 9 <?>

-- | Add more GET parameters to @Chunks@
(<&>) :: Chunks -> (T.Text, T.Text) -> Chunks
prev <&> (k,v) = Chunks (target prev) $ (params prev) ++ [GETParam k v]

infixl 8 <&>

-- | Print a relative path
expandRelative :: ( RelativePath a
                  , MonadReader T.Text m ) =>
                  a -> m T.Text
expandRelative = return . renderRelative

-- | Print a grounded path (ie: @"/foo.bar"@ instead of @"foo.bar"@
expandGrounded :: ( RelativePath a
                  , MonadReader T.Text m ) =>
                  a -> m T.Text
expandGrounded x = return $ "/" `T.append` renderRelative x

-- | Print an absolute path, using the Reader monad index. For instance:
--
-- > Path> (runReader $ expandAbsolute "foo" <?> ("bar","baz") ) "quxx.com"
-- > "quxx.com/foo?bar=baz"
expandAbsolute :: ( RelativePath a
                  , MonadReader T.Text m ) =>
                  a -> m T.Text
expandAbsolute x = do
  root <- ask
  return $ root `T.append` "/" `T.append` renderRelative x
