# urlpath

[![Build Status](https://travis-ci.org/athanclark/urlpath.svg)](https://travis-ci.org/athanclark/urlpath)
[![Coverage Status](https://img.shields.io/coveralls/athanclark/urlpath.svg)](https://coveralls.io/r/athanclark/urlpath)
[![Chat Room](http://img.shields.io/badge/glitter-chat--room-brightgreen.svg)](https://gitter.im/athanclark/urlpath)
[![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://tldrlegal.com/license/mit-license)
[![Hackage](http://img.shields.io/badge/hackage-0.1-brightgreen.svg)](https://hackage.haskell.org/package/urlpath)
[![Waffle Issues](https://badge.waffle.io/athanclark/urlpath.png?label=ready&title=Ready)](https://waffle.io/athanclark/urlpath)

Dirt-simple, embarrassing, horribly unimaginative URL combinator library for 
Haskell.


## Installation

```bash
λ> cabal install urlpath
```

## Usage

You can use the combinators purely, if you're into that:

```haskell
λ> render $ "foo.asp" <?> ("key1","bar") <&> ("key2","baz")

↪ "foo.asp?key1=bar&key2=baz"
```

Or you can use them with a configurable root, via the Reader monad:

```haskell
λ> runReader
     (expandAbsolute $ "foo.asp" <?> ("key1","bar") <&> ("key2","baz"))
     "http://example.com"

↪ "http://example.com/foo.asp?key1=bar&key2=baz"
```

... We can also do things in Lucid:

```haskell
λ> runReader ( renderTextT $
     (\a -> do
         foo <- lift $ expandAbsolute a
         script_ [src_ foo] ""
       ) $ "foo" <?> ("bar","baz")
     ) "http://example.com"

↪ "<script src=\"example.com/foo?bar=baz\"></script>"
```

and, in Scotty:

```haskell
main :: IO ()
main = scottyT 3000
    rootConf
    rootConf
    run

  where
    rootConf = flip runReaderT "http://example.com"

    run :: ( MonadIO m
           , MonadReader T.Text m ) =>
           ScottyT LT.Text m ()
    run = get "/" $ do
      path <- lift $ expandAbsolute $ "foo" <?> ("bar","baz")
      text $ LT.fromStrict path
```

```
λ> curl localhost:3000/
↪ "http://example.com/foo?bar=baz"
```

## How to run tests

```bash
λ> cabal install hspec QuickCheck quickcheck-instances
λ> cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

`¯\_(ツ)_/¯`
