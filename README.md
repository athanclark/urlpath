# urlpath

[![Build Status](https://travis-ci.org/athanclark/urlpath.svg)](https://travis-ci.org/athanclark/urlpath)
[![Coverage Status](https://coveralls.io/repos/athanclark/urlpath/badge.png?branch=master)](https://coveralls.io/r/athanclark/urlpath)
[![Chat Room](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/athanclark/urlpath)
[![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://tldrlegal.com/license/mit-license)
[![Hackage](http://img.shields.io/badge/hackage-0.2-brightgreen.svg)](https://hackage.haskell.org/package/urlpath)
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
λ> expandRelative $ "foo.php" <?> ("key1","bar") <&> ("key2","baz")

↪ "foo.asp?key1=bar&key2=baz"
```

Or you can use them with a configurable root, via the Reader monad:

```haskell
λ> runReader
     (runAbsoluteUrl $ url $ "foo.asp" <?> ("key1","bar") <&> ("key2","baz"))
     "http://example.com"

↪ "http://example.com/foo.asp?key1=bar&key2=baz"
```
`url` puts the `UrlString` in a MonadReader that we can use for applying our⋅
host. We use different monads for different deployment schemes (currently we⋅
have 3 - `RelativeUrlT`, `GroundedUrlT`, and `AbsoluteUrlT`), which we can⋅
integrate in different libraries, like Lucid:

```haskell
λ> (runAbsoluteUrl $ renderTextT $ do
     foo <- lift $ url $ "foo" <?> ("bar","baz")
     script_ [src_ foo] "" )
   ) "example.com"

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
    rootConf = flip runAbsoluteT "http://example.com"

    run :: ( MonadIO m
           , MonadReader T.Text m
           , Url T.Text m ) =>
           ScottyT LT.Text m ()
    run = get "/" $ do
      path <- lift $ url $ "foo" <?> ("bar","baz")
      text $ LT.fromStrict path
```

```
λ> curl localhost:3000/
↪ "http://example.com/foo?bar=baz"
```

## How to run tests

```bash
λ> cabal install hspec --enable-tests && cabal test --show-details=always
```

## Contributing

I would prefer it that any inquiries and questions go to the
[Gitter Chat room](https://gitter.im/athanclark/urlpath), while any suggestions, 
complaints, or requests go in the
[GitHub Issues](https://github.com/athanclark/urlpath/issues) /
[Waffle Dashboard](https://waffle.io/athanclark/urlpath). All ideas are welcome! 
(Except really gross ones. I've got limits.)
