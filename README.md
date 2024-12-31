My solutions to the exercises from the Haskell Functional Programming course [fp-course](https://github.com/system-f/fp-course/). The tests are rewritten using Hspec and QuickCheck instead of their homegrown framework.

[![](https://github.com/asarkar/fp-course-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/fp-course-haskell/actions)

## Progression

The first step is to inspect the introduction modules.

* [ExactlyOne](src/ExactlyOne.hs)
* [Validation](src/Validation.hs)

They contain examples of data structures and Haskell syntax. They do not contain
exercises and exist to provide a cursory examination of Haskell syntax.

After this, we recommend the following progression of modules:

* [Optional](src/Optional.hs)
* [List](src/List.hs)
* [Functor](src/Functor.hs)
* [Applicative](src/Applicative.hs)
* [Monad](src/Monad.hs)
* [FileIO](src/FileIO.hs)
* [State](src/State.hs)
* [StateT](src/StateT.hs)
* [Extend](src/Extend.hs)
* [Comonad](src/Comonad.hs)
* [Contravariant](src/Contravariant.hs)
* [Compose](src/Compose.hs)
* [Traversable](src/Traversable.hs)
* [ListZipper](src/ListZipper.hs)
* Parser *(see also Person for the parsing rules)*
* MoreParser
* JsonParser
* Interactive
* Anagrams
* FastAnagrams
* Cheque

## Running tests

```
./.github/run.sh
```

To run all matching tests:
```
./.github/run.sh -m <some_word>
```

To run exactly matching tests:
```
./.github/run.sh -m "/<some_word>/"
```

To run a _specific test_:
```
./.github/run.sh -m "/Ch11/evaluates expression/eval/"
```

To run a file containing a `main` method:
```
stack runhaskell <path/to/file> <arg1> <arg2>
```

To run an executable listed in `package.yaml`:
```
stack build
stack exec <name>
```

## License

Released under [Apache License v2.0](LICENSE).
