# pqlparse


## Usage

You can build and run `pqlparse` using either `stack` or `cabal` (see below). In
general `pqlparse` reads PQL-like input from stdin.

You can parse PQL input like:

```
$ echo "foo is 'foo' and bar gt 42" | stack run
bar gt 9.0 and foo is 'foo'
```

You can also simplify PQL expressions like:

```
$ stack run -- -r <<EOF
/foo,category is 'foo' or path is '/bar'
/bar,category is 'bar' and price gt 34
EOF
/bar,category is 'bar' and price gt 34.0
/foo,(category is 'bar' or category is 'foo') and (price gt 34.0 or category is 'foo')
```


## Build

### Stack

    $ stack build

### Cabal

    $ cabal build


## Tests

### Stack

    $ stack test

### Cabal

    $ cabal test
