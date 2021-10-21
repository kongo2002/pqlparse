# pqlparse

![pqlparse](https://github.com/kongo2002/pqlparse/actions/workflows/build.yaml/badge.svg)


## Usage

You can build and run `pqlparse` using either [stack][stack] or [cabal][cabal]
(see below). In general `pqlparse` reads PQL-like input from stdin.

You can parse PQL input like:

```shell
$ echo "foo is 'foo' and bar gt 42" | stack run
bar gt 9.0 and foo is 'foo'
```

You can also simplify PQL expressions like:

```shell
$ stack run -- -r <<EOF
/foo,category is 'foo' or path is '/bar'
/bar,category is 'bar' and price gt 34
EOF
/bar,category is 'bar' and price gt 34.0
/foo,(category is 'bar' or category is 'foo') and (price gt 34.0 or category is 'foo')
```

Output can be either in CSV (default) or JSON:

```shell
$ echo "category is 'ham & eggs'" | stack run -- -o json | jq .
[
  {
    "attr": "category",
    "value": "ham & eggs",
    "compare": "=",
    "type": "string",
    "children": []
  }
]
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


[cabal]: https://www.haskell.org/cabal/
[stack]: https://www.haskellstack.org/
