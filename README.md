# irmin-tutorials

A set of tutorials for getting started with [irmin](https://github.com/mirage/irmin). The current version is available at [https://zshipko.github.io/irmin-tutorial](https://zshipko.github.io/irmin-tutorial)

## Dependencies

- [ocamlbuild](https://github.com/ocaml/ocamlbuild)
- [mdx](https://github.com/realworldocaml/mdx)
    - `opam pin add mdx https://github.com/realworldocaml/mdx.git`
- [irmin-unix](https://github.com/mirage/irmin)
    - `opam install irmin-unix`
- [ocaml-hiredis](https://github.com/zshipko/ocaml-hiredis)
    - `opam install hiredis`
- [mdBook](https://github.com/rust-lang-nursery/mdBook)
    - required for `make build`
    - `cargo install mdbook`
- [omd](https://github.com/ocaml/omd)
    - required for `make generate`

## Building

This will check and build the example code and an HTML version of the tutorials in `./book` (if mdBook is installed):

```shell
$ make
```

## Running the example code

```shell
$ make run
```

## Contributing

Contributions are encouraged! If you think something is missing or could be explained better please open an issue.
