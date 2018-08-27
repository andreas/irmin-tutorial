# Getting started using OCaml

Irmin has the ability to adapt to existing data structures using a convenient type combinator ([Irmin.Type](https://mirage.github.io/irmin/irmin/Irmin/Type/index.html)), which is used to define ([Contents](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html)). Additionally, you have a choice of storage backends. By default, Irmin provides a few options: an in-memory store, a filesystem store, a git-compatible in-memory store and a git-compatible filesystem store. Of course, it's also possible to implement your own storage backend! Nearly everything in `Irmin` is configurable including the hash function, branch, key and metadata types. Because of this there are a lot of different options to pick from; I will do my best to explain the most basic usage and work up from there.

The default `Contents` are available under [Irmin.Contents](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html), but the storage backends are all implemented as separate modules. The core backends are on opam as `irmin-mem`, `irmin-fs` and `irmin-git`. These packages define the way that the data should be organized, but not any I/O routines. Luckily, `irmin-unix` implements the I/O routines needed to make Irmin work on unix-like platforms and `irmin-mirage` provides the same for unikernels built using [Mirage](https://mirage.io).

It is important to remember that most `Irmin` functions return `Lwt.t` values, which means that you will need to use `Lwt_main.run` to execute them. If you're not familiar with [Lwt](https://github.com/ocsigen/lwt) then I suggest [this tutorial](https://mirage.io/wiki/tutorial-lwt).

## Creating a store

An in-memory store with string contents:

```ocaml
module Mem_store = Irmin_mem.KV(Irmin.Contents.String)
```

An on-disk git store with JSON contents:

```ocaml
module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.Json)
```

These examples are using a [Irmin.KV]( https://mirage.github.io/irmin/irmin/Irmin/module-type-KV/index.html) store which is a specialization of [Irmin.S](https://mirage.github.io/irmin/irmin/Irmin/module-type-S/index.html) with string list keys, string branches and no metadata.

The following example is the same as the first, using `Irmin_mem.Make` instead of `Irmin_mem.KV`:

```ocaml
module Mem_Store =
    Irmin_mem.Make
        (Irmin.Metadata.None)
        (Irmin.Contents.Json)
        (Irmin.Path.String_list)
        (Irmin.Branch.String)
        (Irmin.Hash.SHA1)
```

## Configuring and creating a repo

Different store types require different configuration options -- an on-disk store needs to know where it should be stored in the filesystem, however an in-memory store doesn't. This means that each storage backend implements its own configuration methods based on [Irmin.Private.Conf](https://mirage.github.io/irmin/irmin/Irmin/Private/Conf/index.html) - for the examples above there are `Irmin_mem.config`, `Irmin_fs.config` and `Irmin_git.config`, each taking slightly different parameters.

```ocaml
let git_config = Irmin_git.config ~bare:true "/tmp/irmin"
let config = Irmin_mem.config ()
```
Once you have created your configuration you can create an [Irmin.Repo](https://mirage.github.io/irmin/irmin/Irmin/Repo/index.html) using [Repo.v](https://mirage.github.io/irmin/irmin/Irmin/Make/Repo/index.html#val-v).

```ocaml
let git_repo = Git_store.Repo.v git_config
let repo = Mem_store.Repo.v config
```

## Using the repo to obtain access to a branch

Once a repo has been created, you can access a branch and start to modify it.

To get access to the `master` branch:

```ocaml
open Lwt.Infix

let master config =
    Mem_store.Repo.v config >>= Mem_store.master
```

To get access to a named branch:

```ocaml
let branch config name =
    Mem_store.Repo.v config >>= fun repo ->
    Mem_store.of_branch repo name
```

## Modifying the store

Now, using everything I've laid out above, you can finally begin to read and write to the store using `get` and `set`.

```ocaml
let info message = Irmin_unix.info ~author:"Example" "%s"

let main =
Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->

(* Set a/b/c to "Hello, Irmin!" *)
Mem_store.set t ["a"; "b"; "c"] "Hello, Irmin!" ~info:(info "my first commit") >>= fun () ->

(* Get a/b/c *)
Mem_store.get t ["a"; "b"; "c"] >|= fun s ->
assert (s = "Hello, Irmin!")

let _ = Lwt_main.run main
```

## Transactions

Transactions allow you to make many modifications to an Irmin store, using an in-memory tree, and apply them all at once. This is done using the `Store.with_tree` function:

```ocaml
let transaction_example =
Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
let info = Irmin_unix.info "example transaction" in
Mem_store.with_tree t [] ~info (fun tree ->
    let tree = match tree with Some t -> t | None -> Mem_store.Tree.empty in
    Mem_store.Tree.remove tree ["foo"; "bar"] >>= fun tree ->
    Mem_store.Tree.add tree ["a"; "b"; "c"] "123" >>= fun tree ->
    Mem_store.Tree.add tree ["d"; "e"; "f"] "456" >>= Lwt.return_some)

let _ = Lwt_main.run transaction_example
```

A tree can be modified directly using the functions in [Irmin.S.Tree](https://mirage.github.io/irmin/irmin/Irmin/module-type-S/Tree/index.html). When a tree is returned by the `with_tree` callback will be applied at the given key (`[]` in the example above).

Here is an example `move` function to move files from one prefix to another:

```ocaml
let move t ~src ~dest =
    Mem_store.with_tree t Mem_store.Key.empty (fun tree ->
        match tree with
        | Some tr ->
            Mem_store.Tree.get_tree tr src >>= fun v ->
            Mem_store.Tree.remove tr src >>= fun _ ->
            Mem_store.Tree.add_tree tr dest v >>= Lwt.return_some
        | None -> Lwt.return_none
    )
```