# Getting started using OCaml

When setting up and Irmin database in OCaml you will need to consider, at least, the content type and storage backend. This is because Irmin has the ability to adapt to existing data structures using a convenient type combinator ([Irmin.Type](https://mirage.github.io/irmin/irmin/Irmin/Type/index.html)), which is used to define [contents](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html) for your datastore. Irmin provides implementations for [String](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html#module-String), [Cstruct](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html#module-Cstruct), [Json](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html#module-Json) and [Json_value](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html#module-Json_value) contents, but it is also very easy to make your own!

Irmin provides a few options when it comes to storage: an in-memory store (`irmin-mem`), a filesystem store (`irmin-fs`) and git-compatible filesystem/in-memory stores (`irmin-git`). These packages define the way that the data should be organized, but not any I/O routines (with the exception of `irmin-mem`, which does no I/O). Luckily, `irmin-unix` implements the I/O routines needed to make Irmin work on unix-like platforms and `irmin-mirage` provides the same for unikernels built using [Mirage](https://mirage.io).

It's also possible to implement your own storage backend if you'd like -- nearly everything in `Irmin` is configurable thanks to the power of functors in OCaml! This includes the hash function, branch, key and metadata types. Because of this flexibility there are a lot of different options to pick from; I will do my best to explain the most basic usage in this section and begin introducing more advanced concepts in subsequent sections.

It is important to note that most `Irmin` functions return `Lwt.t` values, which means that you will need to use `Lwt_main.run` to execute them. If you're not familiar with [Lwt](https://github.com/ocsigen/lwt) then I suggest [this tutorial](https://mirage.io/wiki/tutorial-lwt).

## Creating a store

An in-memory store with string contents:

```ocaml
module Mem_store = Irmin_mem.KV(Irmin.Contents.String)
```

An on-disk git store with JSON contents:

```ocaml
module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.Json)
```

These examples are using [Irmin.KV]( https://mirage.github.io/irmin/irmin/Irmin/module-type-KV/index.html), which is a specialization of [Irmin.S](https://mirage.github.io/irmin/irmin/Irmin/module-type-S/index.html) with string list keys, string branches and no metadata.

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
```

```ocaml
let config = Irmin_mem.config ()
```

With this configuration it's very easy to create an [Irmin.Repo](https://mirage.github.io/irmin/irmin/Irmin/Repo/index.html) using [Repo.v](https://mirage.github.io/irmin/irmin/Irmin/Make/Repo/index.html#val-v):

```ocaml
let git_repo = Git_store.Repo.v git_config
```

```ocaml
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

Now you can begin to interact with the store using `get` and `set`.

```ocaml
let info message = Irmin_unix.info ~author:"Example" "%s" message

let main =
    Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
    (* Set a/b/c to "Hello, Irmin!" *)
    Mem_store.set_exn t ["a"; "b"; "c"] "Hello, Irmin!" ~info:(info "my first commit") >>= fun () ->
    (* Get a/b/c *)
    Mem_store.get t ["a"; "b"; "c"] >|= fun s ->
    assert (s = "Hello, Irmin!")
let () = Lwt_main.run main
```

## Transactions

[Transactions](https://mirage.github.io/irmin/irmin/Irmin/module-type-S_MAKER/index.html#type-transaction) allow you to make many modifications using an in-memory tree then apply them all at once. This is done using [with_tree](https://mirage.github.io/irmin/irmin/Irmin/module-type-S_MAKER/index.html#val-with_tree):

```ocaml
let transaction_example =
Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
let info = Irmin_unix.info "example transaction" in
Mem_store.with_tree_exn t [] ~info ~strategy:`Set (fun tree ->
    let tree = match tree with Some t -> t | None -> Mem_store.Tree.empty in
    Mem_store.Tree.remove tree ["foo"; "bar"] >>= fun tree ->
    Mem_store.Tree.add tree ["a"; "b"; "c"] "123" >>= fun tree ->
    Mem_store.Tree.add tree ["d"; "e"; "f"] "456" >>= Lwt.return_some)
let () = Lwt_main.run transaction_example
```

A tree can be modified using the functions in [Irmin.S.Tree](https://mirage.github.io/irmin/irmin/Irmin/module-type-S/Tree/index.html), and when it is returned by the `with_tree` callback, it will be applied using the transaction's strategy (``` `Set``` in the code above)  at the given key (`[]` in the code above).

Here is an example `move` function to move files from one path to another:

```ocaml
let move t ~src ~dest =
    Mem_store.with_tree_exn t Mem_store.Key.empty ~strategy:`Set (fun tree ->
        match tree with
        | Some tr ->
            Mem_store.Tree.get_tree tr src >>= fun v ->
            Mem_store.Tree.remove tr src >>= fun _ ->
            Mem_store.Tree.add_tree tr dest v >>= Lwt.return_some
        | None -> Lwt.return_none
    )
let main =
    Mem_store.Repo.v config >>= Mem_store.master >>= fun t ->
    let info = Irmin_unix.info "move a -> foo" in
    move t ~src:["a"] ~dest:["foo"] ~info
let () = Lwt_main.run main
```

## Sync

[Irmin.Sync](https://docs.mirage.io/irmin/Irmin/Sync/index.html) implements the functions needed to interact with remote stores.

- [fetch](https://docs.mirage.io/irmin/Irmin/Sync/index.html#val-fetch) populates a local store with objects from a remote store
- [pull](https://docs.mirage.io/irmin/Irmin/Sync/index.html#val-pull) updates a local store with objects from a remote store
- [push](https://docs.mirage.io/irmin/Irmin/Sync/index.html#val-fpush) updates a remote store with objects from a local store

Each of these also has an `_exn` variant which may raise an exception instead of returning `result` value.

For example, you can pull a repo and list the files in the root of the project:

```ocaml
open Irmin_unix
module Git_mem_store = Git.Mem.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Git_mem_store)
let remote = Git_mem_store.remote "git://github.com/mirage/irmin.git"
let main =
    Git_mem_store.Repo.v config >>= Git_mem_store.master >>= fun t ->
    Sync.pull_exn t remote `Set >>= fun () ->
    Git_mem_store.list t [] >|= List.iter (fun (step, kind) ->
        match kind with
        | `Contents -> Printf.printf "FILE %s\n" step
        | `Node -> Printf.printf "DIR %s\n" step
    )
let () = Lwt_main.run main
```

## JSON Contents

Most examples in this tutorial use string contents, so I will provide some further information about using JSON values.

There are two types of JSON contents: [Json_value](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html#module-Json_value) and [Json](https://mirage.github.io/irmin/irmin/Irmin/Contents/index.html#module-Json), where `Json` can only store JSON objects and `Json_value` works with any JSON value.

Setting up the store is exactly the same as when working with strings:

```ocaml
module Mem_store_json = Irmin_mem.KV(Irmin.Contents.Json)
module Mem_store_json_value = Irmin_mem.KV(Irmin.Contents.Json_value)
```

For example, using `Men_store_json_value` we can assign `{"x": 1, "y": 2, "z": 3}` to the key `a/b/c`:

```ocaml
let main =
    let module Store = Mem_store_json_value in
    Store.Repo.v config >>= Store.master >>= fun t ->
    let value = `O ["x", `Float 1.; "y", `Float 2.; "z", `Float 3.] in
    Store.set_exn t ["a"; "b"; "c"] value ~info:(info "set a/b/c") >>= fun () ->
    Store.get t ["a"; "b"; "c"] >|= fun x ->
    assert (Irmin.Type.equal Store.contents_t value x)
let () = Lwt_main.run main
```

An interesting thing about `Json_value` stores is the ability to use [Json_tree](https://mirage.github.io/irmin/irmin/Irmin/index.html#module-Json_tree) to recursively project values onto a key. This means that using `Json_tree.set`, to assign `{"test": {"foo": "bar"}, "x": 1, "y": 2, "z": 3}` to the key `a/b/c` will set `a/b/c/x` to `1`, `a/b/c/y` to `2`, `a/b/c/z` to `3` and `a/b/c/test/foo` to `"bar"`.

This allows for large JSON objects to be modified in pieces without having the encode/decode the entire thing to access specific fields. Using `Json_tree.get` we can also retrieve a tree as a JSON value. So if we call `Json_tree.get` with the key `a/b` we will get the following object back: `{"c": {"test": {"foo": "bar"}, "x": 1, "y": 2, "z": 3}}`.

```ocaml
let main =
    let module Store = Mem_store_json_value in
    let module Proj = Irmin.Json_tree(Store) in
    Store.Repo.v config >>= Store.master >>= fun t ->
    let value = `O ["test", `O ["foo", `String "bar"]; "x", `Float 1.; "y", `Float 2.; "z", `Float 3.] in
    Proj.set t ["a"; "b"; "c"] value ~info:(info "set a/b/c") >>= fun () ->
    Store.get t ["a"; "b"; "c"; "x"] >>= fun x ->
    assert (Irmin.Type.equal Store.contents_t (`Float 1.) x);
    Store.get t ["a"; "b"; "c"; "test"; "foo"] >>= fun x ->
    assert (Irmin.Type.equal Store.contents_t (`String "bar") x);
    Proj.get t ["a"; "b"] >|= fun x ->
    assert (Irmin.Type.equal Store.contents_t (`O ["c", value]) x)
let () = Lwt_main.run main
```

