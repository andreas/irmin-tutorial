# GraphQL bindings

`irmin-graphql` provides a nice interface for accessing remote Irmin stores over a network. This section will show you how to run an `irmin-graphql` server and query it using [irmin-js](https://github.com/zshipko/irmin-js), or your favorite GraphQL client!

## Installation

`irmin-graphql` is part of the latest `irmin` release, so all that's needed is:

```shell
$ opam install irmin-unix
```

## Running the server

To start the GraphQL server:

```shell
$ irmin graphql --port 8080
```

This will start the server on `localhost:8080`. By default `irmin-graphql` provides an GraphiQL editor for writing queries from within the brower which can be accessed at [http://localhost:8080/graphql](http://localhost:8080/graphql)

## Available clients

There are several reference client implementations provide many basic queries by default, in addition to simplifying the process of executing handwritten queries.

- [irmin-js](https://github.com/zshipko/irmin-js)
- [irmin-go](https://github.com/zshipko/irmin-go)

## Schema

Using the GraphiQL web interface you can explore the schema using the **Docs** button in the upper-right corner. Additionally, there are tools like [https://github.com/prisma/get-graphql-schema](get-graphql-schema) which will dump the entire schema for you.

## Queries

Using `irmin-graphql` it is possible to collect information about Irmin databases and (Git repositories) using GraphQL.

### Get

To start off we will create a query to retrieve the value stored at the path `abc`:

```graphql
query {
    master {
        get(key: "abc")
    }
}
```

Using `irmin-js` the same query would be written as:

```javascript
let ir = new Irmin("http://localhost:8080/graphql");
ir.master().get("abc").then((res) => {
    ...
});
```

**NOTE**: `irmin-js` can also be used to send execute raw queries:

```javascript
let ir = new Irmin("http://localhost:8080/graphql");
ir.execute({
    body: "query { master { get(key: "abc") } }",
    variables: {}
}).then((res) => {
    ...
});
```

The following would accomplish the same thing in `my-branch`:

```graphql
query {
    branch(name: "my-branch") {
    	get(key: "a/b/c")
    }
}
```

### Branch info

Using `master`/`branch` queries we are able to find lots of information about the attached Irmin store:

```graphql
query {
    master {
        head {
            hash
            info
            parents
        }
    }
}
```

## Mutations

`irmin-graphql` also supports mutations, which are basically queries with side-effects.

### Set

For example, setting a key is easy:

```graphql
mutation {
    set(branch: null, key: "a/b/c", value: "123", info: null) {
        hash
    }
}
```

The example above sets the key "a/b/c" (`["a"; "b"; "c"]` in OCaml) to "123" and returns the new commit's hash.

### Sync

`clone`, `push` and `pull` are also supported! This allows data to be synchronized accross servers using a simple mutation:

```graphql
mutation {
    pull(remote: "git://github.com/mirage/irmin", branch: null) {
        hash
    }
}
```

## GraphQL servers in OCaml

It is also possible to use the `irmin-graphql` OCaml interface to embed a GraphQL server in any application!

Using `Irmin_unix.Graphql.Server.Make` you can convert an existing `Irmin.S` typed module into a GraphQL server:

```ocaml
module Graphql_store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Graphql = Irmin_unix.Graphql.Server.Make(Graphql_store)(struct let remote = Some Graphql_store.remote end)
```

The following code will initialize and run the server:

```ocaml
let run_server () =
  (* Set up the Irmin store *)
  Graphql_store.Repo.v (Irmin_git.config "/tmp/irmin")
  >>= Graphql_store.master >>= fun store ->

  (* Initialize the GraphQL server *)
  let server = Graphql.server store in

  (* Run the server *)
  let on_exn exn =
    Logs.debug (fun l -> l "on_exn: %s" (Printexc.to_string exn))
  in
  Cohttp_lwt_unix.Server.create ~on_exn ~mode:(`TCP (`Port 1234)) server
```

## Using the OCaml GraphQL client

`irmin-graphql` also provides a GraphQL client. Using `Irmin_unix.Graphql.Client.Make` it is very easy to connect to an Irmin GraphQL server and start making queries!

```ocaml
module Graphql_client = Irmin_unix.Graphql.Client.Make(Graphql_store)
let client = Graphql_client.init (Uri.of_string "http://localhost:1234")
```

Once you're client is set up, you can execute the a built-in query:

```ocaml
let get_value () =
  Graphql_client.get client ["testing"] >>= function
  | Ok v -> Lwt_io.printl v
  | Error (`Msg msg) -> failwith msg
```

Or run your own custom queries using `execute`/`execute_json`:

```ocaml
let get_value() =
  Graphql_client.execute_json client {|
    query {
      master {
        get(key: "testing")
      }
    }
  |} >|= function
  | Ok j ->
    (match Irmin_graphql.Client.Json.find j ["data"; "master"; "get"] with
    | Some x -> Ok x
    | None -> Error (`Msg "invalid response"))
  | Error (`Msg msg) -> Error (`Msg msg)
```

