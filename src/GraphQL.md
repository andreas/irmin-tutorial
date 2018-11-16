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

[irmin-js](https://github.com/zshipko/irmin-js) and [irmin-go](https://github.com/zshipko/irmin-go) serve as the reference client implementations in Javascript and Go. In addition to providing many basic queries by default, they also simplify the process of executing handwritten queries.

## Schema

Using the GraphiQL web interface you can explore the schema using the **Docs** button in the upper-right corner. Additionally, there are tools like [https://github.com/prisma/get-graphql-schema](get-graphql-schema) which will dump the entire schema for you.

## Writing queries

To start off we will compose a query to retrieve the value stored at the key `abc`:

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

`irmin-js` can also be used to send raw queries to the server:

```javascript
let ir = new Irmin("http://localhost:8080/graphql");
ir.execute({
    body: "query { master { get(key: "abc") } }",
    variables: {}
}).then((res) => {
    ...
});
```

If we were interested in the same key on a different branch then the query would look something like this:

```graphql
query {
    branch(name: "my-branch") {
    	get(key: "a/b/c")
    }
}
```

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

`irmin-graphql` also supports mutations, which are queries that are able to modify data.

For example, setting a key is easy:

```graphql
mutation {
    set(branch: null, key: "a/b/c", value: "123", info: null) {
        hash
    }
}
```

The example above sets the key `a/b/c` (`["a"; "b"; "c"]` in OCaml) to `123` and returns the new commit's hash.

It's also possible to update multiple keys at the same time:

```graphql
mutation {
    set_tree(key: "foo", tree: [{key: "bar", value: "a"}, {key: "baz", value: "b"}, {key: "c", value: null}]) {
        hash
    }
}
```

This will set `foo/bar` to `a`, `foo/baz` to `b` and remove `a/b/c`.

### Sync

`clone`, `push` and `pull` are also supported! This means that you're able to sync with remote stores using simple mutations:

```graphql
mutation {
    pull(remote: "git://github.com/mirage/irmin", branch: null) {
        hash
    }
}
```
