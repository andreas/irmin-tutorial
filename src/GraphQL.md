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

## Schema

Using the GraphiQL web interface you can explore the schema using the **Docs** button in the upper-right corner. Additionally, there are tools like [https://github.com/prisma/get-graphql-schema](get-graphql-schema) which will dump the entire schema for you.

Currently, the GraphQL schema looks like this:

```graphql
schema {
  query: query
  mutation: mutation
}

type Branch {
  name: String!
  head: Commit
  get(key: Key!): String
  get_all(key: Key!): Contents
  lca(commit: CommitHash!): [Commit!]!
}

scalar BranchName

type Commit {
  node: Node!
  parents: [Commit!]!
  info: Info!
  hash: String!
}

scalar CommitHash

type Contents {
  key: String!
  metadata: String
  value: String
}

type Info {
  date: String!
  author: String!
  message: String!
}

input InfoInput {
  message: String
  author: String
}

scalar Key

type mutation {
  set(info: InfoInput, value: String!, key: String!, branch: BranchName): Commit
  set_all(info: InfoInput, metadata: String, value: String!, key: String!, branch: BranchName): Commit
  remove(info: InfoInput, key: String!, branch: BranchName): Commit
  merge(info: InfoInput, from: BranchName!, branch: BranchName): Commit
  revert(commit: CommitHash!, branch: BranchName): Commit
  clone(remote: Remote!, branch: BranchName): Commit!
  push(remote: Remote!, branch: BranchName): String
  pull(remote: Remote!, branch: BranchName): Commit
}

type Node {
  key: String!
  get(key: Step): Node
  value: String
  metadata: String
  tree: [Tree!]!
}

type query {
  commit(hash: CommitHash!): Commit
  master: Branch
  branch(name: BranchName!): Branch
}

scalar Remote

scalar Step

union Tree = Contents | Node
```

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

If we were interested in the same key on a different branch then the query will look something like this:

```graphql
query {
    branch(name: "my-branch") {
    	get(key: "abc")
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
    set(branch: null, key: "abc", value: "123", info: null) {
        hash
    }
}
```

The example above sets the key "abc" to "123" and returns the new commit's hash.

## Sync

`clone`, `push` and `pull` are also supported! This means that you're able to sync with remote stores using simple mutations:

```graphql
mutation {
    pull(remote: "git://github.com/mirage/irmin", branch: null) {
        hash
    }
}
```
