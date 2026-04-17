# mochi

Code First GraphQL library for Gleam.

## Installation

```sh
gleam add mochi
```

## Usage

```gleam
import mochi/query
import mochi/types

pub type User {
  User(id: String, name: String)
}

let user_type =
  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.build(decode_user)

let schema =
  query.new()
  |> query.add_query(users_query)
  |> query.add_type(user_type)
  |> query.build
```

## Documentation

https://hexdocs.pm/mochi/

## License

Apache-2.0

