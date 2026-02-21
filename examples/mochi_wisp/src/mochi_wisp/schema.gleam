// mochi_wisp/schema.gleam
// Example GraphQL schema for the Wisp integration demo
// Using high-level types API

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

pub type User {
  User(id: String, name: String, email: String, role: Role)
}

pub type Role {
  Admin
  Member
  Guest
}

pub type Message {
  Message(id: String, content: String, channel: String, timestamp: Int)
}

// ============================================================================
// Sample Data
// ============================================================================

pub fn sample_users() -> List(User) {
  [
    User(id: "1", name: "Alice", email: "alice@example.com", role: Admin),
    User(id: "2", name: "Bob", email: "bob@example.com", role: Member),
    User(id: "3", name: "Charlie", email: "charlie@example.com", role: Guest),
  ]
}

pub fn find_user_by_id(id: String) -> Result(User, String) {
  sample_users()
  |> list.find(fn(u) { u.id == id })
  |> result_from_option("User not found: " <> id)
}

fn result_from_option(opt: Result(a, Nil), error: String) -> Result(a, String) {
  case opt {
    Ok(value) -> Ok(value)
    Error(_) -> Error(error)
  }
}

// ============================================================================
// Enum Helpers
// ============================================================================

pub fn role_to_string(role: Role) -> String {
  case role {
    Admin -> "ADMIN"
    Member -> "MEMBER"
    Guest -> "GUEST"
  }
}

pub fn string_to_role(s: String) -> Result(Role, String) {
  case s {
    "ADMIN" -> Ok(Admin)
    "MEMBER" -> Ok(Member)
    "GUEST" -> Ok(Guest)
    _ -> Error("Unknown role: " <> s)
  }
}

// ============================================================================
// Decoders
// ============================================================================

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use name <- decode.field("name", decode.string)
    use email <- decode.field("email", decode.string)
    use role_str <- decode.field("role", decode.string)
    decode.success(#(id, name, email, role_str))
  }
  case decode.run(dyn, decoder) {
    Ok(#(id, name, email, role_str)) ->
      case string_to_role(role_str) {
        Ok(role) -> Ok(User(id: id, name: name, email: email, role: role))
        Error(e) -> Error(e)
      }
    Error(_) -> Error("Failed to decode User")
  }
}

fn decode_message(dyn: Dynamic) -> Result(Message, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use content <- decode.field("content", decode.string)
    use channel <- decode.field("channel", decode.string)
    use timestamp <- decode.field("timestamp", decode.int)
    decode.success(Message(
      id: id,
      content: content,
      channel: channel,
      timestamp: timestamp,
    ))
  }
  decode.run(dyn, decoder)
  |> result_map_error("Failed to decode Message")
}

fn result_map_error(
  result: Result(a, b),
  error_msg: String,
) -> Result(a, String) {
  case result {
    Ok(v) -> Ok(v)
    Error(_) -> Error(error_msg)
  }
}

// ============================================================================
// Encoders - Using types helpers for cleaner code
// ============================================================================

pub fn user_to_dynamic(user: User) -> Dynamic {
  types.record([
    types.field("id", user.id),
    types.field("name", user.name),
    types.field("email", user.email),
    types.field("role", role_to_string(user.role)),
  ])
}

pub fn users_encoder(users: List(User)) -> Dynamic {
  types.to_dynamic(list.map(users, user_to_dynamic))
}

pub fn user_encoder(user: User) -> Dynamic {
  user_to_dynamic(user)
}

pub fn message_to_dynamic(msg: Message) -> Dynamic {
  types.record([
    types.field("id", msg.id),
    types.field("content", msg.content),
    types.field("channel", msg.channel),
    types.field("timestamp", msg.timestamp),
  ])
}

pub fn message_encoder(msg: Message) -> Dynamic {
  message_to_dynamic(msg)
}

// ============================================================================
// GraphQL Type Definitions - Using High-Level types API
// ============================================================================

pub fn role_enum() -> schema.EnumType {
  types.enum_type("Role")
  |> types.enum_description("User role in the system")
  |> types.value_with_desc("ADMIN", "Administrator with full access")
  |> types.value_with_desc("MEMBER", "Regular member")
  |> types.value_with_desc("GUEST", "Guest with limited access")
  |> types.build_enum
}

pub fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.description("A user in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.string("email", fn(u: User) { u.email })
  |> types.string("role", fn(u: User) { role_to_string(u.role) })
  |> types.build(decode_user)
}

pub fn message_type() -> schema.ObjectType {
  types.object("Message")
  |> types.description("A message in a channel")
  |> types.id("id", fn(m: Message) { m.id })
  |> types.string("content", fn(m: Message) { m.content })
  |> types.string("channel", fn(m: Message) { m.channel })
  |> types.int("timestamp", fn(m: Message) { m.timestamp })
  |> types.build(decode_message)
}

// ============================================================================
// Query Resolvers
// ============================================================================

pub fn users_resolver(
  _ctx: schema.ExecutionContext,
) -> Result(List(User), String) {
  Ok(sample_users())
}

pub fn user_resolver(
  args: UserByIdArgs,
  _ctx: schema.ExecutionContext,
) -> Result(User, String) {
  find_user_by_id(args.id)
}

// ============================================================================
// Argument Types and Decoders
// ============================================================================

pub type UserByIdArgs {
  UserByIdArgs(id: String)
}

pub fn decode_user_by_id_args(
  args: Dict(String, Dynamic),
) -> Result(UserByIdArgs, String) {
  case dict.get(args, "id") {
    Ok(id_dyn) -> {
      case decode.run(id_dyn, decode.string) {
        Ok(id) -> Ok(UserByIdArgs(id: id))
        Error(_) -> Error("Invalid id argument: expected string")
      }
    }
    Error(_) -> Error("Missing required argument: id")
  }
}

pub type ChannelArgs {
  ChannelArgs(channel: String)
}

pub fn decode_channel_args(
  args: Dict(String, Dynamic),
) -> Result(ChannelArgs, String) {
  case dict.get(args, "channel") {
    Ok(ch_dyn) -> {
      case decode.run(ch_dyn, decode.string) {
        Ok(ch) -> Ok(ChannelArgs(channel: ch))
        Error(_) -> Error("Invalid channel argument")
      }
    }
    Error(_) -> Error("Missing required argument: channel")
  }
}

/// Resolve the topic for onMessage subscription based on channel argument
pub fn on_message_topic_resolver(
  args: ChannelArgs,
  _ctx: schema.ExecutionContext,
) -> Result(String, String) {
  Ok("message:" <> args.channel)
}

// ============================================================================
// Schema Builder
// ============================================================================

pub fn build_schema() -> schema.Schema {
  let users_query =
    query.query(
      "users",
      schema.NonNull(schema.List(schema.Named("User"))),
      users_resolver,
      users_encoder,
    )
    |> query.query_description("Get all users")

  let user_query =
    query.query_with_args(
      "user",
      [
        query.arg_with_desc(
          "id",
          schema.NonNull(schema.Named("ID")),
          "The user ID",
        ),
      ],
      schema.Named("User"),
      decode_user_by_id_args,
      user_resolver,
      user_encoder,
    )
    |> query.query_description("Get a user by ID")

  // Subscription for real-time user events
  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.Named("User"),
      "userCreated",
      user_encoder,
    )
    |> query.subscription_description("Subscribe to new user creation events")

  let on_message =
    query.subscription_with_args(
      "onMessage",
      [query.arg("channel", schema.NonNull(schema.string_type()))],
      schema.Named("Message"),
      decode_channel_args,
      on_message_topic_resolver,
      message_encoder,
    )
    |> query.subscription_description("Subscribe to messages on a channel")

  query.new()
  |> query.add_query(users_query)
  |> query.add_query(user_query)
  |> query.add_subscription(on_user_created)
  |> query.add_subscription(on_message)
  |> query.add_type(user_type())
  |> query.add_type(message_type())
  |> query.add_enum(role_enum())
  |> query.build
}
