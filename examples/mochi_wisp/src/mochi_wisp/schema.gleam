// mochi_wisp/schema.gleam
// Example GraphQL schema for the Wisp integration demo

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option
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
// Helpers
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
// Type Builders
// ============================================================================

pub fn role_enum() -> schema.EnumType {
  types.enum_type("Role")
  |> types.enum_description("User role in the system")
  |> types.value_with_desc("ADMIN", "Administrator with full access")
  |> types.value_with_desc("MEMBER", "Regular member")
  |> types.value_with_desc("GUEST", "Guest with limited access")
  |> types.build_enum
}

// Build User type with field resolvers that extract from dict
pub fn user_type() -> schema.ObjectType {
  schema.object("User")
  |> schema.description("A user in the system")
  |> schema.field(id_field())
  |> schema.field(name_field())
  |> schema.field(email_field())
  |> schema.field(role_field())
}

fn id_field() -> schema.FieldDefinition {
  schema.field_def("id", schema.non_null(schema.id_type()))
  |> schema.resolver(fn(info: schema.ResolverInfo) {
    extract_field(info.parent, "id")
  })
}

fn name_field() -> schema.FieldDefinition {
  schema.field_def("name", schema.non_null(schema.string_type()))
  |> schema.resolver(fn(info: schema.ResolverInfo) {
    extract_field(info.parent, "name")
  })
}

fn email_field() -> schema.FieldDefinition {
  schema.field_def("email", schema.non_null(schema.string_type()))
  |> schema.resolver(fn(info: schema.ResolverInfo) {
    extract_field(info.parent, "email")
  })
}

fn role_field() -> schema.FieldDefinition {
  schema.field_def("role", schema.non_null(schema.Named("Role")))
  |> schema.resolver(fn(info: schema.ResolverInfo) {
    extract_field(info.parent, "role")
  })
}

/// Extract a field from Dynamic parent value
/// Uses FFI because we need to access dynamic map fields
fn extract_field(
  parent: option.Option(Dynamic),
  field: String,
) -> Result(Dynamic, String) {
  case parent {
    option.Some(p) -> {
      case get_field_ffi(p, field) {
        option.Some(value) -> Ok(value)
        option.None -> Error("Field not found: " <> field)
      }
    }
    option.None -> Error("No parent value")
  }
}

/// FFI for getting a field from a dynamic map
@external(erlang, "mochi_wisp_ffi", "get_field_safe")
@external(javascript, "../../mochi_wisp_ffi.mjs", "get_field_safe")
fn get_field_ffi(data: Dynamic, field: String) -> option.Option(Dynamic)

// ============================================================================
// Encoders - Convert Gleam types to Dynamic for GraphQL response
// ============================================================================

pub fn user_to_dynamic(user: User) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(user.id)),
      #("name", types.to_dynamic(user.name)),
      #("email", types.to_dynamic(user.email)),
      #("role", types.to_dynamic(role_to_string(user.role))),
    ]),
  )
}

pub fn users_encoder(users: List(User)) -> Dynamic {
  types.to_dynamic(list.map(users, user_to_dynamic))
}

pub fn user_encoder(user: User) -> Dynamic {
  user_to_dynamic(user)
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

// ============================================================================
// Schema Builder
// ============================================================================

pub fn build_schema() -> schema.Schema {
  let user_t = user_type()

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
      // topic name
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
  |> query.add_type(user_t)
  |> query.add_type(message_type())
  |> query.add_enum(role_enum())
  |> query.build
}

// ============================================================================
// Message Type for Subscriptions
// ============================================================================

pub type Message {
  Message(id: String, content: String, channel: String, timestamp: Int)
}

pub fn message_type() -> schema.ObjectType {
  schema.object("Message")
  |> schema.description("A message in a channel")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.id_type()))
    |> schema.resolver(fn(info: schema.ResolverInfo) {
      extract_field(info.parent, "id")
    }),
  )
  |> schema.field(
    schema.field_def("content", schema.non_null(schema.string_type()))
    |> schema.resolver(fn(info: schema.ResolverInfo) {
      extract_field(info.parent, "content")
    }),
  )
  |> schema.field(
    schema.field_def("channel", schema.non_null(schema.string_type()))
    |> schema.resolver(fn(info: schema.ResolverInfo) {
      extract_field(info.parent, "channel")
    }),
  )
  |> schema.field(
    schema.field_def("timestamp", schema.non_null(schema.int_type()))
    |> schema.resolver(fn(info: schema.ResolverInfo) {
      extract_field(info.parent, "timestamp")
    }),
  )
}

pub fn message_to_dynamic(msg: Message) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(msg.id)),
      #("content", types.to_dynamic(msg.content)),
      #("channel", types.to_dynamic(msg.channel)),
      #("timestamp", types.to_dynamic(msg.timestamp)),
    ]),
  )
}

pub fn message_encoder(msg: Message) -> Dynamic {
  message_to_dynamic(msg)
}

// ============================================================================
// Subscription Topic Resolvers
// ============================================================================

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
  // Return the topic name based on the channel argument
  Ok("message:" <> args.channel)
}
