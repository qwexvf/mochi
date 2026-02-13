// auth_example.gleam
// Demonstrates authentication and authorization patterns in Mochi
//
// This example shows:
// 1. Public queries (no auth required)
// 2. Protected queries (require authentication)
// 3. Role-based access (admin-only fields)
// 4. Field-level authorization with middleware

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/context
import mochi/executor
import mochi/middleware.{type MiddlewareDef, type MiddlewarePipeline}
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

pub type User {
  User(id: Int, name: String, email: String, role: Role)
}

pub type Role {
  Admin
  Member
  Guest
}

pub type Post {
  Post(id: Int, title: String, content: String, author_id: Int, is_draft: Bool)
}

// ============================================================================
// Mock Data
// ============================================================================

fn get_users() -> List(User) {
  [
    User(1, "Alice", "alice@example.com", Admin),
    User(2, "Bob", "bob@example.com", Member),
    User(3, "Charlie", "charlie@example.com", Guest),
  ]
}

fn get_posts() -> List(Post) {
  [
    Post(1, "Hello World", "My first post", 1, False),
    Post(2, "Draft Post", "Work in progress", 1, True),
    Post(3, "Public Post", "Everyone can see this", 2, False),
  ]
}

fn find_user(id: Int) -> Result(User, String) {
  get_users()
  |> list.find(fn(u) { u.id == id })
  |> result.map_error(fn(_) { "User not found" })
}

// ============================================================================
// Auth Context
// ============================================================================

/// Represents the authenticated user in context
pub type AuthContext {
  AuthContext(user: Option(User), is_authenticated: Bool)
}

/// Extract auth context from execution context
fn get_auth_context(ctx: schema.ExecutionContext) -> AuthContext {
  case decode.run(ctx.user_context, decode.dict(decode.string, decode.dynamic)) {
    Ok(ctx_dict) -> {
      case dict.get(ctx_dict, "current_user") {
        Ok(user_dyn) -> {
          case decode_user(user_dyn) {
            Ok(user) -> AuthContext(Some(user), True)
            Error(_) -> AuthContext(None, False)
          }
        }
        Error(_) -> AuthContext(None, False)
      }
    }
    Error(_) -> AuthContext(None, False)
  }
}

/// Check if current user has required role
fn has_role(ctx: schema.ExecutionContext, required: Role) -> Bool {
  case get_auth_context(ctx).user {
    Some(user) -> role_level(user.role) >= role_level(required)
    None -> False
  }
}

fn role_level(role: Role) -> Int {
  case role {
    Admin -> 100
    Member -> 50
    Guest -> 10
  }
}

fn role_to_string(role: Role) -> String {
  case role {
    Admin -> "ADMIN"
    Member -> "MEMBER"
    Guest -> "GUEST"
  }
}

// ============================================================================
// Decoders
// ============================================================================

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  let decoder =
    decode.into({
      use id <- decode.parameter
      use name <- decode.parameter
      use email <- decode.parameter
      use role <- decode.parameter
      User(id, name, email, role)
    })
    |> decode.field("id", decode.int)
    |> decode.field("name", decode.string)
    |> decode.field("email", decode.string)
    |> decode.field("role", decode.string |> decode.map(string_to_role))

  decode.run(dyn, decoder)
  |> result.map_error(fn(_) { "Failed to decode user" })
}

fn string_to_role(s: String) -> Role {
  case s {
    "ADMIN" -> Admin
    "MEMBER" -> Member
    _ -> Guest
  }
}

fn user_to_dynamic(u: User) -> Dynamic {
  types.record([
    types.field("id", u.id),
    types.field("name", u.name),
    types.field("email", u.email),
    types.field("role", role_to_string(u.role)),
  ])
}

fn post_to_dynamic(p: Post) -> Dynamic {
  types.record([
    types.field("id", p.id),
    types.field("title", p.title),
    types.field("content", p.content),
    types.field("authorId", p.author_id),
    types.field("isDraft", p.is_draft),
  ])
}

// ============================================================================
// GraphQL Schema
// ============================================================================

pub fn build_schema() -> schema.Schema {
  // User type
  let user_type =
    schema.object("User")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.resolver(field_resolver("id")),
    )
    |> schema.field(
      schema.field_def("name", schema.non_null(schema.string_type()))
      |> schema.resolver(field_resolver("name")),
    )
    |> schema.field(
      schema.field_def("email", schema.non_null(schema.string_type()))
      |> schema.field_description("Only visible to the user themselves or admins")
      |> schema.resolver(email_resolver()),
    )
    |> schema.field(
      schema.field_def("role", schema.non_null(schema.string_type()))
      |> schema.resolver(field_resolver("role")),
    )

  // Post type
  let post_type =
    schema.object("Post")
    |> schema.field(
      schema.field_def("id", schema.non_null(schema.id_type()))
      |> schema.resolver(field_resolver("id")),
    )
    |> schema.field(
      schema.field_def("title", schema.non_null(schema.string_type()))
      |> schema.resolver(field_resolver("title")),
    )
    |> schema.field(
      schema.field_def("content", schema.non_null(schema.string_type()))
      |> schema.resolver(field_resolver("content")),
    )
    |> schema.field(
      schema.field_def("isDraft", schema.non_null(schema.boolean_type()))
      |> schema.resolver(field_resolver("isDraft")),
    )
    |> schema.field(
      schema.field_def("author", schema.named_type("User"))
      |> schema.resolver(author_resolver()),
    )

  // Query type
  let query_type =
    schema.object("Query")
    // Public query - no auth required
    |> schema.field(
      schema.field_def("publicPosts", schema.list_type(schema.named_type("Post")))
      |> schema.field_description("Get all published posts (public)")
      |> schema.resolver(public_posts_resolver()),
    )
    // Protected query - requires authentication
    |> schema.field(
      schema.field_def("myPosts", schema.list_type(schema.named_type("Post")))
      |> schema.field_description("Get current user's posts (requires auth)")
      |> schema.resolver(my_posts_resolver()),
    )
    // Protected query - requires authentication
    |> schema.field(
      schema.field_def("me", schema.named_type("User"))
      |> schema.field_description("Get current user (requires auth)")
      |> schema.resolver(me_resolver()),
    )
    // Admin-only query
    |> schema.field(
      schema.field_def("allUsers", schema.list_type(schema.named_type("User")))
      |> schema.field_description("Get all users (admin only)")
      |> schema.resolver(all_users_resolver()),
    )
    // Admin-only query
    |> schema.field(
      schema.field_def("allPosts", schema.list_type(schema.named_type("Post")))
      |> schema.field_description("Get all posts including drafts (admin only)")
      |> schema.resolver(all_posts_resolver()),
    )
    // Public query with optional auth enhancement
    |> schema.field(
      schema.field_def("user", schema.named_type("User"))
      |> schema.field_description("Get user by ID (public, but email hidden for non-admins)")
      |> schema.argument(schema.arg("id", schema.non_null(schema.id_type())))
      |> schema.resolver(user_resolver()),
    )

  query.new()
  |> query.with_query(query_type)
  |> query.add_type(schema.ObjectTypeDef(user_type))
  |> query.add_type(schema.ObjectTypeDef(post_type))
  |> query.build
}

// ============================================================================
// Resolvers
// ============================================================================

/// Generic field resolver for simple field extraction
fn field_resolver(field_name: String) -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) -> {
        case
          decode.run(parent, decode.dict(decode.string, decode.dynamic))
        {
          Ok(d) -> {
            case dict.get(d, field_name) {
              Ok(value) -> Ok(value)
              Error(_) -> Error("Field not found: " <> field_name)
            }
          }
          Error(_) -> Error("Invalid parent type")
        }
      }
      None -> Error("No parent")
    }
  }
}

/// Email resolver - only shows email to self or admins
fn email_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) -> {
        let auth = get_auth_context(info.context)

        // Get the user being queried
        case decode.run(parent, decode.dict(decode.string, decode.dynamic)) {
          Ok(parent_dict) -> {
            case dict.get(parent_dict, "email") {
              Ok(email) -> {
                // Check authorization
                case auth.user {
                  Some(current_user) -> {
                    // Get the ID of the user being queried
                    case dict.get(parent_dict, "id") {
                      Ok(id_dyn) -> {
                        case decode.run(id_dyn, decode.int) {
                          Ok(user_id) -> {
                            // Allow if admin or viewing own profile
                            case
                              current_user.role == Admin
                              || current_user.id == user_id
                            {
                              True -> Ok(email)
                              False -> Ok(types.to_dynamic("[hidden]"))
                            }
                          }
                          Error(_) -> Ok(types.to_dynamic("[hidden]"))
                        }
                      }
                      Error(_) -> Ok(types.to_dynamic("[hidden]"))
                    }
                  }
                  None -> Ok(types.to_dynamic("[hidden]"))
                }
              }
              Error(_) -> Error("Email field not found")
            }
          }
          Error(_) -> Error("Invalid parent")
        }
      }
      None -> Error("No parent")
    }
  }
}

/// Author resolver for posts
fn author_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) -> {
        case decode.run(parent, decode.dict(decode.string, decode.dynamic)) {
          Ok(d) -> {
            case dict.get(d, "authorId") {
              Ok(author_id_dyn) -> {
                case decode.run(author_id_dyn, decode.int) {
                  Ok(author_id) -> {
                    case find_user(author_id) {
                      Ok(user) -> Ok(user_to_dynamic(user))
                      Error(e) -> Error(e)
                    }
                  }
                  Error(_) -> Error("Invalid author ID")
                }
              }
              Error(_) -> Error("No authorId field")
            }
          }
          Error(_) -> Error("Invalid parent")
        }
      }
      None -> Error("No parent")
    }
  }
}

/// Public posts resolver - returns only published posts
fn public_posts_resolver() -> schema.Resolver {
  fn(_info: schema.ResolverInfo) {
    let published =
      get_posts()
      |> list.filter(fn(p) { !p.is_draft })
      |> list.map(post_to_dynamic)
    Ok(types.to_dynamic(published))
  }
}

/// My posts resolver - requires authentication
fn my_posts_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    let auth = get_auth_context(info.context)
    case auth.user {
      Some(user) -> {
        let my_posts =
          get_posts()
          |> list.filter(fn(p) { p.author_id == user.id })
          |> list.map(post_to_dynamic)
        Ok(types.to_dynamic(my_posts))
      }
      None -> Error("Authentication required")
    }
  }
}

/// Me resolver - returns current user
fn me_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    let auth = get_auth_context(info.context)
    case auth.user {
      Some(user) -> Ok(user_to_dynamic(user))
      None -> Error("Authentication required")
    }
  }
}

/// All users resolver - admin only
fn all_users_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    case has_role(info.context, Admin) {
      True -> {
        let users = get_users() |> list.map(user_to_dynamic)
        Ok(types.to_dynamic(users))
      }
      False -> Error("Forbidden: Admin access required")
    }
  }
}

/// All posts resolver - admin only, includes drafts
fn all_posts_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    case has_role(info.context, Admin) {
      True -> {
        let posts = get_posts() |> list.map(post_to_dynamic)
        Ok(types.to_dynamic(posts))
      }
      False -> Error("Forbidden: Admin access required")
    }
  }
}

/// User by ID resolver
fn user_resolver() -> schema.Resolver {
  fn(info: schema.ResolverInfo) {
    case dict.get(info.arguments, "id") {
      Ok(id_dyn) -> {
        case decode.run(id_dyn, decode.int) {
          Ok(id) -> {
            case find_user(id) {
              Ok(user) -> Ok(user_to_dynamic(user))
              Error(e) -> Error(e)
            }
          }
          Error(_) -> {
            // Try as string
            case decode.run(id_dyn, decode.string) {
              Ok(id_str) -> {
                case
                  id_str
                  |> dynamic.from
                  |> decode.run(decode.int)
                {
                  Ok(id) -> {
                    case find_user(id) {
                      Ok(user) -> Ok(user_to_dynamic(user))
                      Error(e) -> Error(e)
                    }
                  }
                  Error(_) -> Error("Invalid user ID")
                }
              }
              Error(_) -> Error("Invalid user ID")
            }
          }
        }
      }
      Error(_) -> Error("Missing id argument")
    }
  }
}

// ============================================================================
// Middleware Setup
// ============================================================================

/// Create auth middleware that requires authentication for specific fields
pub fn create_auth_middleware() -> MiddlewarePipeline {
  middleware.new_pipeline()
  |> middleware.add_middleware(
    middleware.middleware("require_auth", fn(resolution, next) {
      let auth = get_auth_context(resolution.context.info.context)
      case auth.is_authenticated {
        True -> next(resolution)
        False -> middleware.set_error(resolution, "Authentication required")
      }
    })
    |> middleware.with_filter(middleware.NamedFields(["myPosts", "me"]))
    |> middleware.with_priority(10),
  )
  |> middleware.add_middleware(
    middleware.middleware("require_admin", fn(resolution, next) {
      case has_role(resolution.context.info.context, Admin) {
        True -> next(resolution)
        False ->
          middleware.set_error(resolution, "Forbidden: Admin access required")
      }
    })
    |> middleware.with_filter(middleware.NamedFields(["allUsers", "allPosts"]))
    |> middleware.with_priority(10),
  )
}

// ============================================================================
// Context Builder
// ============================================================================

/// Simulate JWT token validation and user lookup
fn validate_token(token: String) -> Result(User, String) {
  // In production, decode JWT and lookup user
  case token {
    "admin-token" -> Ok(User(1, "Alice", "alice@example.com", Admin))
    "member-token" -> Ok(User(2, "Bob", "bob@example.com", Member))
    "guest-token" -> Ok(User(3, "Charlie", "charlie@example.com", Guest))
    _ -> Error("Invalid token")
  }
}

/// Create execution context from request headers
pub fn create_context_from_headers(
  headers: Dict(String, String),
) -> schema.ExecutionContext {
  let auth_context = case dict.get(headers, "authorization") {
    Ok(auth_header) -> {
      case auth_header {
        "Bearer " <> token -> {
          case validate_token(token) {
            Ok(user) ->
              dict.from_list([
                #("current_user", user_to_dynamic(user)),
                #("is_authenticated", types.to_dynamic(True)),
              ])
            Error(_) ->
              dict.from_list([#("is_authenticated", types.to_dynamic(False))])
          }
        }
        _ -> dict.from_list([#("is_authenticated", types.to_dynamic(False))])
      }
    }
    Error(_) -> dict.from_list([#("is_authenticated", types.to_dynamic(False))])
  }

  schema.execution_context(types.to_dynamic(auth_context))
}

// ============================================================================
// Demo
// ============================================================================

pub fn main() {
  io.println("=== Mochi Auth Example ===\n")

  let my_schema = build_schema()

  // Test 1: Public query (no auth)
  io.println("1. Public query (no auth):")
  io.println("   Query: { publicPosts { title } }")
  let ctx1 = create_context_from_headers(dict.new())
  let result1 = executor.execute_query_with_context(my_schema, "{ publicPosts { title } }", ctx1)
  io.println("   Result: " <> result_summary(result1))
  io.println("")

  // Test 2: Protected query without auth
  io.println("2. Protected query WITHOUT auth:")
  io.println("   Query: { me { name } }")
  let ctx2 = create_context_from_headers(dict.new())
  let result2 = executor.execute_query_with_context(my_schema, "{ me { name } }", ctx2)
  io.println("   Result: " <> result_summary(result2))
  io.println("")

  // Test 3: Protected query with auth
  io.println("3. Protected query WITH auth (member):")
  io.println("   Query: { me { name email } }")
  let ctx3 =
    create_context_from_headers(
      dict.from_list([#("authorization", "Bearer member-token")]),
    )
  let result3 = executor.execute_query_with_context(my_schema, "{ me { name email } }", ctx3)
  io.println("   Result: " <> result_summary(result3))
  io.println("")

  // Test 4: Admin-only query with member token
  io.println("4. Admin query with MEMBER token:")
  io.println("   Query: { allUsers { name } }")
  let ctx4 =
    create_context_from_headers(
      dict.from_list([#("authorization", "Bearer member-token")]),
    )
  let result4 = executor.execute_query_with_context(my_schema, "{ allUsers { name } }", ctx4)
  io.println("   Result: " <> result_summary(result4))
  io.println("")

  // Test 5: Admin-only query with admin token
  io.println("5. Admin query with ADMIN token:")
  io.println("   Query: { allUsers { name email } }")
  let ctx5 =
    create_context_from_headers(
      dict.from_list([#("authorization", "Bearer admin-token")]),
    )
  let result5 = executor.execute_query_with_context(my_schema, "{ allUsers { name email } }", ctx5)
  io.println("   Result: " <> result_summary(result5))
  io.println("")

  // Test 6: Field-level auth (email visibility)
  io.println("6. Field-level auth - viewing another user's email:")
  io.println("   Query: { user(id: 2) { name email } } as member")
  let ctx6 =
    create_context_from_headers(
      dict.from_list([#("authorization", "Bearer member-token")]),
    )
  let result6 = executor.execute_query_with_context(my_schema, "{ user(id: \"2\") { name email } }", ctx6)
  io.println("   Result: " <> result_summary(result6))
  io.println("   (Email should be [hidden] since member can't see other's email)")
  io.println("")

  // Test 7: Field-level auth (admin can see all emails)
  io.println("7. Field-level auth - admin viewing user's email:")
  io.println("   Query: { user(id: 2) { name email } } as admin")
  let ctx7 =
    create_context_from_headers(
      dict.from_list([#("authorization", "Bearer admin-token")]),
    )
  let result7 = executor.execute_query_with_context(my_schema, "{ user(id: \"2\") { name email } }", ctx7)
  io.println("   Result: " <> result_summary(result7))
  io.println("   (Admin can see the actual email)")

  io.println("\n=== Auth Patterns Summary ===")
  io.println("- publicPosts: No auth required")
  io.println("- me, myPosts: Requires authentication")
  io.println("- allUsers, allPosts: Requires admin role")
  io.println("- user.email: Hidden unless viewing own profile or admin")
}

fn result_summary(result: executor.ExecutionResult) -> String {
  case result.errors {
    [] -> "Success"
    errors -> {
      let msgs =
        list.map(errors, fn(e) { e.message })
        |> list.fold("", fn(acc, msg) {
          case acc {
            "" -> msg
            _ -> acc <> ", " <> msg
          }
        })
      "Error: " <> msgs
    }
  }
}
