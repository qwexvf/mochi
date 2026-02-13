// mochi_wisp/complex_schema.gleam
// Realistic GraphQL schema for benchmarking real-world scenarios
// Includes: Users, Posts, Comments, Mutations, Subscriptions, Input Types

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/query
import mochi/schema
import mochi/types

// ============================================================================
// Domain Types
// ============================================================================

pub type User {
  User(
    id: String,
    username: String,
    email: String,
    display_name: String,
    bio: Option(String),
    role: Role,
    created_at: String,
    updated_at: String,
  )
}

pub type Role {
  Admin
  Moderator
  Member
  Guest
}

pub type Post {
  Post(
    id: String,
    title: String,
    content: String,
    excerpt: Option(String),
    author_id: String,
    status: PostStatus,
    tags: List(String),
    view_count: Int,
    created_at: String,
    updated_at: String,
  )
}

pub type PostStatus {
  Draft
  Published
  Archived
}

pub type Comment {
  Comment(
    id: String,
    content: String,
    author_id: String,
    post_id: String,
    parent_id: Option(String),
    created_at: String,
  )
}

pub type PageInfo {
  PageInfo(
    has_next_page: Bool,
    has_previous_page: Bool,
    start_cursor: Option(String),
    end_cursor: Option(String),
  )
}

pub type PostConnection {
  PostConnection(edges: List(PostEdge), page_info: PageInfo, total_count: Int)
}

pub type PostEdge {
  PostEdge(node: Post, cursor: String)
}

// Input Types
pub type CreateUserInput {
  CreateUserInput(
    username: String,
    email: String,
    display_name: String,
    bio: Option(String),
    role: Option(Role),
  )
}

pub type UpdateUserInput {
  UpdateUserInput(
    display_name: Option(String),
    bio: Option(String),
    role: Option(Role),
  )
}

pub type CreatePostInput {
  CreatePostInput(
    title: String,
    content: String,
    excerpt: Option(String),
    tags: Option(List(String)),
    status: Option(PostStatus),
  )
}

pub type PostFilterInput {
  PostFilterInput(
    status: Option(PostStatus),
    author_id: Option(String),
    tags: Option(List(String)),
    search: Option(String),
  )
}

// ============================================================================
// Sample Data
// ============================================================================

pub fn sample_users() -> List(User) {
  [
    User(
      id: "user-1",
      username: "alice",
      email: "alice@example.com",
      display_name: "Alice Johnson",
      bio: Some("Software engineer passionate about Gleam"),
      role: Admin,
      created_at: "2024-01-15T10:30:00Z",
      updated_at: "2024-03-20T14:45:00Z",
    ),
    User(
      id: "user-2",
      username: "bob",
      email: "bob@example.com",
      display_name: "Bob Smith",
      bio: None,
      role: Moderator,
      created_at: "2024-02-01T08:00:00Z",
      updated_at: "2024-02-01T08:00:00Z",
    ),
    User(
      id: "user-3",
      username: "charlie",
      email: "charlie@example.com",
      display_name: "Charlie Brown",
      bio: Some("GraphQL enthusiast"),
      role: Member,
      created_at: "2024-02-15T12:00:00Z",
      updated_at: "2024-03-01T09:30:00Z",
    ),
    User(
      id: "user-4",
      username: "diana",
      email: "diana@example.com",
      display_name: "Diana Prince",
      bio: Some("Full-stack developer"),
      role: Member,
      created_at: "2024-03-01T16:20:00Z",
      updated_at: "2024-03-15T11:00:00Z",
    ),
    User(
      id: "user-5",
      username: "guest",
      email: "guest@example.com",
      display_name: "Guest User",
      bio: None,
      role: Guest,
      created_at: "2024-03-10T00:00:00Z",
      updated_at: "2024-03-10T00:00:00Z",
    ),
  ]
}

pub fn sample_posts() -> List(Post) {
  [
    Post(
      id: "post-1",
      title: "Getting Started with Gleam",
      content: "Gleam is a friendly language for building type-safe systems...",
      excerpt: Some("An introduction to the Gleam programming language"),
      author_id: "user-1",
      status: Published,
      tags: ["gleam", "tutorial", "beginners"],
      view_count: 1542,
      created_at: "2024-02-01T10:00:00Z",
      updated_at: "2024-02-15T14:30:00Z",
    ),
    Post(
      id: "post-2",
      title: "Building GraphQL APIs with Mochi",
      content: "Mochi is a code-first GraphQL library for Gleam...",
      excerpt: Some("Learn how to build GraphQL APIs using Mochi"),
      author_id: "user-1",
      status: Published,
      tags: ["gleam", "graphql", "mochi", "api"],
      view_count: 892,
      created_at: "2024-02-20T08:00:00Z",
      updated_at: "2024-03-01T16:00:00Z",
    ),
    Post(
      id: "post-3",
      title: "Advanced Type Safety Patterns",
      content: "Exploring advanced patterns for type-safe programming...",
      excerpt: None,
      author_id: "user-3",
      status: Draft,
      tags: ["types", "patterns", "advanced"],
      view_count: 0,
      created_at: "2024-03-15T11:00:00Z",
      updated_at: "2024-03-15T11:00:00Z",
    ),
    Post(
      id: "post-4",
      title: "Performance Optimization Techniques",
      content: "How to optimize your Gleam applications for performance...",
      excerpt: Some("Tips and tricks for high-performance Gleam code"),
      author_id: "user-2",
      status: Published,
      tags: ["performance", "optimization", "gleam"],
      view_count: 456,
      created_at: "2024-03-10T09:00:00Z",
      updated_at: "2024-03-12T15:20:00Z",
    ),
    Post(
      id: "post-5",
      title: "Archived Post Example",
      content: "This post has been archived...",
      excerpt: None,
      author_id: "user-1",
      status: Archived,
      tags: ["archive"],
      view_count: 123,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-02-01T00:00:00Z",
    ),
  ]
}

pub fn sample_comments() -> List(Comment) {
  [
    Comment(
      id: "comment-1",
      content: "Great article! Very helpful for beginners.",
      author_id: "user-2",
      post_id: "post-1",
      parent_id: None,
      created_at: "2024-02-02T14:30:00Z",
    ),
    Comment(
      id: "comment-2",
      content: "Thanks for the feedback!",
      author_id: "user-1",
      post_id: "post-1",
      parent_id: Some("comment-1"),
      created_at: "2024-02-02T15:00:00Z",
    ),
    Comment(
      id: "comment-3",
      content: "Could you elaborate on the error handling section?",
      author_id: "user-3",
      post_id: "post-1",
      parent_id: None,
      created_at: "2024-02-03T09:00:00Z",
    ),
    Comment(
      id: "comment-4",
      content: "Mochi looks amazing! Can't wait to try it.",
      author_id: "user-4",
      post_id: "post-2",
      parent_id: None,
      created_at: "2024-02-21T10:00:00Z",
    ),
    Comment(
      id: "comment-5",
      content: "I've been using it in production, works great!",
      author_id: "user-2",
      post_id: "post-2",
      parent_id: Some("comment-4"),
      created_at: "2024-02-22T08:30:00Z",
    ),
  ]
}

// ============================================================================
// Data Access
// ============================================================================

pub fn find_user_by_id(id: String) -> Option(User) {
  list.find(sample_users(), fn(u) { u.id == id })
  |> option.from_result
}

pub fn find_post_by_id(id: String) -> Option(Post) {
  list.find(sample_posts(), fn(p) { p.id == id })
  |> option.from_result
}

pub fn find_comment_by_id(id: String) -> Option(Comment) {
  list.find(sample_comments(), fn(c) { c.id == id })
  |> option.from_result
}

pub fn find_posts_by_author(author_id: String) -> List(Post) {
  list.filter(sample_posts(), fn(p) { p.author_id == author_id })
}

pub fn find_comments_by_post(post_id: String) -> List(Comment) {
  list.filter(sample_comments(), fn(c) { c.post_id == post_id })
}

pub fn find_comments_by_author(author_id: String) -> List(Comment) {
  list.filter(sample_comments(), fn(c) { c.author_id == author_id })
}

pub fn find_replies(parent_id: String) -> List(Comment) {
  list.filter(sample_comments(), fn(c) { c.parent_id == Some(parent_id) })
}

// ============================================================================
// Enum Conversions
// ============================================================================

pub fn role_to_string(role: Role) -> String {
  case role {
    Admin -> "ADMIN"
    Moderator -> "MODERATOR"
    Member -> "MEMBER"
    Guest -> "GUEST"
  }
}

pub fn status_to_string(status: PostStatus) -> String {
  case status {
    Draft -> "DRAFT"
    Published -> "PUBLISHED"
    Archived -> "ARCHIVED"
  }
}

// ============================================================================
// Dynamic Encoders
// ============================================================================

pub fn user_to_dynamic(user: User) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(user.id)),
      #("username", types.to_dynamic(user.username)),
      #("email", types.to_dynamic(user.email)),
      #("displayName", types.to_dynamic(user.display_name)),
      #("bio", option_to_dynamic(user.bio, types.to_dynamic)),
      #("role", types.to_dynamic(role_to_string(user.role))),
      #("createdAt", types.to_dynamic(user.created_at)),
      #("updatedAt", types.to_dynamic(user.updated_at)),
    ]),
  )
}

pub fn post_to_dynamic(post: Post) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(post.id)),
      #("title", types.to_dynamic(post.title)),
      #("content", types.to_dynamic(post.content)),
      #("excerpt", option_to_dynamic(post.excerpt, types.to_dynamic)),
      #("authorId", types.to_dynamic(post.author_id)),
      #("status", types.to_dynamic(status_to_string(post.status))),
      #("tags", types.to_dynamic(post.tags)),
      #("viewCount", types.to_dynamic(post.view_count)),
      #("createdAt", types.to_dynamic(post.created_at)),
      #("updatedAt", types.to_dynamic(post.updated_at)),
    ]),
  )
}

pub fn comment_to_dynamic(comment: Comment) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(comment.id)),
      #("content", types.to_dynamic(comment.content)),
      #("authorId", types.to_dynamic(comment.author_id)),
      #("postId", types.to_dynamic(comment.post_id)),
      #("parentId", option_to_dynamic(comment.parent_id, types.to_dynamic)),
      #("createdAt", types.to_dynamic(comment.created_at)),
    ]),
  )
}

fn option_to_dynamic(opt: Option(a), encoder: fn(a) -> Dynamic) -> Dynamic {
  case opt {
    Some(v) -> encoder(v)
    None -> types.to_dynamic(Nil)
  }
}

// ============================================================================
// Type Definitions
// ============================================================================

pub fn role_enum() -> schema.EnumType {
  types.enum_type("Role")
  |> types.enum_description("User role determining access levels")
  |> types.value_with_desc("ADMIN", "Full administrative access")
  |> types.value_with_desc("MODERATOR", "Content moderation access")
  |> types.value_with_desc("MEMBER", "Standard member access")
  |> types.value_with_desc("GUEST", "Limited guest access")
  |> types.build_enum
}

pub fn post_status_enum() -> schema.EnumType {
  types.enum_type("PostStatus")
  |> types.enum_description("Publication status of a post")
  |> types.value_with_desc("DRAFT", "Not yet published")
  |> types.value_with_desc("PUBLISHED", "Publicly visible")
  |> types.value_with_desc("ARCHIVED", "No longer active")
  |> types.build_enum
}

pub fn user_type() -> schema.ObjectType {
  schema.object("User")
  |> schema.description("A user account in the system")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.id_type()))
    |> schema.resolver(field_resolver("id")),
  )
  |> schema.field(
    schema.field_def("username", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("username")),
  )
  |> schema.field(
    schema.field_def("email", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("email")),
  )
  |> schema.field(
    schema.field_def("displayName", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("displayName")),
  )
  |> schema.field(
    schema.field_def("bio", schema.string_type())
    |> schema.resolver(field_resolver("bio")),
  )
  |> schema.field(
    schema.field_def("role", schema.non_null(schema.Named("Role")))
    |> schema.resolver(field_resolver("role")),
  )
  |> schema.field(
    schema.field_def("createdAt", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("createdAt")),
  )
  |> schema.field(
    schema.field_def("updatedAt", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("updatedAt")),
  )
  |> schema.field(
    schema.field_def(
      "posts",
      schema.non_null(schema.List(schema.non_null(schema.Named("Post")))),
    )
    |> schema.resolver(user_posts_resolver()),
  )
  |> schema.field(
    schema.field_def(
      "comments",
      schema.non_null(schema.List(schema.non_null(schema.Named("Comment")))),
    )
    |> schema.resolver(user_comments_resolver()),
  )
}

pub fn post_type() -> schema.ObjectType {
  schema.object("Post")
  |> schema.description("A blog post")
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
    schema.field_def("excerpt", schema.string_type())
    |> schema.resolver(field_resolver("excerpt")),
  )
  |> schema.field(
    schema.field_def("status", schema.non_null(schema.Named("PostStatus")))
    |> schema.resolver(field_resolver("status")),
  )
  |> schema.field(
    schema.field_def(
      "tags",
      schema.non_null(schema.List(schema.non_null(schema.string_type()))),
    )
    |> schema.resolver(field_resolver("tags")),
  )
  |> schema.field(
    schema.field_def("viewCount", schema.non_null(schema.int_type()))
    |> schema.resolver(field_resolver("viewCount")),
  )
  |> schema.field(
    schema.field_def("createdAt", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("createdAt")),
  )
  |> schema.field(
    schema.field_def("updatedAt", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("updatedAt")),
  )
  |> schema.field(
    schema.field_def("author", schema.non_null(schema.Named("User")))
    |> schema.resolver(post_author_resolver()),
  )
  |> schema.field(
    schema.field_def(
      "comments",
      schema.non_null(schema.List(schema.non_null(schema.Named("Comment")))),
    )
    |> schema.resolver(post_comments_resolver()),
  )
}

pub fn comment_type() -> schema.ObjectType {
  schema.object("Comment")
  |> schema.description("A comment on a post")
  |> schema.field(
    schema.field_def("id", schema.non_null(schema.id_type()))
    |> schema.resolver(field_resolver("id")),
  )
  |> schema.field(
    schema.field_def("content", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("content")),
  )
  |> schema.field(
    schema.field_def("createdAt", schema.non_null(schema.string_type()))
    |> schema.resolver(field_resolver("createdAt")),
  )
  |> schema.field(
    schema.field_def("author", schema.non_null(schema.Named("User")))
    |> schema.resolver(comment_author_resolver()),
  )
  |> schema.field(
    schema.field_def("post", schema.non_null(schema.Named("Post")))
    |> schema.resolver(comment_post_resolver()),
  )
  |> schema.field(
    schema.field_def("parent", schema.Named("Comment"))
    |> schema.resolver(comment_parent_resolver()),
  )
  |> schema.field(
    schema.field_def(
      "replies",
      schema.non_null(schema.List(schema.non_null(schema.Named("Comment")))),
    )
    |> schema.resolver(comment_replies_resolver()),
  )
}

// ============================================================================
// Field Resolvers
// ============================================================================

@external(erlang, "mochi_wisp_ffi", "get_field_safe")
@external(javascript, "../../mochi_wisp_ffi.mjs", "get_field_safe")
fn get_field_ffi(data: Dynamic, field: String) -> option.Option(Dynamic)

fn field_resolver(
  field_name: String,
) -> fn(schema.ResolverInfo) -> Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, field_name) {
          Some(value) -> Ok(value)
          None -> Error("Field not found: " <> field_name)
        }
      None -> Error("No parent value")
    }
  }
}

fn user_posts_resolver() -> fn(schema.ResolverInfo) -> Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "id") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(user_id) -> {
                let posts = find_posts_by_author(user_id)
                Ok(types.to_dynamic(list.map(posts, post_to_dynamic)))
              }
              Error(_) -> Error("Invalid user id")
            }
          None -> Error("User id not found")
        }
      None -> Error("No parent value")
    }
  }
}

fn user_comments_resolver() -> fn(schema.ResolverInfo) ->
  Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "id") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(user_id) -> {
                let comments = find_comments_by_author(user_id)
                Ok(types.to_dynamic(list.map(comments, comment_to_dynamic)))
              }
              Error(_) -> Error("Invalid user id")
            }
          None -> Error("User id not found")
        }
      None -> Error("No parent value")
    }
  }
}

fn post_author_resolver() -> fn(schema.ResolverInfo) -> Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "authorId") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(author_id) ->
                case find_user_by_id(author_id) {
                  Some(user) -> Ok(user_to_dynamic(user))
                  None -> Error("Author not found: " <> author_id)
                }
              Error(_) -> Error("Invalid author id")
            }
          None -> Error("Author id not found")
        }
      None -> Error("No parent value")
    }
  }
}

fn post_comments_resolver() -> fn(schema.ResolverInfo) ->
  Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "id") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(post_id) -> {
                let comments = find_comments_by_post(post_id)
                Ok(types.to_dynamic(list.map(comments, comment_to_dynamic)))
              }
              Error(_) -> Error("Invalid post id")
            }
          None -> Error("Post id not found")
        }
      None -> Error("No parent value")
    }
  }
}

fn comment_author_resolver() -> fn(schema.ResolverInfo) ->
  Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "authorId") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(author_id) ->
                case find_user_by_id(author_id) {
                  Some(user) -> Ok(user_to_dynamic(user))
                  None -> Error("Author not found: " <> author_id)
                }
              Error(_) -> Error("Invalid author id")
            }
          None -> Error("Author id not found")
        }
      None -> Error("No parent value")
    }
  }
}

fn comment_post_resolver() -> fn(schema.ResolverInfo) -> Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "postId") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(post_id) ->
                case find_post_by_id(post_id) {
                  Some(post) -> Ok(post_to_dynamic(post))
                  None -> Error("Post not found: " <> post_id)
                }
              Error(_) -> Error("Invalid post id")
            }
          None -> Error("Post id not found")
        }
      None -> Error("No parent value")
    }
  }
}

fn comment_parent_resolver() -> fn(schema.ResolverInfo) ->
  Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "parentId") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.optional(decode.string)) {
              Ok(Some(parent_id)) ->
                case find_comment_by_id(parent_id) {
                  Some(comment) -> Ok(comment_to_dynamic(comment))
                  None -> Ok(types.to_dynamic(Nil))
                }
              Ok(None) -> Ok(types.to_dynamic(Nil))
              Error(_) -> Ok(types.to_dynamic(Nil))
            }
          None -> Ok(types.to_dynamic(Nil))
        }
      None -> Error("No parent value")
    }
  }
}

fn comment_replies_resolver() -> fn(schema.ResolverInfo) ->
  Result(Dynamic, String) {
  fn(info: schema.ResolverInfo) {
    case info.parent {
      Some(parent) ->
        case get_field_ffi(parent, "id") {
          Some(id_dyn) ->
            case decode.run(id_dyn, decode.string) {
              Ok(comment_id) -> {
                let replies = find_replies(comment_id)
                Ok(types.to_dynamic(list.map(replies, comment_to_dynamic)))
              }
              Error(_) -> Error("Invalid comment id")
            }
          None -> Error("Comment id not found")
        }
      None -> Error("No parent value")
    }
  }
}

// ============================================================================
// Query Resolvers
// ============================================================================

pub fn users_resolver(
  _ctx: schema.ExecutionContext,
) -> Result(List(User), String) {
  Ok(sample_users())
}

pub fn posts_resolver(
  _ctx: schema.ExecutionContext,
) -> Result(List(Post), String) {
  Ok(sample_posts())
}

pub fn published_posts_resolver(
  _ctx: schema.ExecutionContext,
) -> Result(List(Post), String) {
  Ok(list.filter(sample_posts(), fn(p) { p.status == Published }))
}

pub type UserByIdArgs {
  UserByIdArgs(id: String)
}

pub fn decode_id_args(
  args: Dict(String, Dynamic),
) -> Result(UserByIdArgs, String) {
  case dict.get(args, "id") {
    Ok(id_dyn) ->
      case decode.run(id_dyn, decode.string) {
        Ok(id) -> Ok(UserByIdArgs(id))
        Error(_) -> Error("Invalid id")
      }
    Error(_) -> Error("Missing id")
  }
}

pub fn user_by_id_resolver(
  args: UserByIdArgs,
  _ctx: schema.ExecutionContext,
) -> Result(User, String) {
  case find_user_by_id(args.id) {
    Some(user) -> Ok(user)
    None -> Error("User not found")
  }
}

pub fn post_by_id_resolver(
  args: UserByIdArgs,
  _ctx: schema.ExecutionContext,
) -> Result(Post, String) {
  case find_post_by_id(args.id) {
    Some(post) -> Ok(post)
    None -> Error("Post not found")
  }
}

// ============================================================================
// Schema Builder
// ============================================================================

pub fn build_complex_schema() -> schema.Schema {
  // Queries
  let users_q =
    query.query(
      "users",
      schema.non_null(schema.List(schema.non_null(schema.Named("User")))),
      users_resolver,
      fn(users) { types.to_dynamic(list.map(users, user_to_dynamic)) },
    )
    |> query.query_description("Get all users")

  let user_q =
    query.query_with_args(
      "user",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.Named("User"),
      decode_id_args,
      user_by_id_resolver,
      user_to_dynamic,
    )
    |> query.query_description("Get user by ID")

  let posts_q =
    query.query(
      "posts",
      schema.non_null(schema.List(schema.non_null(schema.Named("Post")))),
      posts_resolver,
      fn(posts) { types.to_dynamic(list.map(posts, post_to_dynamic)) },
    )
    |> query.query_description("Get all posts")

  let post_q =
    query.query_with_args(
      "post",
      [query.arg("id", schema.non_null(schema.id_type()))],
      schema.Named("Post"),
      decode_id_args,
      post_by_id_resolver,
      post_to_dynamic,
    )
    |> query.query_description("Get post by ID")

  let published_posts_q =
    query.query(
      "publishedPosts",
      schema.non_null(schema.List(schema.non_null(schema.Named("Post")))),
      published_posts_resolver,
      fn(posts) { types.to_dynamic(list.map(posts, post_to_dynamic)) },
    )
    |> query.query_description("Get all published posts")

  // Build schema
  query.new()
  |> query.add_query(users_q)
  |> query.add_query(user_q)
  |> query.add_query(posts_q)
  |> query.add_query(post_q)
  |> query.add_query(published_posts_q)
  |> query.add_type(user_type())
  |> query.add_type(post_type())
  |> query.add_type(comment_type())
  |> query.add_enum(role_enum())
  |> query.add_enum(post_status_enum())
  |> query.build
}
