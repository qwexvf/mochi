// mochi_wisp/complex_schema.gleam
// Realistic GraphQL schema for benchmarking real-world scenarios
// Using high-level types API

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

pub fn string_to_role(s: String) -> Result(Role, String) {
  case s {
    "ADMIN" -> Ok(Admin)
    "MODERATOR" -> Ok(Moderator)
    "MEMBER" -> Ok(Member)
    "GUEST" -> Ok(Guest)
    _ -> Error("Unknown role: " <> s)
  }
}

pub fn status_to_string(status: PostStatus) -> String {
  case status {
    Draft -> "DRAFT"
    Published -> "PUBLISHED"
    Archived -> "ARCHIVED"
  }
}

pub fn string_to_status(s: String) -> Result(PostStatus, String) {
  case s {
    "DRAFT" -> Ok(Draft)
    "PUBLISHED" -> Ok(Published)
    "ARCHIVED" -> Ok(Archived)
    _ -> Error("Unknown status: " <> s)
  }
}

// ============================================================================
// Decoders
// ============================================================================

fn decode_user(dyn: Dynamic) -> Result(User, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use username <- decode.field("username", decode.string)
    use email <- decode.field("email", decode.string)
    use display_name <- decode.field("displayName", decode.string)
    use bio <- decode.field("bio", decode.optional(decode.string))
    use role_str <- decode.field("role", decode.string)
    use created_at <- decode.field("createdAt", decode.string)
    use updated_at <- decode.field("updatedAt", decode.string)
    decode.success(#(
      id,
      username,
      email,
      display_name,
      bio,
      role_str,
      created_at,
      updated_at,
    ))
  }
  case decode.run(dyn, decoder) {
    Ok(#(id, username, email, display_name, bio, role_str, created_at, updated_at)) ->
      case string_to_role(role_str) {
        Ok(role) ->
          Ok(User(
            id: id,
            username: username,
            email: email,
            display_name: display_name,
            bio: bio,
            role: role,
            created_at: created_at,
            updated_at: updated_at,
          ))
        Error(e) -> Error(e)
      }
    Error(_) -> Error("Failed to decode User")
  }
}

fn decode_post(dyn: Dynamic) -> Result(Post, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use title <- decode.field("title", decode.string)
    use content <- decode.field("content", decode.string)
    use excerpt <- decode.field("excerpt", decode.optional(decode.string))
    use author_id <- decode.field("authorId", decode.string)
    use status_str <- decode.field("status", decode.string)
    use tags <- decode.field("tags", decode.list(decode.string))
    use view_count <- decode.field("viewCount", decode.int)
    use created_at <- decode.field("createdAt", decode.string)
    use updated_at <- decode.field("updatedAt", decode.string)
    decode.success(#(
      id,
      title,
      content,
      excerpt,
      author_id,
      status_str,
      tags,
      view_count,
      created_at,
      updated_at,
    ))
  }
  case decode.run(dyn, decoder) {
    Ok(#(
      id,
      title,
      content,
      excerpt,
      author_id,
      status_str,
      tags,
      view_count,
      created_at,
      updated_at,
    )) ->
      case string_to_status(status_str) {
        Ok(status) ->
          Ok(Post(
            id: id,
            title: title,
            content: content,
            excerpt: excerpt,
            author_id: author_id,
            status: status,
            tags: tags,
            view_count: view_count,
            created_at: created_at,
            updated_at: updated_at,
          ))
        Error(e) -> Error(e)
      }
    Error(_) -> Error("Failed to decode Post")
  }
}

fn decode_comment(dyn: Dynamic) -> Result(Comment, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use content <- decode.field("content", decode.string)
    use author_id <- decode.field("authorId", decode.string)
    use post_id <- decode.field("postId", decode.string)
    use parent_id <- decode.field("parentId", decode.optional(decode.string))
    use created_at <- decode.field("createdAt", decode.string)
    decode.success(Comment(
      id: id,
      content: content,
      author_id: author_id,
      post_id: post_id,
      parent_id: parent_id,
      created_at: created_at,
    ))
  }
  decode.run(dyn, decoder)
  |> result_map_error("Failed to decode Comment")
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
    types.field("username", user.username),
    types.field("email", user.email),
    types.field("displayName", user.display_name),
    #("bio", types.option(user.bio)),
    types.field("role", role_to_string(user.role)),
    types.field("createdAt", user.created_at),
    types.field("updatedAt", user.updated_at),
  ])
}

pub fn post_to_dynamic(post: Post) -> Dynamic {
  types.record([
    types.field("id", post.id),
    types.field("title", post.title),
    types.field("content", post.content),
    #("excerpt", types.option(post.excerpt)),
    types.field("authorId", post.author_id),
    types.field("status", status_to_string(post.status)),
    types.field("tags", post.tags),
    types.field("viewCount", post.view_count),
    types.field("createdAt", post.created_at),
    types.field("updatedAt", post.updated_at),
  ])
}

pub fn comment_to_dynamic(comment: Comment) -> Dynamic {
  types.record([
    types.field("id", comment.id),
    types.field("content", comment.content),
    types.field("authorId", comment.author_id),
    types.field("postId", comment.post_id),
    #("parentId", types.option(comment.parent_id)),
    types.field("createdAt", comment.created_at),
  ])
}

// ============================================================================
// GraphQL Enum Types
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

// ============================================================================
// GraphQL Object Types - Using High-Level types API
// ============================================================================

pub fn user_type() -> schema.ObjectType {
  types.object("User")
  |> types.description("A user account in the system")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("username", fn(u: User) { u.username })
  |> types.string("email", fn(u: User) { u.email })
  |> types.string("displayName", fn(u: User) { u.display_name })
  |> types.optional_string("bio", fn(u: User) { u.bio })
  |> types.string("role", fn(u: User) { role_to_string(u.role) })
  |> types.string("createdAt", fn(u: User) { u.created_at })
  |> types.string("updatedAt", fn(u: User) { u.updated_at })
  |> types.build(decode_user)
  // Add relationship fields
  |> schema.field(
    schema.field_def(
      "posts",
      schema.non_null(schema.List(schema.non_null(schema.Named("Post")))),
    )
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_user(p) {
            Ok(user) -> {
              let posts = find_posts_by_author(user.id)
              Ok(types.to_dynamic(list.map(posts, post_to_dynamic)))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def(
      "comments",
      schema.non_null(schema.List(schema.non_null(schema.Named("Comment")))),
    )
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_user(p) {
            Ok(user) -> {
              let comments = find_comments_by_author(user.id)
              Ok(types.to_dynamic(list.map(comments, comment_to_dynamic)))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
}

pub fn post_type() -> schema.ObjectType {
  types.object("Post")
  |> types.description("A blog post")
  |> types.id("id", fn(p: Post) { p.id })
  |> types.string("title", fn(p: Post) { p.title })
  |> types.string("content", fn(p: Post) { p.content })
  |> types.optional_string("excerpt", fn(p: Post) { p.excerpt })
  |> types.string("status", fn(p: Post) { status_to_string(p.status) })
  |> types.list_string("tags", fn(p: Post) { p.tags })
  |> types.int("viewCount", fn(p: Post) { p.view_count })
  |> types.string("createdAt", fn(p: Post) { p.created_at })
  |> types.string("updatedAt", fn(p: Post) { p.updated_at })
  |> types.build(decode_post)
  // Add relationship fields
  |> schema.field(
    schema.field_def("author", schema.non_null(schema.Named("User")))
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_post(p) {
            Ok(post) ->
              case find_user_by_id(post.author_id) {
                Some(user) -> Ok(user_to_dynamic(user))
                None -> Error("Author not found: " <> post.author_id)
              }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def(
      "comments",
      schema.non_null(schema.List(schema.non_null(schema.Named("Comment")))),
    )
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_post(p) {
            Ok(post) -> {
              let comments = find_comments_by_post(post.id)
              Ok(types.to_dynamic(list.map(comments, comment_to_dynamic)))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
}

pub fn comment_type() -> schema.ObjectType {
  types.object("Comment")
  |> types.description("A comment on a post")
  |> types.id("id", fn(c: Comment) { c.id })
  |> types.string("content", fn(c: Comment) { c.content })
  |> types.string("createdAt", fn(c: Comment) { c.created_at })
  |> types.build(decode_comment)
  // Add relationship fields
  |> schema.field(
    schema.field_def("author", schema.non_null(schema.Named("User")))
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_comment(p) {
            Ok(comment) ->
              case find_user_by_id(comment.author_id) {
                Some(user) -> Ok(user_to_dynamic(user))
                None -> Error("Author not found: " <> comment.author_id)
              }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def("post", schema.non_null(schema.Named("Post")))
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_comment(p) {
            Ok(comment) ->
              case find_post_by_id(comment.post_id) {
                Some(post) -> Ok(post_to_dynamic(post))
                None -> Error("Post not found: " <> comment.post_id)
              }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def("parent", schema.Named("Comment"))
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_comment(p) {
            Ok(comment) ->
              case comment.parent_id {
                Some(parent_id) ->
                  case find_comment_by_id(parent_id) {
                    Some(parent) -> Ok(comment_to_dynamic(parent))
                    None -> Ok(types.to_dynamic(Nil))
                  }
                None -> Ok(types.to_dynamic(Nil))
              }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
  |> schema.field(
    schema.field_def(
      "replies",
      schema.non_null(schema.List(schema.non_null(schema.Named("Comment")))),
    )
    |> schema.resolver(fn(info) {
      case info.parent {
        Some(p) ->
          case decode_comment(p) {
            Ok(comment) -> {
              let replies = find_replies(comment.id)
              Ok(types.to_dynamic(list.map(replies, comment_to_dynamic)))
            }
            Error(e) -> Error(e)
          }
        None -> Error("No parent")
      }
    }),
  )
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
