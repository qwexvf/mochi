// Test that the bookstore split-schema pattern works end-to-end
// Mirrors examples/bookstore_split_schema.gleam

import gleam/list
import gleam/option.{None, Some}
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// -- Domain types --

pub type Book {
  Book(id: String, title: String, author_id: String)
}

pub type Author {
  Author(id: String, name: String)
}

pub type Review {
  Review(id: String, book_id: String, rating: Int, comment: String)
}

// -- Data --

fn all_books() {
  [
    Book("1", "Norwegian Wood", "1"),
    Book("2", "Kafka on the Shore", "1"),
    Book("3", "Kitchen", "2"),
  ]
}

fn all_authors() {
  [Author("1", "Haruki Murakami"), Author("2", "Banana Yoshimoto")]
}

// -- Split schemas --

fn book_schema() -> query.SchemaBuilder {
  let book_type =
    types.object("Book")
    |> types.id("id", fn(b: Book) { b.id })
    |> types.string("title", fn(b: Book) { b.title })
    |> types.build(fn(_d) { Ok(Book("1", "Norwegian Wood", "1")) })

  let books_query =
    query.query(
      "books",
      schema.list_type(schema.named_type("Book")),
      fn(_ctx) { Ok(all_books()) },
      fn(books) { types.to_dynamic(books) },
    )

  let book_query =
    query.query_with_args(
      name: "book",
      args: [query.arg("id", schema.non_null(schema.id_type()))],
      returns: schema.named_type("Book"),
      decode: fn(args) { query.get_id(args, "id") },
      resolve: fn(id, _ctx) {
        case list.find(all_books(), fn(b) { b.id == id }) {
          Ok(b) -> Ok(b)
          Error(_) -> Error("Not found")
        }
      },
      encode: fn(b) { types.to_dynamic(b) },
    )

  let add_book =
    query.mutation(
      name: "addBook",
      args: [
        query.arg("title", schema.non_null(schema.string_type())),
        query.arg("authorId", schema.non_null(schema.id_type())),
      ],
      returns: schema.non_null(schema.named_type("Book")),
      decode: fn(args) {
        case query.get_string(args, "title"), query.get_id(args, "authorId") {
          Ok(t), Ok(a) -> Ok(#(t, a))
          _, _ -> Error("Missing args")
        }
      },
      resolve: fn(input, _ctx) {
        let #(title, author_id) = input
        Ok(Book("new-1", title, author_id))
      },
      encode: fn(b) { types.to_dynamic(b) },
    )

  query.new()
  |> query.add_query(books_query)
  |> query.add_query(book_query)
  |> query.add_mutation(add_book)
  |> query.add_type(book_type)
}

fn author_schema() -> query.SchemaBuilder {
  let author_type =
    types.object("Author")
    |> types.id("id", fn(a: Author) { a.id })
    |> types.string("name", fn(a: Author) { a.name })
    |> types.build(fn(_d) { Ok(Author("1", "Haruki Murakami")) })

  let authors_query =
    query.query(
      "authors",
      schema.list_type(schema.named_type("Author")),
      fn(_ctx) { Ok(all_authors()) },
      fn(a) { types.to_dynamic(a) },
    )

  let author_query =
    query.query_with_args(
      name: "author",
      args: [query.arg("id", schema.non_null(schema.id_type()))],
      returns: schema.named_type("Author"),
      decode: fn(args) { query.get_id(args, "id") },
      resolve: fn(id, _ctx) {
        case list.find(all_authors(), fn(a) { a.id == id }) {
          Ok(a) -> Ok(a)
          Error(_) -> Error("Not found")
        }
      },
      encode: fn(a) { types.to_dynamic(a) },
    )

  query.new()
  |> query.add_query(authors_query)
  |> query.add_query(author_query)
  |> query.add_type(author_type)
}

fn review_schema() -> query.SchemaBuilder {
  let review_type =
    types.object("Review")
    |> types.id("id", fn(r: Review) { r.id })
    |> types.int("rating", fn(r: Review) { r.rating })
    |> types.string("comment", fn(r: Review) { r.comment })
    |> types.build(fn(_d) { Ok(Review("1", "1", 5, "Great")) })

  let add_review =
    query.mutation(
      name: "addReview",
      args: [
        query.arg("bookId", schema.non_null(schema.id_type())),
        query.arg("rating", schema.non_null(schema.int_type())),
        query.arg("comment", schema.string_type()),
      ],
      returns: schema.non_null(schema.named_type("Review")),
      decode: fn(args) {
        case query.get_id(args, "bookId"), query.get_int(args, "rating") {
          Ok(bid), Ok(r) -> {
            let c = case query.get_optional_string(args, "comment") {
              option.Some(v) -> v
              option.None -> ""
            }
            Ok(#(bid, r, c))
          }
          _, _ -> Error("Missing args")
        }
      },
      resolve: fn(input, _ctx) {
        let #(_bid, rating, comment) = input
        Ok(Review("new-1", "1", rating, comment))
      },
      encode: fn(r) { types.to_dynamic(r) },
    )

  query.new()
  |> query.add_mutation(add_review)
  |> query.add_type(review_type)
}

fn create_schema() -> schema.Schema {
  book_schema()
  |> query.merge(author_schema())
  |> query.merge(review_schema())
  |> query.build
}

// -- Tests --

pub fn bookstore_list_books_test() {
  let schema = create_schema()
  let result = executor.execute_query(schema, "{ books { id title } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "books query should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "books query should return data"
  }
}

pub fn bookstore_get_book_by_id_test() {
  let schema = create_schema()
  let result =
    executor.execute_query(schema, "{ book(id: \"2\") { id title } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "book(id) query should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "book(id) query should return data"
  }
}

pub fn bookstore_list_authors_test() {
  let schema = create_schema()
  let result = executor.execute_query(schema, "{ authors { id name } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "authors query should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "authors query should return data"
  }
}

pub fn bookstore_get_author_by_id_test() {
  let schema = create_schema()
  let result =
    executor.execute_query(schema, "{ author(id: \"1\") { id name } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "author(id) query should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "author(id) query should return data"
  }
}

pub fn bookstore_add_book_mutation_test() {
  let schema = create_schema()
  let result =
    executor.execute_query(
      schema,
      "mutation { addBook(title: \"1Q84\", authorId: \"1\") { id title } }",
    )

  case result.errors {
    [] -> Nil
    _ -> panic as "addBook mutation should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "addBook mutation should return data"
  }
}

pub fn bookstore_add_review_mutation_test() {
  let schema = create_schema()
  let result =
    executor.execute_query(
      schema,
      "mutation { addReview(bookId: \"1\", rating: 5, comment: \"Great!\") { id rating comment } }",
    )

  case result.errors {
    [] -> Nil
    _ -> panic as "addReview mutation should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "addReview mutation should return data"
  }
}

pub fn bookstore_cross_domain_query_test() {
  let schema = create_schema()

  // Query fields from different domain schemas in one request
  let result =
    executor.execute_query(schema, "{ books { id title } authors { id name } }")

  case result.errors {
    [] -> Nil
    _ -> panic as "Cross-domain query should succeed"
  }
  case result.data {
    Some(_) -> Nil
    None -> panic as "Cross-domain query should return data"
  }
}
