// Test that the bookstore split-schema pattern works end-to-end
// Mirrors examples/bookstore_split_schema.gleam

import gleam/list
import gleam/option.{None, Some}
import gleam/result
import mochi/error
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
    query.query("books", schema.list_type(schema.named_type("Book")), fn(_ctx) {
      Ok(all_books())
    })

  let book_query =
    query.query_with_args(
      name: "book",
      args: [query.arg("id", schema.non_null(schema.id_type()))],
      returns: schema.named_type("Book"),
      resolve: fn(args, _ctx) {
        use id <- result.try(query.get_id(args, "id"))
        case list.find(all_books(), fn(b) { b.id == id }) {
          Ok(b) -> Ok(b)
          Error(_) -> Error(error.new("Not found"))
        }
      },
    )

  let add_book =
    query.mutation(
      name: "addBook",
      args: [
        query.arg("title", schema.non_null(schema.string_type())),
        query.arg("authorId", schema.non_null(schema.id_type())),
      ],
      returns: schema.non_null(schema.named_type("Book")),
      resolve: fn(args, _ctx) {
        use title <- result.try(query.get_string(args, "title"))
        use author_id <- result.try(query.get_id(args, "authorId"))
        Ok(Book("new-1", title, author_id))
      },
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
    )

  let author_query =
    query.query_with_args(
      name: "author",
      args: [query.arg("id", schema.non_null(schema.id_type()))],
      returns: schema.named_type("Author"),
      resolve: fn(args, _ctx) {
        use id <- result.try(query.get_id(args, "id"))
        case list.find(all_authors(), fn(a) { a.id == id }) {
          Ok(a) -> Ok(a)
          Error(_) -> Error(error.new("Not found"))
        }
      },
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
      resolve: fn(args, _ctx) {
        use _bid <- result.try(query.get_id(args, "bookId"))
        use rating <- result.try(query.get_int(args, "rating"))
        let comment =
          query.get_optional_string(args, "comment") |> option.unwrap("")
        Ok(Review("new-1", "1", rating, comment))
      },
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
