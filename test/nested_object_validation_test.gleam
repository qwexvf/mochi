// Tests for query validation with nested object types.
//
// Regression tests for a bug where the validation context's current_type
// leaked from a nested object selection set into sibling fields on the parent
// type, causing valid queries to be rejected with "Cannot query field X on
// type Y" errors.

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

// ── Domain types ─────────────────────────────────────────────────────────────

pub type Address {
  Address(street: String, city: String, zip: String)
}

pub type ContactInfo {
  ContactInfo(email: String, phone: String)
}

pub type User {
  User(
    id: String,
    name: String,
    address: Address,
    contact: ContactInfo,
    score: Int,
    active: Bool,
  )
}

pub type EquityByStreet {
  EquityByStreet(preflop: Float, flop: Float, turn: Float, river: Float)
}

pub type DetectedHand {
  DetectedHand(
    id: String,
    hole_cards: String,
    board: String,
    equity: EquityByStreet,
    gto_deviation: Float,
    accepted: Bool,
  )
}

pub type Comment {
  Comment(id: String, body: String, author: User)
}

// ── Schema helpers ────────────────────────────────────────────────────────────

fn address_type() {
  types.object("Address")
  |> types.string("street", fn(a: Address) { a.street })
  |> types.string("city", fn(a: Address) { a.city })
  |> types.string("zip", fn(a: Address) { a.zip })
  |> types.build(fn(_) { Ok(Address("", "", "")) })
}

fn contact_type() {
  types.object("ContactInfo")
  |> types.string("email", fn(c: ContactInfo) { c.email })
  |> types.string("phone", fn(c: ContactInfo) { c.phone })
  |> types.build(fn(_) { Ok(ContactInfo("", "")) })
}

fn user_type() {
  types.object("User")
  |> types.id("id", fn(u: User) { u.id })
  |> types.string("name", fn(u: User) { u.name })
  |> types.object_field("address", "Address", fn(u: User) {
    types.to_dynamic(u.address)
  })
  |> types.object_field("contact", "ContactInfo", fn(u: User) {
    types.to_dynamic(u.contact)
  })
  |> types.int("score", fn(u: User) { u.score })
  |> types.bool("active", fn(u: User) { u.active })
  |> types.build(fn(_) {
    Ok(User(
      "1",
      "Alice",
      Address("123 Main St", "Springfield", "12345"),
      ContactInfo("alice@example.com", "555-0100"),
      42,
      True,
    ))
  })
}

fn equity_type() {
  types.object("EquityByStreet")
  |> types.float("preflop", fn(e: EquityByStreet) { e.preflop })
  |> types.float("flop", fn(e: EquityByStreet) { e.flop })
  |> types.float("turn", fn(e: EquityByStreet) { e.turn })
  |> types.float("river", fn(e: EquityByStreet) { e.river })
  |> types.build(fn(_) { Ok(EquityByStreet(0.0, 0.0, 0.0, 0.0)) })
}

fn hand_type() {
  types.object("DetectedHand")
  |> types.id("id", fn(h: DetectedHand) { h.id })
  |> types.string("holeCards", fn(h: DetectedHand) { h.hole_cards })
  |> types.string("board", fn(h: DetectedHand) { h.board })
  |> types.object_field("equity", "EquityByStreet", fn(h: DetectedHand) {
    types.to_dynamic(h.equity)
  })
  |> types.float("gtoDeviation", fn(h: DetectedHand) { h.gto_deviation })
  |> types.bool("accepted", fn(h: DetectedHand) { h.accepted })
  |> types.build(fn(_) {
    Ok(DetectedHand(
      "1",
      "As Kh",
      "Qd Jc Ts",
      EquityByStreet(0.6, 0.55, 0.7, 0.9),
      0.05,
      False,
    ))
  })
}

fn comment_type() {
  types.object("Comment")
  |> types.id("id", fn(c: Comment) { c.id })
  |> types.string("body", fn(c: Comment) { c.body })
  |> types.object_field("author", "User", fn(c: Comment) {
    types.to_dynamic(c.author)
  })
  |> types.build(fn(_) {
    Ok(Comment(
      "1",
      "Nice post",
      User("2", "Bob", Address("", "", ""), ContactInfo("", ""), 0, True),
    ))
  })
}

fn user_schema() {
  let the_user =
    User(
      "1",
      "Alice",
      Address("123 Main St", "Springfield", "12345"),
      ContactInfo("alice@example.com", "555-0100"),
      42,
      True,
    )

  let user_query =
    query.query("user", schema.named_type("User"), fn(_ctx) { Ok(the_user) })

  query.new()
  |> query.add_query(user_query)
  |> query.add_type(user_type())
  |> query.add_type(address_type())
  |> query.add_type(contact_type())
  |> query.build
}

fn hand_schema() {
  let the_hand =
    DetectedHand(
      "hand-1",
      "As Kh",
      "Qd Jc Ts",
      EquityByStreet(0.62, 0.55, 0.71, 0.9),
      0.05,
      False,
    )

  let hand_query =
    query.query("hand", schema.named_type("DetectedHand"), fn(_ctx) {
      Ok(the_hand)
    })

  query.new()
  |> query.add_query(hand_query)
  |> query.add_type(hand_type())
  |> query.add_type(equity_type())
  |> query.build
}

fn comment_schema() {
  let the_comment =
    Comment(
      "c1",
      "Nice post",
      User(
        "2",
        "Bob",
        Address("456 Elm St", "Shelbyville", "67890"),
        ContactInfo("bob@example.com", "555-0200"),
        10,
        False,
      ),
    )

  let comment_query =
    query.query("comment", schema.named_type("Comment"), fn(_ctx) {
      Ok(the_comment)
    })

  query.new()
  |> query.add_query(comment_query)
  |> query.add_type(comment_type())
  |> query.add_type(user_type())
  |> query.add_type(address_type())
  |> query.add_type(contact_type())
  |> query.build
}

// ── Regression tests: sibling field after nested object ──────────────────────

// This was the original bug: `gtoDeviation` (a sibling of `equity`) was
// incorrectly validated against `EquityByStreet` instead of `DetectedHand`.
pub fn sibling_field_after_nested_object_is_valid_test() {
  let s = hand_schema()
  let q =
    "{ hand { id equity { preflop flop turn river } gtoDeviation accepted } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn sibling_field_before_nested_object_is_valid_test() {
  let s = hand_schema()
  let q = "{ hand { id gtoDeviation equity { preflop flop } accepted } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn multiple_sibling_fields_after_nested_object_are_valid_test() {
  let s = user_schema()
  let q = "{ user { id address { street city zip } score active name } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn two_nested_objects_then_sibling_is_valid_test() {
  let s = user_schema()
  let q =
    "{ user { id address { street city } contact { email phone } score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn nested_object_only_no_siblings_is_valid_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop flop turn river } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── Validation: invalid fields are still caught ───────────────────────────────

pub fn invalid_field_on_parent_type_is_rejected_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop } nonExistentField } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error for nonExistentField"
    _ -> Nil
  }
}

pub fn invalid_field_on_nested_type_is_rejected_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop bogus } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error for bogus field on EquityByStreet"
    _ -> Nil
  }
}

pub fn field_from_nested_type_on_parent_is_rejected_test() {
  let s = hand_schema()
  // preflop belongs to EquityByStreet, not DetectedHand
  let q = "{ hand { preflop } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error: preflop is not on DetectedHand"
    _ -> Nil
  }
}

pub fn field_from_parent_type_inside_nested_object_is_rejected_test() {
  let s = hand_schema()
  // gtoDeviation belongs to DetectedHand, not EquityByStreet
  let q = "{ hand { equity { preflop gtoDeviation } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] ->
      panic as "Expected validation error: gtoDeviation is not on EquityByStreet"
    _ -> Nil
  }
}

// ── Deeply nested objects ─────────────────────────────────────────────────────

pub fn deeply_nested_object_with_sibling_is_valid_test() {
  let s = comment_schema()
  let q =
    "{ comment { id body author { id name address { street city } score } } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn sibling_after_nested_at_depth_two_is_valid_test() {
  let s = comment_schema()
  // After author.address {...}, author.score should still validate against User
  let q =
    "{ comment { id author { name address { street } active score } body } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── Execution: correct data returned ─────────────────────────────────────────

pub fn equity_and_gto_deviation_both_resolve_test() {
  let s = hand_schema()
  let q = "{ hand { equity { preflop river } gtoDeviation accepted } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

pub fn user_siblings_after_nested_address_resolve_test() {
  let s = user_schema()
  let q = "{ user { address { city } name score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

// ── E-commerce: nested object + list of objects + scalar siblings ─────────────

pub type Category {
  Category(id: String, name: String, slug: String)
}

pub type Product {
  Product(
    id: String,
    name: String,
    category: Category,
    price: Float,
    in_stock: Bool,
    sku: String,
  )
}

pub type LineItem {
  LineItem(product: Product, quantity: Int, unit_price: Float)
}

pub type Order {
  Order(id: String, items: List(LineItem), total: Float, status: String)
}

fn category_type() {
  types.object("Category")
  |> types.id("id", fn(c: Category) { c.id })
  |> types.string("name", fn(c: Category) { c.name })
  |> types.string("slug", fn(c: Category) { c.slug })
  |> types.build(fn(_) { Ok(Category("", "", "")) })
}

fn product_type() {
  types.object("Product")
  |> types.id("id", fn(p: Product) { p.id })
  |> types.string("name", fn(p: Product) { p.name })
  |> types.object_field("category", "Category", fn(p: Product) {
    types.to_dynamic(p.category)
  })
  |> types.float("price", fn(p: Product) { p.price })
  |> types.bool("inStock", fn(p: Product) { p.in_stock })
  |> types.string("sku", fn(p: Product) { p.sku })
  |> types.build(fn(_) {
    Ok(Product("", "", Category("", "", ""), 0.0, False, ""))
  })
}

fn line_item_type() {
  types.object("LineItem")
  |> types.object_field("product", "Product", fn(li: LineItem) {
    types.to_dynamic(li.product)
  })
  |> types.int("quantity", fn(li: LineItem) { li.quantity })
  |> types.float("unitPrice", fn(li: LineItem) { li.unit_price })
  |> types.build(fn(_) {
    Ok(LineItem(Product("", "", Category("", "", ""), 0.0, False, ""), 0, 0.0))
  })
}

fn order_type() {
  types.object("Order")
  |> types.id("id", fn(o: Order) { o.id })
  |> types.list_object("items", "LineItem", fn(o: Order) {
    types.to_dynamic(
      list_to_dynamic(o.items, fn(li: LineItem) {
        dict.from_list([
          #(
            "product",
            dict.from_list([
              #("id", types.to_dynamic(li.product.id)),
              #("name", types.to_dynamic(li.product.name)),
              #("price", types.to_dynamic(li.product.price)),
              #("inStock", types.to_dynamic(li.product.in_stock)),
              #("sku", types.to_dynamic(li.product.sku)),
              #(
                "category",
                dict.from_list([
                  #("id", types.to_dynamic(li.product.category.id)),
                  #("name", types.to_dynamic(li.product.category.name)),
                  #("slug", types.to_dynamic(li.product.category.slug)),
                ])
                  |> types.to_dynamic,
              ),
            ])
              |> types.to_dynamic,
          ),
          #("quantity", types.to_dynamic(li.quantity)),
          #("unitPrice", types.to_dynamic(li.unit_price)),
        ])
        |> types.to_dynamic
      }),
    )
  })
  |> types.float("total", fn(o: Order) { o.total })
  |> types.string("status", fn(o: Order) { o.status })
  |> types.build(fn(_) { Ok(Order("", [], 0.0, "")) })
}

fn the_order() {
  let cat = Category("cat-1", "Electronics", "electronics")
  let prod =
    Product("prod-1", "Mechanical Keyboard", cat, 149.99, True, "KB-MX-001")
  Order("ord-1", [LineItem(prod, 2, 149.99)], 299.98, "PENDING")
}

fn order_schema() {
  let order_query =
    query.query("order", schema.named_type("Order"), fn(_ctx) {
      Ok(the_order())
    })

  query.new()
  |> query.add_query(order_query)
  |> query.add_type(order_type())
  |> query.add_type(line_item_type())
  |> query.add_type(product_type())
  |> query.add_type(category_type())
  |> query.build
}

fn product_schema() {
  let cat = Category("cat-1", "Electronics", "electronics")
  let prod =
    Product("prod-1", "Mechanical Keyboard", cat, 149.99, True, "KB-MX-001")

  let product_query =
    query.query("product", schema.named_type("Product"), fn(_ctx) { Ok(prod) })

  query.new()
  |> query.add_query(product_query)
  |> query.add_type(product_type())
  |> query.add_type(category_type())
  |> query.build
}

pub fn product_nested_category_then_scalar_siblings_test() {
  let s = product_schema()
  let q = "{ product { id name category { id name slug } price inStock sku } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

pub fn scalar_then_nested_category_then_more_scalars_test() {
  let s = product_schema()
  let q = "{ product { sku name category { slug name } price inStock id } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn order_list_object_then_scalar_siblings_validation_test() {
  let s = order_schema()
  let q =
    "{ order { id items { quantity unitPrice product { name price } } total status } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn order_scalars_before_and_after_list_object_test() {
  let s = order_schema()
  let q =
    "{ order { status items { quantity product { name category { name } } } total id } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn invalid_field_after_list_object_is_rejected_test() {
  let s = order_schema()
  let q = "{ order { items { quantity } bogusField } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected validation error for bogusField on Order"
    _ -> Nil
  }
}

pub fn field_from_list_item_type_on_parent_is_rejected_test() {
  let s = order_schema()
  let q = "{ order { quantity } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> panic as "Expected error: quantity is on LineItem, not Order"
    _ -> Nil
  }
}

// ── 3-level deep nesting: Organization > Department > Employee ────────────────

pub type Employee {
  Employee(id: String, name: String, role: String)
}

pub type Department {
  Department(
    id: String,
    name: String,
    employees: List(Employee),
    head_count: Int,
  )
}

pub type Organization {
  Organization(
    id: String,
    name: String,
    departments: List(Department),
    ceo: Employee,
    founded: Int,
  )
}

fn employee_type() {
  types.object("Employee")
  |> types.id("id", fn(e: Employee) { e.id })
  |> types.string("name", fn(e: Employee) { e.name })
  |> types.string("role", fn(e: Employee) { e.role })
  |> types.build(fn(_) { Ok(Employee("", "", "")) })
}

fn department_type() {
  types.object("Department")
  |> types.id("id", fn(d: Department) { d.id })
  |> types.string("name", fn(d: Department) { d.name })
  |> types.list_object("employees", "Employee", fn(d: Department) {
    types.to_dynamic(
      list_to_dynamic(d.employees, fn(e: Employee) {
        dict.from_list([
          #("id", types.to_dynamic(e.id)),
          #("name", types.to_dynamic(e.name)),
          #("role", types.to_dynamic(e.role)),
        ])
        |> types.to_dynamic
      }),
    )
  })
  |> types.int("headCount", fn(d: Department) { d.head_count })
  |> types.build(fn(_) { Ok(Department("", "", [], 0)) })
}

fn organization_type() {
  types.object("Organization")
  |> types.id("id", fn(o: Organization) { o.id })
  |> types.string("name", fn(o: Organization) { o.name })
  |> types.list_object("departments", "Department", fn(o: Organization) {
    types.to_dynamic(
      list_to_dynamic(o.departments, fn(d: Department) {
        dict.from_list([
          #("id", types.to_dynamic(d.id)),
          #("name", types.to_dynamic(d.name)),
          #("headCount", types.to_dynamic(d.head_count)),
          #(
            "employees",
            list_to_dynamic(d.employees, fn(e: Employee) {
              dict.from_list([
                #("id", types.to_dynamic(e.id)),
                #("name", types.to_dynamic(e.name)),
                #("role", types.to_dynamic(e.role)),
              ])
              |> types.to_dynamic
            })
              |> types.to_dynamic,
          ),
        ])
        |> types.to_dynamic
      }),
    )
  })
  |> types.object_field("ceo", "Employee", fn(o: Organization) {
    types.to_dynamic(o.ceo)
  })
  |> types.int("founded", fn(o: Organization) { o.founded })
  |> types.build(fn(_) { Ok(Organization("", "", [], Employee("", "", ""), 0)) })
}

fn org_schema() {
  let eng = Employee("e1", "Alice", "ENGINEER")
  let des = Employee("e2", "Bob", "DESIGNER")
  let ceo = Employee("e0", "Carol", "CEO")
  let dept = Department("d1", "Product", [eng, des], 2)
  let org = Organization("org-1", "Acme Corp", [dept], ceo, 2010)

  let org_query =
    query.query("org", schema.named_type("Organization"), fn(_ctx) { Ok(org) })

  query.new()
  |> query.add_query(org_query)
  |> query.add_type(organization_type())
  |> query.add_type(department_type())
  |> query.add_type(employee_type())
  |> query.build
}

pub fn three_level_deep_nesting_with_siblings_test() {
  let s = org_schema()
  let q =
    "{ org { id departments { name employees { id name role } headCount } ceo { name role } founded } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

pub fn siblings_after_nested_object_at_depth_two_org_test() {
  let s = org_schema()
  let q =
    "{ org { departments { employees { name } headCount id name } name founded } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn field_from_nested_level_on_org_root_is_rejected_test() {
  let s = org_schema()
  let q = "{ org { headCount } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] ->
      panic as "Expected error: headCount is on Department, not Organization"
    _ -> Nil
  }
}

// ── Aliases on nested and sibling fields ─────────────────────────────────────

pub fn alias_on_nested_object_and_sibling_test() {
  let s = user_schema()
  let q =
    "{ user { homeAddress: address { city zip } fullName: name score active } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn multiple_aliases_on_different_fields_test() {
  let s = user_schema()
  let q =
    "{ user { addr: address { street city } info: contact { email phone } points: score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── __typename in nested and parent contexts ──────────────────────────────────

pub fn typename_inside_nested_object_then_parent_sibling_test() {
  let s = user_schema()
  let q = "{ user { address { __typename city zip } name score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn typename_on_parent_and_in_nested_object_test() {
  let s = user_schema()
  let q = "{ user { __typename id address { __typename street } active } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── Named fragment containing both nested and scalar fields ───────────────────

pub fn named_fragment_with_nested_and_scalar_fields_test() {
  let s = user_schema()
  let q =
    "
    {
      user {
        ...UserProfile
      }
    }
    fragment UserProfile on User {
      id
      address { street city zip }
      name
      score
      active
    }
    "
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn named_fragment_on_nested_type_test() {
  let s = comment_schema()
  let q =
    "
    {
      comment {
        id
        author {
          ...AuthorDetails
          score
          active
        }
        body
      }
    }
    fragment AuthorDetails on User {
      id
      name
      address { street city }
    }
    "
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn named_fragment_sibling_after_spread_test() {
  let s = comment_schema()
  let q =
    "
    {
      comment {
        ...CommentCore
        body
      }
    }
    fragment CommentCore on Comment {
      id
      author { name address { city } score }
    }
    "
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── Inline fragment on a nested type ─────────────────────────────────────────

pub fn inline_fragment_on_nested_type_then_sibling_test() {
  let s = user_schema()
  let q =
    "{ user { address { ... on Address { city zip } street } name score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn inline_fragment_without_type_condition_in_nested_test() {
  let s = user_schema()
  let q = "{ user { address { ... { city } street } name score } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── Mutation returning nested type with scalar siblings ───────────────────────

pub fn mutation_returning_nested_type_with_siblings_test() {
  let prod =
    Product(
      "new-1",
      "Wireless Mouse",
      Category("cat-2", "Peripherals", "peripherals"),
      39.99,
      True,
      "MS-WL-002",
    )

  let create_product =
    query.mutation(
      name: "createProduct",
      args: [query.arg("name", schema.non_null(schema.string_type()))],
      returns: schema.named_type("Product"),
      resolve: fn(_args, _ctx) { Ok(prod) },
    )

  let s =
    query.new()
    |> query.add_mutation(create_product)
    |> query.add_type(product_type())
    |> query.add_type(category_type())
    |> query.build

  let q =
    "mutation { createProduct(name: \"Wireless Mouse\") { id name category { id name slug } price inStock sku } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
  case result.data {
    None -> panic as "Expected data"
    Some(_) -> Nil
  }
}

// ── Same nested type reused in multiple fields, with siblings between ─────────

pub fn same_nested_type_reused_with_siblings_between_test() {
  let s = user_schema()
  let q =
    "{ user { id address { street city } score contact { email phone } active name } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

pub fn interleaved_nested_and_scalar_fields_test() {
  let s = user_schema()
  let q = "{ user { id address { city } name contact { email } score active } }"
  let result = executor.execute_query(s, q)
  case result.errors {
    [] -> Nil
    errs ->
      panic as {
        "Expected no errors but got: "
        <> string_join(errs, fn(e) { execution_error_message(e) })
      }
  }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

fn execution_error_message(err: executor.ExecutionError) -> String {
  case err {
    executor.ResolverError(message: m, ..)
    | executor.ValidationError(message: m, ..)
    | executor.TypeError(message: m, ..)
    | executor.NullValueError(message: m, ..) -> m
    executor.RichResolverError(graphql_error: e, ..) -> e.message
  }
}

fn string_join(items: List(a), f: fn(a) -> String) -> String {
  case items {
    [] -> ""
    [x] -> f(x)
    [x, ..rest] -> f(x) <> ", " <> string_join(rest, f)
  }
}

fn list_to_dynamic(items: List(a), f: fn(a) -> Dynamic) -> List(Dynamic) {
  case items {
    [] -> []
    [x, ..rest] -> [f(x), ..list_to_dynamic(rest, f)]
  }
}
