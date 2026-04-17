# Guards

Guards are lightweight, composable precondition checks that run **before** a
resolver. If a guard returns `Ok(Nil)`, the resolver proceeds normally. If it
returns `Error(message)`, the resolver is skipped entirely and the error is
returned to the client.

Guards are the recommended way to handle authorization, feature flags, rate
limiting, and any other "should this resolver run at all?" logic.

## Why guards instead of middleware?

| | Guards | Middleware |
|---|---|---|
| **Runs** | Before resolver only | Before and after resolver |
| **Signature** | `fn(ctx) -> Result(Nil, String)` | `fn(Resolution, next) -> Resolution` |
| **Scope** | Attached per field/query/mutation | Global pipeline, optionally filtered |
| **Use case** | Access control, preconditions | Logging, caching, timing, transforms |
| **Complexity** | Minimal | Full chain with before/after hooks |

Use guards when you just need a yes/no decision before a resolver runs. Use
middleware when you need to inspect or transform the result after resolution, or
when you need cross-cutting concerns like logging across all fields.

## Quick start

```gleam
import mochi/query
import mochi/schema

// Define a reusable guard
fn require_auth(ctx: schema.ExecutionContext) -> Result(Nil, String) {
  case get_current_user(ctx) {
    Some(_) -> Ok(Nil)
    None -> Error("Authentication required")
  }
}

// Attach it to a query
let my_posts = query.query_with_args(
  name: "myPosts",
  args: [query.arg("limit", schema.int_type())],
  returns: schema.list_type(schema.named_type("Post")),
  decode: fn(args) { query.get_optional_int(args, "limit") },
  resolve: fn(limit, ctx) { get_my_posts(ctx, limit) },
  encode: fn(posts) { types.to_dynamic(posts) },
)
|> query.with_guard(require_auth)
```

## API reference

### High-level API (`mochi/query`)

These guards take `fn(ExecutionContext) -> Result(Nil, String)` — you get access
to the user context, which is the right level of abstraction for most
authorization checks.

```gleam
// On queries
query.with_guard(q, guard_fn)

// On mutations
query.mutation_with_guard(m, guard_fn)

// On subscriptions (guards the topic resolver)
query.subscription_with_guard(s, guard_fn)

// On field definitions
query.field_with_guard(f, guard_fn)
```

### Low-level API (`mochi/schema`)

These guards take `fn(ResolverInfo) -> Result(Nil, String)` — you get access to
the full resolver info including the parent value, arguments, and execution
context. Use this when your guard logic depends on the specific arguments or
parent data.

```gleam
// Single guard
schema.guard(field_def, guard_fn)

// Multiple guards (checked in list order)
schema.guards(field_def, [guard_a, guard_b])
```

## Stacking multiple guards

Guards compose by wrapping the resolver. You can stack multiple guards, and all
must pass for the resolver to execute.

### Using `schema.guards` (list order)

With `schema.guards`, guards are checked in the order they appear in the list:

```gleam
schema.field_def("adminData", schema.string_type())
  |> schema.resolver(admin_resolver)
  |> schema.guards([require_auth, require_admin])
  // require_auth is checked first, then require_admin
```

### Using pipe chains (wrapping order)

When stacking with pipes, each `with_guard` wraps the previous resolver+guards.
The **last** guard added becomes the outermost check and runs **first**:

```gleam
query.query_with_args(name: "adminUsers", ...)
  |> query.with_guard(require_admin)  // checked second (inner)
  |> query.with_guard(require_auth)   // checked first (outer)
```

This is standard functional composition (like nested function calls), but if
you find it confusing, prefer `schema.guards([...])` where list order matches
execution order.

## Common guard patterns

### Authentication

```gleam
fn require_auth(ctx: schema.ExecutionContext) -> Result(Nil, String) {
  case get_current_user(ctx) {
    Some(_) -> Ok(Nil)
    None -> Error("Authentication required")
  }
}
```

### Role-based access

```gleam
fn require_role(role: Role) -> fn(schema.ExecutionContext) -> Result(Nil, String) {
  fn(ctx) {
    case get_current_user(ctx) {
      Some(user) if user.role == role -> Ok(Nil)
      Some(_) -> Error("Forbidden: requires " <> role_to_string(role) <> " role")
      None -> Error("Authentication required")
    }
  }
}

// Usage:
|> query.with_guard(require_role(Admin))
```

### Feature flags

```gleam
fn require_feature(flag: String) -> fn(schema.ExecutionContext) -> Result(Nil, String) {
  fn(ctx) {
    case is_feature_enabled(ctx, flag) {
      True -> Ok(Nil)
      False -> Error("Feature not available")
    }
  }
}

|> query.with_guard(require_feature("beta_search"))
```

### Resource ownership (low-level guard with args access)

```gleam
fn require_owner() -> schema.Guard {
  fn(info: schema.ResolverInfo) {
    let current_user = get_current_user(info.context)
    let requested_id = query.get_id(info.arguments, "userId")

    case current_user, requested_id {
      Some(user), Ok(uid) if uid == user.id -> Ok(Nil)
      Some(_), Ok(_) -> Error("Forbidden: you can only access your own data")
      None, _ -> Error("Authentication required")
      _, Error(_) -> Error("Missing userId argument")
    }
  }
}

// Low-level API since we need argument access
schema.field_def("privateData", schema.string_type())
  |> schema.resolver(private_data_resolver)
  |> schema.guard(require_owner())
```

## Guard combinators

Combinators let you compose guards with boolean logic.

### `all_of` — AND logic (all must pass)

```gleam
// High-level: checked in list order, fails on first error
let admin_guard = query.all_of([require_auth, require_admin])

|> query.with_guard(admin_guard)
```

### `any_of` — OR logic (at least one must pass)

```gleam
// Succeeds if any guard passes; fails with the last error if all fail
let can_access = query.any_of([require_admin, require_owner])

|> query.with_guard(can_access)
```

### Low-level combinators

For guards that take `ResolverInfo`:

```gleam
// AND: all must pass
schema.all_guards([guard_a, guard_b])

// OR: at least one must pass
schema.any_guard([guard_a, guard_b])
```

These return a single `Guard` that can be passed to `schema.guard()`.

## Best practices

1. **Keep guards pure and focused.** A guard should answer one question: "is
   this operation allowed?" Don't put business logic, data fetching, or side
   effects in guards.

2. **Return clear error messages.** The error string is returned directly to
   the GraphQL client. Make it actionable: "Authentication required" is better
   than "Forbidden".

3. **Use the high-level API by default.** `query.with_guard` takes just
   `ExecutionContext`, which is the right abstraction for most auth checks.
   Only drop to `schema.guard` (which takes `ResolverInfo`) when you need
   access to arguments or parent data.

4. **Prefer `schema.guards([...])` for multiple guards.** The list form makes
   execution order explicit and readable. Pipe chaining works but the
   outermost-first ordering can be confusing.

5. **Build guard factories for reuse.** Instead of writing one-off closures,
   create functions that return guards:

   ```gleam
   fn require_role(role: Role) {
     fn(ctx) { ... }
   }

   |> query.with_guard(require_role(Admin))
   |> query.with_guard(require_role(Manager))
   ```

6. **Don't duplicate guard logic in resolvers.** If you attach a guard, trust
   it. Don't re-check the same condition inside the resolver.

7. **Use guards for authorization, middleware for observation.** Guards are for
   access control (yes/no decisions). Middleware is for cross-cutting concerns
   like logging, timing, caching, and response transformation.
