// Tests for GraphQL subscription support

import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/option.{None, Some}
import mochi/query
import mochi/schema
import mochi/subscription
import mochi/subscription_executor
import mochi/types

// ============================================================================
// Test Types
// ============================================================================

pub type User {
  User(id: String, name: String, email: String)
}

pub type Message {
  Message(id: String, content: String, from_user: String)
}

fn user_to_dynamic(user: User) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(user.id)),
      #("name", types.to_dynamic(user.name)),
      #("email", types.to_dynamic(user.email)),
    ]),
  )
}

fn message_to_dynamic(msg: Message) -> Dynamic {
  types.to_dynamic(
    dict.from_list([
      #("id", types.to_dynamic(msg.id)),
      #("content", types.to_dynamic(msg.content)),
      #("fromUser", types.to_dynamic(msg.from_user)),
    ]),
  )
}

// ============================================================================
// PubSub Tests
// ============================================================================

pub fn pubsub_create_test() {
  let pubsub = subscription.new_pubsub()
  case subscription.subscription_count(pubsub) == 0 {
    True -> Nil
    False -> panic as "New pubsub should have 0 subscriptions"
  }
}

pub fn pubsub_subscribe_test() {
  let pubsub = subscription.new_pubsub()

  let result =
    subscription.subscribe(pubsub, "test:topic", "testField", dict.new(), fn(_) {
      Nil
    })

  case subscription.subscription_count(result.pubsub) == 1 {
    True -> Nil
    False -> panic as "Should have 1 subscription after subscribing"
  }
}

pub fn pubsub_unsubscribe_test() {
  let pubsub = subscription.new_pubsub()

  let result =
    subscription.subscribe(pubsub, "test:topic", "testField", dict.new(), fn(_) {
      Nil
    })

  let pubsub2 = subscription.unsubscribe(result.pubsub, result.subscription_id)

  case subscription.subscription_count(pubsub2) == 0 {
    True -> Nil
    False -> panic as "Should have 0 subscriptions after unsubscribing"
  }
}

pub fn pubsub_multiple_subscribers_test() {
  let pubsub = subscription.new_pubsub()

  let result1 =
    subscription.subscribe(
      pubsub,
      "user:created",
      "onUserCreated",
      dict.new(),
      fn(_) { Nil },
    )

  let result2 =
    subscription.subscribe(
      result1.pubsub,
      "user:created",
      "onUserCreated",
      dict.new(),
      fn(_) { Nil },
    )

  let result3 =
    subscription.subscribe(
      result2.pubsub,
      "message:sent",
      "onMessageSent",
      dict.new(),
      fn(_) { Nil },
    )

  case subscription.subscription_count(result3.pubsub) == 3 {
    True -> Nil
    False -> panic as "Should have 3 subscriptions"
  }

  // Check topic subscribers
  let user_subs =
    subscription.get_topic_subscribers(result3.pubsub, "user:created")
  case user_subs {
    [_, _] -> Nil
    _ -> panic as "Should have 2 subscribers to user:created"
  }

  let msg_subs =
    subscription.get_topic_subscribers(result3.pubsub, "message:sent")
  case msg_subs {
    [_] -> Nil
    _ -> panic as "Should have 1 subscriber to message:sent"
  }
}

pub fn pubsub_publish_test() {
  // Track received events
  let received = dict.new()

  let pubsub = subscription.new_pubsub()

  // We can't easily test publish with side effects in pure Gleam
  // but we can verify the mechanics work
  let result =
    subscription.subscribe(pubsub, "test:topic", "testField", dict.new(), fn(
      _data,
    ) { Nil })

  // Publish should not error
  subscription.publish(
    result.pubsub,
    "test:topic",
    types.to_dynamic("test event"),
  )

  // Publishing to non-existent topic should not error
  subscription.publish(
    result.pubsub,
    "nonexistent:topic",
    types.to_dynamic("ignored"),
  )

  Nil
}

pub fn pubsub_get_subscription_test() {
  let pubsub = subscription.new_pubsub()

  let result =
    subscription.subscribe(pubsub, "test:topic", "testField", dict.new(), fn(_) {
      Nil
    })

  case subscription.get_subscription(result.pubsub, result.subscription_id) {
    Some(sub) -> {
      case sub.topic == "test:topic" && sub.field_name == "testField" {
        True -> Nil
        False -> panic as "Subscription should have correct topic and field"
      }
    }
    None -> panic as "Should find subscription by ID"
  }

  case subscription.get_subscription(result.pubsub, "nonexistent") {
    None -> Nil
    Some(_) -> panic as "Should not find nonexistent subscription"
  }
}

// ============================================================================
// Topic Helper Tests
// ============================================================================

pub fn topic_helpers_test() {
  let simple = subscription.topic("user:created")
  case simple == "user:created" {
    True -> Nil
    False -> panic as "Simple topic should match"
  }

  let with_id = subscription.topic_with_id("user", "123")
  case with_id == "user:123" {
    True -> Nil
    False -> panic as "Topic with ID should be 'user:123'"
  }

  let from_parts = subscription.topic_from_parts(["chat", "room", "456"])
  case from_parts == "chat:room:456" {
    True -> Nil
    False -> panic as "Topic from parts should be 'chat:room:456'"
  }
}

// ============================================================================
// Subscription Definition Tests
// ============================================================================

pub fn subscription_definition_test() {
  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )

  case on_user_created.name == "onUserCreated" {
    True -> Nil
    False -> panic as "Subscription name should be 'onUserCreated'"
  }
}

pub fn subscription_with_description_test() {
  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )
    |> query.subscription_description("Triggered when a new user is created")

  case on_user_created.description {
    Some("Triggered when a new user is created") -> Nil
    _ -> panic as "Description should be set"
  }
}

pub fn subscription_with_args_test() {
  let on_message =
    query.subscription_with_args(
      "onMessageSent",
      [query.arg("roomId", schema.non_null(schema.id_type()))],
      schema.named_type("Message"),
      fn(args) {
        case dict.get(args, "roomId") {
          Ok(_) -> Ok("room_id_here")
          Error(_) -> Error("Missing roomId")
        }
      },
      fn(room_id, _ctx) { Ok("messages:" <> room_id) },
      message_to_dynamic,
    )

  case on_message.name == "onMessageSent" {
    True -> Nil
    False -> panic as "Subscription name should be 'onMessageSent'"
  }
}

// ============================================================================
// Schema Builder Tests
// ============================================================================

pub fn schema_with_subscription_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(fn(_) { Ok(User("1", "Test", "test@example.com")) })

  let users_query =
    query.query(
      "users",
      schema.list_type(schema.named_type("User")),
      fn(_ctx) { Ok([]) },
      fn(_) { types.to_dynamic([]) },
    )

  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )

  let test_schema =
    query.new()
    |> query.add_query(users_query)
    |> query.add_subscription(on_user_created)
    |> query.add_type(user_type)
    |> query.build

  // Schema should have subscription type
  case test_schema.subscription {
    Some(sub_type) -> {
      case sub_type.name == "Subscription" {
        True -> Nil
        False -> panic as "Subscription type should be named 'Subscription'"
      }
      case dict.has_key(sub_type.fields, "onUserCreated") {
        True -> Nil
        False -> panic as "Should have onUserCreated field"
      }
    }
    None -> panic as "Schema should have subscription type"
  }
}

pub fn schema_multiple_subscriptions_test() {
  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )

  let on_user_updated =
    query.subscription(
      "onUserUpdated",
      schema.named_type("User"),
      "user:updated",
      user_to_dynamic,
    )

  let on_message =
    query.subscription(
      "onMessageSent",
      schema.named_type("Message"),
      "message:sent",
      message_to_dynamic,
    )

  let dummy_query =
    query.query(
      "dummy",
      schema.string_type(),
      fn(_) { Ok("dummy") },
      types.to_dynamic,
    )

  let test_schema =
    query.new()
    |> query.add_query(dummy_query)
    |> query.add_subscription(on_user_created)
    |> query.add_subscription(on_user_updated)
    |> query.add_subscription(on_message)
    |> query.build

  case test_schema.subscription {
    Some(sub_type) -> {
      case dict.size(sub_type.fields) == 3 {
        True -> Nil
        False -> panic as "Should have 3 subscription fields"
      }
    }
    None -> panic as "Schema should have subscription type"
  }
}

// ============================================================================
// Subscription Executor Tests
// ============================================================================

pub fn subscription_executor_basic_test() {
  let user_type =
    types.object("User")
    |> types.id("id", fn(u: User) { u.id })
    |> types.string("name", fn(u: User) { u.name })
    |> types.build(fn(_) { Ok(User("1", "Test", "test@example.com")) })

  let dummy_query =
    query.query(
      "dummy",
      schema.string_type(),
      fn(_) { Ok("dummy") },
      types.to_dynamic,
    )

  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )

  let test_schema =
    query.new()
    |> query.add_query(dummy_query)
    |> query.add_subscription(on_user_created)
    |> query.add_type(user_type)
    |> query.build

  let pubsub = subscription.new_pubsub()
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))

  let sub_ctx =
    subscription_executor.SubscriptionContext(
      schema: test_schema,
      pubsub: pubsub,
      execution_context: ctx,
      variable_values: dict.new(),
    )

  let subscription_query =
    "subscription { onUserCreated { id name } }"

  let result =
    subscription_executor.subscribe(sub_ctx, subscription_query, fn(_result) {
      Nil
    })

  case result {
    subscription_executor.SubscriptionResult(subscription_id, topic, new_pubsub) -> {
      case subscription.subscription_count(new_pubsub) == 1 {
        True -> Nil
        False -> panic as "Should have 1 active subscription"
      }
      case topic == "onUserCreated" {
        True -> Nil
        False -> panic as "Topic should be 'onUserCreated'"
      }
    }
    subscription_executor.SubscriptionError(msg) ->
      panic as { "Subscription should succeed: " <> msg }
  }
}

pub fn subscription_executor_no_subscription_type_test() {
  // Schema without subscription type
  let dummy_query =
    query.query(
      "dummy",
      schema.string_type(),
      fn(_) { Ok("dummy") },
      types.to_dynamic,
    )

  let test_schema =
    query.new()
    |> query.add_query(dummy_query)
    |> query.build

  let pubsub = subscription.new_pubsub()
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))

  let sub_ctx =
    subscription_executor.SubscriptionContext(
      schema: test_schema,
      pubsub: pubsub,
      execution_context: ctx,
      variable_values: dict.new(),
    )

  let result =
    subscription_executor.subscribe(
      sub_ctx,
      "subscription { onUserCreated { id } }",
      fn(_) { Nil },
    )

  case result {
    subscription_executor.SubscriptionError(_) -> Nil
    subscription_executor.SubscriptionResult(_, _, _) ->
      panic as "Should fail when schema has no subscription type"
  }
}

pub fn subscription_executor_invalid_field_test() {
  let dummy_query =
    query.query(
      "dummy",
      schema.string_type(),
      fn(_) { Ok("dummy") },
      types.to_dynamic,
    )

  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )

  let test_schema =
    query.new()
    |> query.add_query(dummy_query)
    |> query.add_subscription(on_user_created)
    |> query.build

  let pubsub = subscription.new_pubsub()
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))

  let sub_ctx =
    subscription_executor.SubscriptionContext(
      schema: test_schema,
      pubsub: pubsub,
      execution_context: ctx,
      variable_values: dict.new(),
    )

  let result =
    subscription_executor.subscribe(
      sub_ctx,
      "subscription { nonExistentField { id } }",
      fn(_) { Nil },
    )

  case result {
    subscription_executor.SubscriptionError(_) -> Nil
    subscription_executor.SubscriptionResult(_, _, _) ->
      panic as "Should fail for non-existent subscription field"
  }
}

pub fn subscription_unsubscribe_test() {
  let dummy_query =
    query.query(
      "dummy",
      schema.string_type(),
      fn(_) { Ok("dummy") },
      types.to_dynamic,
    )

  let on_user_created =
    query.subscription(
      "onUserCreated",
      schema.named_type("User"),
      "user:created",
      user_to_dynamic,
    )

  let test_schema =
    query.new()
    |> query.add_query(dummy_query)
    |> query.add_subscription(on_user_created)
    |> query.build

  let pubsub = subscription.new_pubsub()
  let ctx = schema.execution_context(types.to_dynamic(dict.new()))

  let sub_ctx =
    subscription_executor.SubscriptionContext(
      schema: test_schema,
      pubsub: pubsub,
      execution_context: ctx,
      variable_values: dict.new(),
    )

  let result =
    subscription_executor.subscribe(
      sub_ctx,
      "subscription { onUserCreated { id } }",
      fn(_) { Nil },
    )

  case result {
    subscription_executor.SubscriptionResult(sub_id, _, new_pubsub) -> {
      let after_unsub = subscription_executor.unsubscribe(new_pubsub, sub_id)
      case subscription.subscription_count(after_unsub) == 0 {
        True -> Nil
        False -> panic as "Should have 0 subscriptions after unsubscribe"
      }
    }
    subscription_executor.SubscriptionError(msg) ->
      panic as { "Subscription should succeed: " <> msg }
  }
}
