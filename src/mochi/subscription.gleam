// mochi/subscription.gleam
// GraphQL Subscription support with pub/sub pattern
//
// Usage:
//   // Define a subscription
//   let on_user_created = subscription.subscription(
//     "onUserCreated",
//     schema.named_type("User"),
//     fn(info) { Ok(subscription.topic("user:created")) },
//     fn(event) { types.to_dynamic(event) },
//   )
//
//   // Publish events
//   subscription.publish(pubsub, "user:created", user_data)

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option.{type Option, None, Some}
import mochi/schema

// ============================================================================
// Types
// ============================================================================

/// Unique identifier for a subscription instance
pub type SubscriptionId =
  String

/// A topic that subscriptions can listen to
pub type Topic =
  String

/// Subscription definition for the schema
pub type SubscriptionDefinition(event) {
  SubscriptionDefinition(
    name: String,
    description: Option(String),
    field_type: schema.FieldType,
    arguments: Dict(String, schema.ArgumentDefinition),
    /// Returns the topic(s) to subscribe to based on arguments
    topic_resolver: fn(schema.ResolverInfo) -> Result(Topic, String),
    /// Transforms the raw event data into the GraphQL response type
    event_transformer: fn(event) -> Dynamic,
    /// Optional filter function to determine if event should be sent to this subscriber
    filter: Option(fn(event, Dict(String, Dynamic)) -> Bool),
  )
}

/// Active subscription instance
pub type Subscription {
  Subscription(
    id: SubscriptionId,
    topic: Topic,
    field_name: String,
    arguments: Dict(String, Dynamic),
    /// Callback to execute when event is received
    callback: fn(Dynamic) -> Nil,
  )
}

/// PubSub state for managing subscriptions
pub opaque type PubSub {
  PubSub(
    subscriptions: Dict(SubscriptionId, Subscription),
    topics: Dict(Topic, List(SubscriptionId)),
    next_id: Int,
  )
}

/// Result of subscribing
pub type SubscribeResult {
  SubscribeResult(subscription_id: SubscriptionId, pubsub: PubSub)
}

/// Subscription event with metadata
pub type SubscriptionEvent {
  SubscriptionEvent(topic: Topic, payload: Dynamic)
}

// ============================================================================
// PubSub Management
// ============================================================================

/// Create a new PubSub instance
pub fn new_pubsub() -> PubSub {
  PubSub(subscriptions: dict.new(), topics: dict.new(), next_id: 1)
}

/// Subscribe to a topic
pub fn subscribe(
  pubsub: PubSub,
  topic: Topic,
  field_name: String,
  arguments: Dict(String, Dynamic),
  callback: fn(Dynamic) -> Nil,
) -> SubscribeResult {
  let id = "sub_" <> int_to_string(pubsub.next_id)

  let subscription =
    Subscription(
      id: id,
      topic: topic,
      field_name: field_name,
      arguments: arguments,
      callback: callback,
    )

  let subscriptions = dict.insert(pubsub.subscriptions, id, subscription)

  let topic_subs = case dict.get(pubsub.topics, topic) {
    Ok(existing) -> [id, ..existing]
    Error(_) -> [id]
  }
  let topics = dict.insert(pubsub.topics, topic, topic_subs)

  let new_pubsub =
    PubSub(
      subscriptions: subscriptions,
      topics: topics,
      next_id: pubsub.next_id + 1,
    )

  SubscribeResult(subscription_id: id, pubsub: new_pubsub)
}

/// Unsubscribe from a topic
pub fn unsubscribe(pubsub: PubSub, subscription_id: SubscriptionId) -> PubSub {
  case dict.get(pubsub.subscriptions, subscription_id) {
    Ok(subscription) -> {
      let subscriptions = dict.delete(pubsub.subscriptions, subscription_id)

      let topics = case dict.get(pubsub.topics, subscription.topic) {
        Ok(sub_ids) -> {
          let filtered = list.filter(sub_ids, fn(id) { id != subscription_id })
          case filtered {
            [] -> dict.delete(pubsub.topics, subscription.topic)
            _ -> dict.insert(pubsub.topics, subscription.topic, filtered)
          }
        }
        Error(_) -> pubsub.topics
      }

      PubSub(..pubsub, subscriptions: subscriptions, topics: topics)
    }
    Error(_) -> pubsub
  }
}

/// Publish an event to all subscribers of a topic
pub fn publish(pubsub: PubSub, topic: Topic, payload: Dynamic) -> Nil {
  case dict.get(pubsub.topics, topic) {
    Ok(sub_ids) -> {
      list.each(sub_ids, fn(sub_id) {
        case dict.get(pubsub.subscriptions, sub_id) {
          Ok(subscription) -> subscription.callback(payload)
          Error(_) -> Nil
        }
      })
    }
    Error(_) -> Nil
  }
}

/// Get all active subscription IDs for a topic
pub fn get_topic_subscribers(
  pubsub: PubSub,
  topic: Topic,
) -> List(SubscriptionId) {
  case dict.get(pubsub.topics, topic) {
    Ok(sub_ids) -> sub_ids
    Error(_) -> []
  }
}

/// Get subscription by ID
pub fn get_subscription(
  pubsub: PubSub,
  subscription_id: SubscriptionId,
) -> Option(Subscription) {
  case dict.get(pubsub.subscriptions, subscription_id) {
    Ok(sub) -> Some(sub)
    Error(_) -> None
  }
}

/// Count active subscriptions
pub fn subscription_count(pubsub: PubSub) -> Int {
  dict.size(pubsub.subscriptions)
}

// ============================================================================
// Subscription Definition Builder
// ============================================================================

/// Create a new subscription definition
pub fn subscription(
  name: String,
  field_type: schema.FieldType,
  topic_resolver: fn(schema.ResolverInfo) -> Result(Topic, String),
  event_transformer: fn(event) -> Dynamic,
) -> SubscriptionDefinition(event) {
  SubscriptionDefinition(
    name: name,
    description: None,
    field_type: field_type,
    arguments: dict.new(),
    topic_resolver: topic_resolver,
    event_transformer: event_transformer,
    filter: None,
  )
}

/// Add description to subscription
pub fn description(
  sub: SubscriptionDefinition(event),
  desc: String,
) -> SubscriptionDefinition(event) {
  SubscriptionDefinition(..sub, description: Some(desc))
}

/// Add argument to subscription
pub fn argument(
  sub: SubscriptionDefinition(event),
  arg: schema.ArgumentDefinition,
) -> SubscriptionDefinition(event) {
  SubscriptionDefinition(
    ..sub,
    arguments: dict.insert(sub.arguments, arg.name, arg),
  )
}

/// Add filter function to subscription
pub fn filter(
  sub: SubscriptionDefinition(event),
  filter_fn: fn(event, Dict(String, Dynamic)) -> Bool,
) -> SubscriptionDefinition(event) {
  SubscriptionDefinition(..sub, filter: Some(filter_fn))
}

/// Convert subscription definition to a field definition for schema
pub fn to_field_definition(
  sub: SubscriptionDefinition(event),
) -> schema.FieldDefinition {
  schema.FieldDefinition(
    name: sub.name,
    description: sub.description,
    field_type: sub.field_type,
    arguments: sub.arguments,
    resolver: None,
    is_deprecated: False,
    deprecation_reason: None,
  )
}

// ============================================================================
// Topic Helpers
// ============================================================================

/// Create a simple topic string
pub fn topic(name: String) -> Topic {
  name
}

/// Create a topic with an ID suffix (e.g., "user:123:updated")
pub fn topic_with_id(base: String, id: String) -> Topic {
  base <> ":" <> id
}

/// Create a topic from multiple parts
pub fn topic_from_parts(parts: List(String)) -> Topic {
  join_with_colon(parts)
}

// ============================================================================
// Helpers
// ============================================================================

fn int_to_string(n: Int) -> String {
  case n < 0 {
    True -> "-" <> positive_int_to_string(-n)
    False -> positive_int_to_string(n)
  }
}

fn positive_int_to_string(n: Int) -> String {
  case n < 10 {
    True -> digit_to_string(n)
    False -> positive_int_to_string(n / 10) <> digit_to_string(n % 10)
  }
}

fn digit_to_string(d: Int) -> String {
  case d {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    _ -> "9"
  }
}

fn join_with_colon(parts: List(String)) -> String {
  case parts {
    [] -> ""
    [first] -> first
    [first, ..rest] -> first <> ":" <> join_with_colon(rest)
  }
}
