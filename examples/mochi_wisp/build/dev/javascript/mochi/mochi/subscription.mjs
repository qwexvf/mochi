import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import {
  Ok,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../gleam.mjs";
import * as $schema from "../mochi/schema.mjs";

export class SubscriptionDefinition extends $CustomType {
  constructor(name, description, field_type, arguments$, topic_resolver, event_transformer, filter) {
    super();
    this.name = name;
    this.description = description;
    this.field_type = field_type;
    this.arguments = arguments$;
    this.topic_resolver = topic_resolver;
    this.event_transformer = event_transformer;
    this.filter = filter;
  }
}
export const SubscriptionDefinition$SubscriptionDefinition = (name, description, field_type, arguments$, topic_resolver, event_transformer, filter) =>
  new SubscriptionDefinition(name,
  description,
  field_type,
  arguments$,
  topic_resolver,
  event_transformer,
  filter);
export const SubscriptionDefinition$isSubscriptionDefinition = (value) =>
  value instanceof SubscriptionDefinition;
export const SubscriptionDefinition$SubscriptionDefinition$name = (value) =>
  value.name;
export const SubscriptionDefinition$SubscriptionDefinition$0 = (value) =>
  value.name;
export const SubscriptionDefinition$SubscriptionDefinition$description = (value) =>
  value.description;
export const SubscriptionDefinition$SubscriptionDefinition$1 = (value) =>
  value.description;
export const SubscriptionDefinition$SubscriptionDefinition$field_type = (value) =>
  value.field_type;
export const SubscriptionDefinition$SubscriptionDefinition$2 = (value) =>
  value.field_type;
export const SubscriptionDefinition$SubscriptionDefinition$arguments = (value) =>
  value.arguments;
export const SubscriptionDefinition$SubscriptionDefinition$3 = (value) =>
  value.arguments;
export const SubscriptionDefinition$SubscriptionDefinition$topic_resolver = (value) =>
  value.topic_resolver;
export const SubscriptionDefinition$SubscriptionDefinition$4 = (value) =>
  value.topic_resolver;
export const SubscriptionDefinition$SubscriptionDefinition$event_transformer = (value) =>
  value.event_transformer;
export const SubscriptionDefinition$SubscriptionDefinition$5 = (value) =>
  value.event_transformer;
export const SubscriptionDefinition$SubscriptionDefinition$filter = (value) =>
  value.filter;
export const SubscriptionDefinition$SubscriptionDefinition$6 = (value) =>
  value.filter;

export class Subscription extends $CustomType {
  constructor(id, topic, field_name, arguments$, callback) {
    super();
    this.id = id;
    this.topic = topic;
    this.field_name = field_name;
    this.arguments = arguments$;
    this.callback = callback;
  }
}
export const Subscription$Subscription = (id, topic, field_name, arguments$, callback) =>
  new Subscription(id, topic, field_name, arguments$, callback);
export const Subscription$isSubscription = (value) =>
  value instanceof Subscription;
export const Subscription$Subscription$id = (value) => value.id;
export const Subscription$Subscription$0 = (value) => value.id;
export const Subscription$Subscription$topic = (value) => value.topic;
export const Subscription$Subscription$1 = (value) => value.topic;
export const Subscription$Subscription$field_name = (value) => value.field_name;
export const Subscription$Subscription$2 = (value) => value.field_name;
export const Subscription$Subscription$arguments = (value) => value.arguments;
export const Subscription$Subscription$3 = (value) => value.arguments;
export const Subscription$Subscription$callback = (value) => value.callback;
export const Subscription$Subscription$4 = (value) => value.callback;

class PubSub extends $CustomType {
  constructor(subscriptions, topics, next_id) {
    super();
    this.subscriptions = subscriptions;
    this.topics = topics;
    this.next_id = next_id;
  }
}

export class SubscribeResult extends $CustomType {
  constructor(subscription_id, pubsub) {
    super();
    this.subscription_id = subscription_id;
    this.pubsub = pubsub;
  }
}
export const SubscribeResult$SubscribeResult = (subscription_id, pubsub) =>
  new SubscribeResult(subscription_id, pubsub);
export const SubscribeResult$isSubscribeResult = (value) =>
  value instanceof SubscribeResult;
export const SubscribeResult$SubscribeResult$subscription_id = (value) =>
  value.subscription_id;
export const SubscribeResult$SubscribeResult$0 = (value) =>
  value.subscription_id;
export const SubscribeResult$SubscribeResult$pubsub = (value) => value.pubsub;
export const SubscribeResult$SubscribeResult$1 = (value) => value.pubsub;

export class SubscriptionEvent extends $CustomType {
  constructor(topic, payload) {
    super();
    this.topic = topic;
    this.payload = payload;
  }
}
export const SubscriptionEvent$SubscriptionEvent = (topic, payload) =>
  new SubscriptionEvent(topic, payload);
export const SubscriptionEvent$isSubscriptionEvent = (value) =>
  value instanceof SubscriptionEvent;
export const SubscriptionEvent$SubscriptionEvent$topic = (value) => value.topic;
export const SubscriptionEvent$SubscriptionEvent$0 = (value) => value.topic;
export const SubscriptionEvent$SubscriptionEvent$payload = (value) =>
  value.payload;
export const SubscriptionEvent$SubscriptionEvent$1 = (value) => value.payload;

/**
 * Create a new PubSub instance
 */
export function new_pubsub() {
  return new PubSub($dict.new$(), $dict.new$(), 1);
}

/**
 * Unsubscribe from a topic
 */
export function unsubscribe(pubsub, subscription_id) {
  let $ = $dict.get(pubsub.subscriptions, subscription_id);
  if ($ instanceof Ok) {
    let subscription$1 = $[0];
    let subscriptions = $dict.delete$(pubsub.subscriptions, subscription_id);
    let _block;
    let $1 = $dict.get(pubsub.topics, subscription$1.topic);
    if ($1 instanceof Ok) {
      let sub_ids = $1[0];
      let filtered = $list.filter(
        sub_ids,
        (id) => { return id !== subscription_id; },
      );
      if (filtered instanceof $Empty) {
        _block = $dict.delete$(pubsub.topics, subscription$1.topic);
      } else {
        _block = $dict.insert(pubsub.topics, subscription$1.topic, filtered);
      }
    } else {
      _block = pubsub.topics;
    }
    let topics = _block;
    return new PubSub(subscriptions, topics, pubsub.next_id);
  } else {
    return pubsub;
  }
}

/**
 * Publish an event to all subscribers of a topic
 */
export function publish(pubsub, topic, payload) {
  let $ = $dict.get(pubsub.topics, topic);
  if ($ instanceof Ok) {
    let sub_ids = $[0];
    return $list.each(
      sub_ids,
      (sub_id) => {
        let $1 = $dict.get(pubsub.subscriptions, sub_id);
        if ($1 instanceof Ok) {
          let subscription$1 = $1[0];
          return subscription$1.callback(payload);
        } else {
          return undefined;
        }
      },
    );
  } else {
    return undefined;
  }
}

/**
 * Get all active subscription IDs for a topic
 */
export function get_topic_subscribers(pubsub, topic) {
  let $ = $dict.get(pubsub.topics, topic);
  if ($ instanceof Ok) {
    let sub_ids = $[0];
    return sub_ids;
  } else {
    return toList([]);
  }
}

/**
 * Get subscription by ID
 */
export function get_subscription(pubsub, subscription_id) {
  let $ = $dict.get(pubsub.subscriptions, subscription_id);
  if ($ instanceof Ok) {
    let sub = $[0];
    return new Some(sub);
  } else {
    return new None();
  }
}

/**
 * Count active subscriptions
 */
export function subscription_count(pubsub) {
  return $dict.size(pubsub.subscriptions);
}

/**
 * Create a new subscription definition
 */
export function subscription(
  name,
  field_type,
  topic_resolver,
  event_transformer
) {
  return new SubscriptionDefinition(
    name,
    new None(),
    field_type,
    $dict.new$(),
    topic_resolver,
    event_transformer,
    new None(),
  );
}

/**
 * Add description to subscription
 */
export function description(sub, desc) {
  return new SubscriptionDefinition(
    sub.name,
    new Some(desc),
    sub.field_type,
    sub.arguments,
    sub.topic_resolver,
    sub.event_transformer,
    sub.filter,
  );
}

/**
 * Add argument to subscription
 */
export function argument(sub, arg) {
  return new SubscriptionDefinition(
    sub.name,
    sub.description,
    sub.field_type,
    $dict.insert(sub.arguments, arg.name, arg),
    sub.topic_resolver,
    sub.event_transformer,
    sub.filter,
  );
}

/**
 * Add filter function to subscription
 */
export function filter(sub, filter_fn) {
  return new SubscriptionDefinition(
    sub.name,
    sub.description,
    sub.field_type,
    sub.arguments,
    sub.topic_resolver,
    sub.event_transformer,
    new Some(filter_fn),
  );
}

/**
 * Convert subscription definition to a field definition for schema
 */
export function to_field_definition(sub) {
  return new $schema.FieldDefinition(
    sub.name,
    sub.description,
    sub.field_type,
    sub.arguments,
    new None(),
    false,
    new None(),
  );
}

/**
 * Create a simple topic string
 */
export function topic(name) {
  return name;
}

/**
 * Create a topic with an ID suffix (e.g., "user:123:updated")
 */
export function topic_with_id(base, id) {
  return (base + ":") + id;
}

function digit_to_string(d) {
  if (d === 0) {
    return "0";
  } else if (d === 1) {
    return "1";
  } else if (d === 2) {
    return "2";
  } else if (d === 3) {
    return "3";
  } else if (d === 4) {
    return "4";
  } else if (d === 5) {
    return "5";
  } else if (d === 6) {
    return "6";
  } else if (d === 7) {
    return "7";
  } else if (d === 8) {
    return "8";
  } else {
    return "9";
  }
}

function positive_int_to_string(n) {
  let $ = n < 10;
  if ($) {
    return digit_to_string(n);
  } else {
    return positive_int_to_string(globalThis.Math.trunc(n / 10)) + digit_to_string(
      n % 10,
    );
  }
}

function int_to_string(n) {
  let $ = n < 0;
  if ($) {
    return "-" + positive_int_to_string(- n);
  } else {
    return positive_int_to_string(n);
  }
}

/**
 * Subscribe to a topic
 */
export function subscribe(pubsub, topic, field_name, arguments$, callback) {
  let id = "sub_" + int_to_string(pubsub.next_id);
  let subscription$1 = new Subscription(
    id,
    topic,
    field_name,
    arguments$,
    callback,
  );
  let subscriptions = $dict.insert(pubsub.subscriptions, id, subscription$1);
  let _block;
  let $ = $dict.get(pubsub.topics, topic);
  if ($ instanceof Ok) {
    let existing = $[0];
    _block = listPrepend(id, existing);
  } else {
    _block = toList([id]);
  }
  let topic_subs = _block;
  let topics = $dict.insert(pubsub.topics, topic, topic_subs);
  let new_pubsub$1 = new PubSub(subscriptions, topics, pubsub.next_id + 1);
  return new SubscribeResult(id, new_pubsub$1);
}

function join_with_colon(parts) {
  if (parts instanceof $Empty) {
    return "";
  } else {
    let $ = parts.tail;
    if ($ instanceof $Empty) {
      let first = parts.head;
      return first;
    } else {
      let first = parts.head;
      let rest = $;
      return (first + ":") + join_with_colon(rest);
    }
  }
}

/**
 * Create a topic from multiple parts
 */
export function topic_from_parts(parts) {
  return join_with_colon(parts);
}
