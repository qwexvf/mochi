// mochi/security.gleam
// Query security: depth limiting, complexity analysis, rate limiting

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/ast

// ============================================================================
// Configuration
// ============================================================================

pub type SecurityConfig {
  SecurityConfig(
    max_depth: Option(Int),
    max_complexity: Option(Int),
    max_aliases: Option(Int),
    max_root_fields: Option(Int),
  )
}

/// Default security configuration
pub fn default_config() -> SecurityConfig {
  SecurityConfig(
    max_depth: Some(10),
    max_complexity: Some(1000),
    max_aliases: Some(50),
    max_root_fields: Some(20),
  )
}

/// Permissive configuration (no limits)
pub fn no_limits() -> SecurityConfig {
  SecurityConfig(
    max_depth: None,
    max_complexity: None,
    max_aliases: None,
    max_root_fields: None,
  )
}

// ============================================================================
// Validation Result
// ============================================================================

pub type SecurityError {
  DepthLimitExceeded(depth: Int, max: Int)
  ComplexityLimitExceeded(complexity: Int, max: Int)
  AliasLimitExceeded(count: Int, max: Int)
  RootFieldLimitExceeded(count: Int, max: Int)
}

pub fn error_message(error: SecurityError) -> String {
  case error {
    DepthLimitExceeded(depth, max) ->
      "Query depth "
      <> int.to_string(depth)
      <> " exceeds maximum allowed depth of "
      <> int.to_string(max)
    ComplexityLimitExceeded(complexity, max) ->
      "Query complexity "
      <> int.to_string(complexity)
      <> " exceeds maximum allowed complexity of "
      <> int.to_string(max)
    AliasLimitExceeded(count, max) ->
      "Query has "
      <> int.to_string(count)
      <> " field aliases, exceeding maximum of "
      <> int.to_string(max)
    RootFieldLimitExceeded(count, max) ->
      "Query has "
      <> int.to_string(count)
      <> " root fields, exceeding maximum of "
      <> int.to_string(max)
  }
}

// ============================================================================
// Query Analysis
// ============================================================================

pub type QueryAnalysis {
  QueryAnalysis(
    depth: Int,
    complexity: Int,
    alias_count: Int,
    root_field_count: Int,
    field_count: Int,
  )
}

/// Analyze a query document
pub fn analyze(document: ast.Document) -> QueryAnalysis {
  let operations = get_operations(document)
  let fragments = get_fragments(document)

  let #(max_depth, total_complexity, alias_count, root_fields, field_count) =
    list.fold(operations, #(0, 0, 0, 0, 0), fn(acc, op) {
      let #(d, c, a, r, f) = acc
      let selection_set = get_operation_selection_set(op)
      let op_depth = calculate_depth(selection_set, fragments, 1)
      let op_complexity = calculate_complexity(selection_set, fragments)
      let op_aliases = count_aliases(selection_set, fragments)
      let op_root_fields = count_selections(selection_set)
      let op_field_count = count_fields(selection_set, fragments)

      #(
        int.max(d, op_depth),
        c + op_complexity,
        a + op_aliases,
        r + op_root_fields,
        f + op_field_count,
      )
    })

  QueryAnalysis(
    depth: max_depth,
    complexity: total_complexity,
    alias_count: alias_count,
    root_field_count: root_fields,
    field_count: field_count,
  )
}

/// Validate a query against security configuration
pub fn validate(
  document: ast.Document,
  config: SecurityConfig,
) -> Result(QueryAnalysis, SecurityError) {
  let analysis = analyze(document)

  // Check depth limit
  case config.max_depth {
    Some(max) if analysis.depth > max ->
      Error(DepthLimitExceeded(analysis.depth, max))
    _ -> Ok(Nil)
  }
  |> result.try(fn(_) {
    // Check complexity limit
    case config.max_complexity {
      Some(max) if analysis.complexity > max ->
        Error(ComplexityLimitExceeded(analysis.complexity, max))
      _ -> Ok(Nil)
    }
  })
  |> result.try(fn(_) {
    // Check alias limit
    case config.max_aliases {
      Some(max) if analysis.alias_count > max ->
        Error(AliasLimitExceeded(analysis.alias_count, max))
      _ -> Ok(Nil)
    }
  })
  |> result.try(fn(_) {
    // Check root field limit
    case config.max_root_fields {
      Some(max) if analysis.root_field_count > max ->
        Error(RootFieldLimitExceeded(analysis.root_field_count, max))
      _ -> Ok(Nil)
    }
  })
  |> result.map(fn(_) { analysis })
}

// ============================================================================
// Helper Functions
// ============================================================================

fn get_operations(document: ast.Document) -> List(ast.Operation) {
  list.filter_map(document.definitions, fn(def) {
    case def {
      ast.OperationDefinition(op) -> Ok(op)
      _ -> Error(Nil)
    }
  })
}

fn get_fragments(document: ast.Document) -> Dict(String, ast.Fragment) {
  list.fold(document.definitions, dict.new(), fn(acc, def) {
    case def {
      ast.FragmentDefinition(frag) -> dict.insert(acc, frag.name, frag)
      _ -> acc
    }
  })
}

fn get_operation_selection_set(op: ast.Operation) -> ast.SelectionSet {
  case op {
    ast.Operation(_, _, _, _, selection_set) -> selection_set
    ast.ShorthandQuery(selection_set) -> selection_set
  }
}

fn calculate_depth(
  selection_set: ast.SelectionSet,
  fragments: Dict(String, ast.Fragment),
  current_depth: Int,
) -> Int {
  case selection_set.selections {
    [] -> current_depth
    selections -> {
      list.fold(selections, current_depth, fn(max_depth, selection) {
        let child_depth = case selection {
          ast.FieldSelection(field) -> {
            case field.selection_set {
              Some(ss) -> calculate_depth(ss, fragments, current_depth + 1)
              None -> current_depth
            }
          }
          ast.FragmentSpread(spread) -> {
            case dict.get(fragments, spread.name) {
              Ok(frag) ->
                calculate_depth(
                  frag.selection_set,
                  fragments,
                  current_depth + 1,
                )
              Error(_) -> current_depth
            }
          }
          ast.InlineFragment(inline) -> {
            calculate_depth(inline.selection_set, fragments, current_depth)
          }
        }
        int.max(max_depth, child_depth)
      })
    }
  }
}

fn calculate_complexity(
  selection_set: ast.SelectionSet,
  fragments: Dict(String, ast.Fragment),
) -> Int {
  list.fold(selection_set.selections, 0, fn(total, selection) {
    let field_complexity = case selection {
      ast.FieldSelection(field) -> {
        // Base complexity of 1 per field
        let base = 1
        // Add complexity for nested selections
        let nested = case field.selection_set {
          Some(ss) -> calculate_complexity(ss, fragments)
          None -> 0
        }
        // Multiply by list argument if present (heuristic for list fields)
        let multiplier = get_list_multiplier(field.arguments)
        { base + nested } * multiplier
      }
      ast.FragmentSpread(spread) -> {
        case dict.get(fragments, spread.name) {
          Ok(frag) -> calculate_complexity(frag.selection_set, fragments)
          Error(_) -> 0
        }
      }
      ast.InlineFragment(inline) -> {
        calculate_complexity(inline.selection_set, fragments)
      }
    }
    total + field_complexity
  })
}

fn get_list_multiplier(arguments: List(ast.Argument)) -> Int {
  // Look for common list size arguments: first, last, limit, take
  list.fold(arguments, 1, fn(mult, arg) {
    case arg.name {
      "first" | "last" | "limit" | "take" -> {
        case arg.value {
          ast.IntValue(n) if n > 0 -> int.max(mult, n)
          _ -> mult
        }
      }
      _ -> mult
    }
  })
}

fn count_aliases(
  selection_set: ast.SelectionSet,
  fragments: Dict(String, ast.Fragment),
) -> Int {
  list.fold(selection_set.selections, 0, fn(count, selection) {
    case selection {
      ast.FieldSelection(field) -> {
        let has_alias = case field.alias {
          Some(_) -> 1
          None -> 0
        }
        let nested = case field.selection_set {
          Some(ss) -> count_aliases(ss, fragments)
          None -> 0
        }
        count + has_alias + nested
      }
      ast.FragmentSpread(spread) -> {
        case dict.get(fragments, spread.name) {
          Ok(frag) -> count + count_aliases(frag.selection_set, fragments)
          Error(_) -> count
        }
      }
      ast.InlineFragment(inline) -> {
        count + count_aliases(inline.selection_set, fragments)
      }
    }
  })
}

fn count_selections(selection_set: ast.SelectionSet) -> Int {
  list.length(selection_set.selections)
}

fn count_fields(
  selection_set: ast.SelectionSet,
  fragments: Dict(String, ast.Fragment),
) -> Int {
  list.fold(selection_set.selections, 0, fn(count, selection) {
    case selection {
      ast.FieldSelection(field) -> {
        let nested = case field.selection_set {
          Some(ss) -> count_fields(ss, fragments)
          None -> 0
        }
        count + 1 + nested
      }
      ast.FragmentSpread(spread) -> {
        case dict.get(fragments, spread.name) {
          Ok(frag) -> count + count_fields(frag.selection_set, fragments)
          Error(_) -> count
        }
      }
      ast.InlineFragment(inline) -> {
        count + count_fields(inline.selection_set, fragments)
      }
    }
  })
}
