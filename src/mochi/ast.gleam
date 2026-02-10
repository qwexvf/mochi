import gleam/option.{type Option}

pub type Document {
  Document(definitions: List(Definition))
}

pub type Definition {
  OperationDefinition(operation: Operation)
  FragmentDefinition(fragment: Fragment)
}

pub type Operation {
  Operation(
    operation_type: OperationType,
    name: Option(String),
    variable_definitions: List(VariableDefinition),
    directives: List(Directive),
    selection_set: SelectionSet,
  )
  ShorthandQuery(selection_set: SelectionSet)
}

pub type OperationType {
  Query
  Mutation
  Subscription
}

pub type SelectionSet {
  SelectionSet(selections: List(Selection))
}

pub type Selection {
  FieldSelection(field: Field)
  FragmentSpread(fragment_spread: FragmentSpreadValue)
  InlineFragment(inline_fragment: InlineFragmentValue)
}

pub type Field {
  Field(
    alias: Option(String),
    name: String,
    arguments: List(Argument),
    directives: List(Directive),
    selection_set: Option(SelectionSet),
  )
}

pub type Fragment {
  Fragment(
    name: String,
    type_condition: String,
    directives: List(Directive),
    selection_set: SelectionSet,
  )
}

pub type FragmentSpreadValue {
  FragmentSpreadValue(name: String, directives: List(Directive))
}

pub type InlineFragmentValue {
  InlineFragmentValue(
    type_condition: Option(String),
    directives: List(Directive),
    selection_set: SelectionSet,
  )
}

pub type VariableDefinition {
  VariableDefinition(
    variable: String,
    type_: Type,
    default_value: Option(Value),
    directives: List(Directive),
  )
}

pub type Argument {
  Argument(name: String, value: Value)
}

pub type Directive {
  Directive(name: String, arguments: List(Argument))
}

pub type Type {
  NamedType(name: String)
  ListType(inner_type: Type)
  NonNullType(inner_type: Type)
}

pub type Value {
  IntValue(value: Int)
  FloatValue(value: Float)
  StringValue(value: String)
  BooleanValue(value: Bool)
  NullValue
  EnumValue(value: String)
  ListValue(values: List(Value))
  ObjectValue(fields: List(ObjectField))
  VariableValue(name: String)
}

pub type ObjectField {
  ObjectField(name: String, value: Value)
}
