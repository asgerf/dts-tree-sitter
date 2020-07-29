export interface Parser {
  parse(input: string | Input, previousTree?: Tree): Tree;
  getLanguage(): any;
  setLanguage(language: any): void;
  getLogger(): Logger;
  setLogger(logFunc: Logger): void;
}

export type Point = {
  row: number;
  column: number;
};

export type Range = {
  start: Point;
  end: Point;
};

export type Edit = {
  startIndex: number;
  oldEndIndex: number;
  newEndIndex: number;
  startPosition: Point;
  oldEndPosition: Point;
  newEndPosition: Point;
};

export type Logger = (
  message: string,
  params: {[param: string]: string},
  type: "parse" | "lex"
) => void;

export interface Input {
  seek(index: number): void;
  read(): any;
}

interface SyntaxNodeBase {
  tree: Tree;
  type: string;
  isNamed: boolean;
  text: string;
  startPosition: Point;
  endPosition: Point;
  startIndex: number;
  endIndex: number;
  parent: SyntaxNode | null;
  children: Array<SyntaxNode>;
  namedChildren: Array<SyntaxNode>;
  childCount: number;
  namedChildCount: number;
  firstChild: SyntaxNode | null;
  firstNamedChild: SyntaxNode | null;
  lastChild: SyntaxNode | null;
  lastNamedChild: SyntaxNode | null;
  nextSibling: SyntaxNode | null;
  nextNamedSibling: SyntaxNode | null;
  previousSibling: SyntaxNode | null;
  previousNamedSibling: SyntaxNode | null;

  hasChanges(): boolean;
  hasError(): boolean;
  isMissing(): boolean;
  toString(): string;
  child(index: number): SyntaxNode | null;
  namedChild(index: number): SyntaxNode | null;
  firstChildForIndex(index: number): SyntaxNode | null;
  firstNamedChildForIndex(index: number): SyntaxNode | null;

  descendantForIndex(index: number): SyntaxNode;
  descendantForIndex(startIndex: number, endIndex: number): SyntaxNode;
  namedDescendantForIndex(index: number): SyntaxNode;
  namedDescendantForIndex(startIndex: number, endIndex: number): SyntaxNode;
  descendantForPosition(position: Point): SyntaxNode;
  descendantForPosition(startPosition: Point, endPosition: Point): SyntaxNode;
  namedDescendantForPosition(position: Point): SyntaxNode;
  namedDescendantForPosition(startPosition: Point, endPosition: Point): SyntaxNode;
  descendantsOfType<T extends TypeString>(types: T | readonly T[], startPosition?: Point, endPosition?: Point): NodeOfType<T>[];

  closest<T extends SyntaxType>(types: T | readonly T[]): NamedNode<T> | null;
  walk(): TreeCursor;
}

export interface TreeCursor {
  nodeType: string;
  nodeText: string;
  nodeIsNamed: boolean;
  startPosition: Point;
  endPosition: Point;
  startIndex: number;
  endIndex: number;
  readonly currentNode: SyntaxNode

  reset(node: SyntaxNode): void
  gotoParent(): boolean;
  gotoFirstChild(): boolean;
  gotoFirstChildForIndex(index: number): boolean;
  gotoNextSibling(): boolean;
}

export interface Tree {
  readonly rootNode: SyntaxNode;

  edit(delta: Edit): Tree;
  walk(): TreeCursor;
  getChangedRanges(other: Tree): Range[];
  getEditedRange(other: Tree): Range;
}

interface NamedNodeBase extends SyntaxNodeBase {
    isNamed: true;
}

/** An unnamed node with the given type string. */
export interface UnnamedNode<T extends string = string> extends SyntaxNodeBase {
  type: T;
  isNamed: false;
}

type PickNamedType<Node, T extends string> = Node extends { type: T; isNamed: true } ? Node : never;

type PickType<Node, T extends string> = Node extends { type: T } ? Node : never;

/** A named node with the given `type` string. */
export type NamedNode<T extends SyntaxType = SyntaxType> = PickNamedType<SyntaxNode, T>;

/**
 * A node with the given `type` string.
 *
 * Note that this matches both named and unnamed nodes. Use `NamedNode<T>` to pick only named nodes.
 */
export type NodeOfType<T extends string> = PickType<SyntaxNode, T>;

interface TreeCursorOfType<S extends string, T extends SyntaxNodeBase> {
  nodeType: S;
  currentNode: T;
}

type TreeCursorRecord = { [K in TypeString]: TreeCursorOfType<K, NodeOfType<K>> };

/**
 * A tree cursor whose `nodeType` correlates with `currentNode`.
 *
 * The typing becomes invalid once the underlying cursor is mutated.
 *
 * The intention is to cast a `TreeCursor` to `TypedTreeCursor` before
 * switching on `nodeType`.
 *
 * For example:
 * ```ts
 * let cursor = root.walk();
 * while (cursor.gotoNextSibling()) {
 *   const c = cursor as TypedTreeCursor;
 *   switch (c.nodeType) {
 *     case SyntaxType.Foo: {
 *       let node = c.currentNode; // Typed as FooNode.
 *       break;
 *     }
 *   }
 * }
 * ```
 */
export type TypedTreeCursor = TreeCursorRecord[keyof TreeCursorRecord];

export interface ErrorNode extends NamedNodeBase {
    type: SyntaxType.ERROR;
    hasError(): true;
}

export const enum SyntaxType {
  ERROR = "ERROR",
  Arguments = "arguments",
  Array = "array",
  ArrayPattern = "array_pattern",
  ArrowFunction = "arrow_function",
  AssignmentExpression = "assignment_expression",
  AssignmentPattern = "assignment_pattern",
  AugmentedAssignmentExpression = "augmented_assignment_expression",
  AwaitExpression = "await_expression",
  BinaryExpression = "binary_expression",
  BreakStatement = "break_statement",
  CallExpression = "call_expression",
  CatchClause = "catch_clause",
  Class = "class",
  ClassBody = "class_body",
  ClassDeclaration = "class_declaration",
  ClassHeritage = "class_heritage",
  ComputedPropertyName = "computed_property_name",
  ContinueStatement = "continue_statement",
  DebuggerStatement = "debugger_statement",
  Decorator = "decorator",
  DoStatement = "do_statement",
  EmptyStatement = "empty_statement",
  ExportClause = "export_clause",
  ExportStatement = "export_statement",
  ExpressionStatement = "expression_statement",
  FinallyClause = "finally_clause",
  ForInStatement = "for_in_statement",
  ForStatement = "for_statement",
  FormalParameters = "formal_parameters",
  Function = "function",
  FunctionDeclaration = "function_declaration",
  GeneratorFunction = "generator_function",
  GeneratorFunctionDeclaration = "generator_function_declaration",
  IfStatement = "if_statement",
  Import = "import",
  ImportClause = "import_clause",
  ImportStatement = "import_statement",
  JsxAttribute = "jsx_attribute",
  JsxClosingElement = "jsx_closing_element",
  JsxElement = "jsx_element",
  JsxExpression = "jsx_expression",
  JsxFragment = "jsx_fragment",
  JsxNamespaceName = "jsx_namespace_name",
  JsxOpeningElement = "jsx_opening_element",
  JsxSelfClosingElement = "jsx_self_closing_element",
  LabeledStatement = "labeled_statement",
  LexicalDeclaration = "lexical_declaration",
  MemberExpression = "member_expression",
  MetaProperty = "meta_property",
  MethodDefinition = "method_definition",
  NamedImports = "named_imports",
  NamespaceImport = "namespace_import",
  NestedIdentifier = "nested_identifier",
  NewExpression = "new_expression",
  Object = "object",
  ObjectPattern = "object_pattern",
  Pair = "pair",
  ParenthesizedExpression = "parenthesized_expression",
  Program = "program",
  PublicFieldDefinition = "public_field_definition",
  Regex = "regex",
  RestParameter = "rest_parameter",
  ReturnStatement = "return_statement",
  SequenceExpression = "sequence_expression",
  SpreadElement = "spread_element",
  StatementBlock = "statement_block",
  String = "string",
  SubscriptExpression = "subscript_expression",
  SwitchBody = "switch_body",
  SwitchCase = "switch_case",
  SwitchDefault = "switch_default",
  SwitchStatement = "switch_statement",
  TemplateString = "template_string",
  TemplateSubstitution = "template_substitution",
  TernaryExpression = "ternary_expression",
  ThrowStatement = "throw_statement",
  TryStatement = "try_statement",
  UnaryExpression = "unary_expression",
  UpdateExpression = "update_expression",
  VariableDeclaration = "variable_declaration",
  VariableDeclarator = "variable_declarator",
  WhileStatement = "while_statement",
  WithStatement = "with_statement",
  YieldExpression = "yield_expression",
  EscapeSequence = "escape_sequence",
  False = "false",
  HashBangLine = "hash_bang_line",
  Identifier = "identifier",
  JsxText = "jsx_text",
  Null = "null",
  Number = "number",
  PropertyIdentifier = "property_identifier",
  RegexFlags = "regex_flags",
  RegexPattern = "regex_pattern",
  ShorthandPropertyIdentifier = "shorthand_property_identifier",
  StatementIdentifier = "statement_identifier",
  Super = "super",
  This = "this",
  True = "true",
  Undefined = "undefined",
}

export type UnnamedType =
  | "!"
  | "!="
  | "!=="
  | "\""
  | "${"
  | "%"
  | "%="
  | "&"
  | "&&"
  | "&="
  | "'"
  | "("
  | ")"
  | "*"
  | "**"
  | "**="
  | "*="
  | "+"
  | "++"
  | "+="
  | ","
  | "-"
  | "--"
  | "-="
  | "."
  | "..."
  | "/"
  | "/="
  | ":"
  | ";"
  | "<"
  | "<<"
  | "<<="
  | "<="
  | "="
  | "=="
  | "==="
  | "=>"
  | ">"
  | ">="
  | ">>"
  | ">>="
  | ">>>"
  | ">>>="
  | "?"
  | "@"
  | "["
  | "]"
  | "^"
  | "^="
  | "`"
  | "as"
  | "async"
  | "await"
  | "break"
  | "case"
  | "catch"
  | SyntaxType.Class // both named and unnamed
  | "const"
  | "continue"
  | "debugger"
  | "default"
  | "delete"
  | "do"
  | "else"
  | "export"
  | "extends"
  | "finally"
  | "for"
  | "from"
  | SyntaxType.Function // both named and unnamed
  | "get"
  | "if"
  | SyntaxType.Import // both named and unnamed
  | "in"
  | "instanceof"
  | "let"
  | "new"
  | "of"
  | "return"
  | "set"
  | "static"
  | "switch"
  | "target"
  | "throw"
  | "try"
  | "typeof"
  | "var"
  | "void"
  | "while"
  | "with"
  | "yield"
  | "{"
  | "|"
  | "|="
  | "||"
  | "}"
  | "~"
  ;

export type TypeString = SyntaxType | UnnamedType;

export type SyntaxNode = 
  | DeclarationNode
  | DestructuringPatternNode
  | ExpressionNode
  | StatementNode
  | ArgumentsNode
  | ArrayNode
  | ArrayPatternNode
  | ArrowFunctionNode
  | AssignmentExpressionNode
  | AssignmentPatternNode
  | AugmentedAssignmentExpressionNode
  | AwaitExpressionNode
  | BinaryExpressionNode
  | BreakStatementNode
  | CallExpressionNode
  | CatchClauseNode
  | ClassNode
  | ClassBodyNode
  | ClassDeclarationNode
  | ClassHeritageNode
  | ComputedPropertyNameNode
  | ContinueStatementNode
  | DebuggerStatementNode
  | DecoratorNode
  | DoStatementNode
  | EmptyStatementNode
  | ExportClauseNode
  | ExportStatementNode
  | ExpressionStatementNode
  | FinallyClauseNode
  | ForInStatementNode
  | ForStatementNode
  | FormalParametersNode
  | FunctionNode
  | FunctionDeclarationNode
  | GeneratorFunctionNode
  | GeneratorFunctionDeclarationNode
  | IfStatementNode
  | ImportNode
  | ImportClauseNode
  | ImportStatementNode
  | JsxAttributeNode
  | JsxClosingElementNode
  | JsxElementNode
  | JsxExpressionNode
  | JsxFragmentNode
  | JsxNamespaceNameNode
  | JsxOpeningElementNode
  | JsxSelfClosingElementNode
  | LabeledStatementNode
  | LexicalDeclarationNode
  | MemberExpressionNode
  | MetaPropertyNode
  | MethodDefinitionNode
  | NamedImportsNode
  | NamespaceImportNode
  | NestedIdentifierNode
  | NewExpressionNode
  | ObjectNode
  | ObjectPatternNode
  | PairNode
  | ParenthesizedExpressionNode
  | ProgramNode
  | PublicFieldDefinitionNode
  | RegexNode
  | RestParameterNode
  | ReturnStatementNode
  | SequenceExpressionNode
  | SpreadElementNode
  | StatementBlockNode
  | StringNode
  | SubscriptExpressionNode
  | SwitchBodyNode
  | SwitchCaseNode
  | SwitchDefaultNode
  | SwitchStatementNode
  | TemplateStringNode
  | TemplateSubstitutionNode
  | TernaryExpressionNode
  | ThrowStatementNode
  | TryStatementNode
  | UnaryExpressionNode
  | UpdateExpressionNode
  | VariableDeclarationNode
  | VariableDeclaratorNode
  | WhileStatementNode
  | WithStatementNode
  | YieldExpressionNode
  | UnnamedNode<"!">
  | UnnamedNode<"!=">
  | UnnamedNode<"!==">
  | UnnamedNode<"\"">
  | UnnamedNode<"${">
  | UnnamedNode<"%">
  | UnnamedNode<"%=">
  | UnnamedNode<"&">
  | UnnamedNode<"&&">
  | UnnamedNode<"&=">
  | UnnamedNode<"'">
  | UnnamedNode<"(">
  | UnnamedNode<")">
  | UnnamedNode<"*">
  | UnnamedNode<"**">
  | UnnamedNode<"**=">
  | UnnamedNode<"*=">
  | UnnamedNode<"+">
  | UnnamedNode<"++">
  | UnnamedNode<"+=">
  | UnnamedNode<",">
  | UnnamedNode<"-">
  | UnnamedNode<"--">
  | UnnamedNode<"-=">
  | UnnamedNode<".">
  | UnnamedNode<"...">
  | UnnamedNode<"/">
  | UnnamedNode<"/=">
  | UnnamedNode<":">
  | UnnamedNode<";">
  | UnnamedNode<"<">
  | UnnamedNode<"<<">
  | UnnamedNode<"<<=">
  | UnnamedNode<"<=">
  | UnnamedNode<"=">
  | UnnamedNode<"==">
  | UnnamedNode<"===">
  | UnnamedNode<"=>">
  | UnnamedNode<">">
  | UnnamedNode<">=">
  | UnnamedNode<">>">
  | UnnamedNode<">>=">
  | UnnamedNode<">>>">
  | UnnamedNode<">>>=">
  | UnnamedNode<"?">
  | UnnamedNode<"@">
  | UnnamedNode<"[">
  | UnnamedNode<"]">
  | UnnamedNode<"^">
  | UnnamedNode<"^=">
  | UnnamedNode<"`">
  | UnnamedNode<"as">
  | UnnamedNode<"async">
  | UnnamedNode<"await">
  | UnnamedNode<"break">
  | UnnamedNode<"case">
  | UnnamedNode<"catch">
  | UnnamedNode<SyntaxType.Class>
  | UnnamedNode<"const">
  | UnnamedNode<"continue">
  | UnnamedNode<"debugger">
  | UnnamedNode<"default">
  | UnnamedNode<"delete">
  | UnnamedNode<"do">
  | UnnamedNode<"else">
  | EscapeSequenceNode
  | UnnamedNode<"export">
  | UnnamedNode<"extends">
  | FalseNode
  | UnnamedNode<"finally">
  | UnnamedNode<"for">
  | UnnamedNode<"from">
  | UnnamedNode<SyntaxType.Function>
  | UnnamedNode<"get">
  | HashBangLineNode
  | IdentifierNode
  | UnnamedNode<"if">
  | UnnamedNode<SyntaxType.Import>
  | UnnamedNode<"in">
  | UnnamedNode<"instanceof">
  | JsxTextNode
  | UnnamedNode<"let">
  | UnnamedNode<"new">
  | NullNode
  | NumberNode
  | UnnamedNode<"of">
  | PropertyIdentifierNode
  | RegexFlagsNode
  | RegexPatternNode
  | UnnamedNode<"return">
  | UnnamedNode<"set">
  | ShorthandPropertyIdentifierNode
  | StatementIdentifierNode
  | UnnamedNode<"static">
  | SuperNode
  | UnnamedNode<"switch">
  | UnnamedNode<"target">
  | ThisNode
  | UnnamedNode<"throw">
  | TrueNode
  | UnnamedNode<"try">
  | UnnamedNode<"typeof">
  | UndefinedNode
  | UnnamedNode<"var">
  | UnnamedNode<"void">
  | UnnamedNode<"while">
  | UnnamedNode<"with">
  | UnnamedNode<"yield">
  | UnnamedNode<"{">
  | UnnamedNode<"|">
  | UnnamedNode<"|=">
  | UnnamedNode<"||">
  | UnnamedNode<"}">
  | UnnamedNode<"~">
  | ErrorNode
  ;

export type DeclarationNode = 
  | ClassDeclarationNode
  | FunctionDeclarationNode
  | GeneratorFunctionDeclarationNode
  | LexicalDeclarationNode
  | VariableDeclarationNode
  ;

export type DestructuringPatternNode = 
  | ArrayPatternNode
  | ObjectPatternNode
  ;

export type ExpressionNode = 
  | ArrayNode
  | ArrowFunctionNode
  | AssignmentExpressionNode
  | AugmentedAssignmentExpressionNode
  | AwaitExpressionNode
  | BinaryExpressionNode
  | CallExpressionNode
  | ClassNode
  | FalseNode
  | FunctionNode
  | GeneratorFunctionNode
  | IdentifierNode
  | ImportNode
  | JsxElementNode
  | JsxFragmentNode
  | JsxSelfClosingElementNode
  | MemberExpressionNode
  | MetaPropertyNode
  | NewExpressionNode
  | NullNode
  | NumberNode
  | ObjectNode
  | ParenthesizedExpressionNode
  | RegexNode
  | StringNode
  | SubscriptExpressionNode
  | TemplateStringNode
  | TernaryExpressionNode
  | ThisNode
  | TrueNode
  | UnaryExpressionNode
  | UndefinedNode
  | UpdateExpressionNode
  | YieldExpressionNode
  ;

export type StatementNode = 
  | DeclarationNode
  | BreakStatementNode
  | ContinueStatementNode
  | DebuggerStatementNode
  | DoStatementNode
  | EmptyStatementNode
  | ExportStatementNode
  | ExpressionStatementNode
  | ForInStatementNode
  | ForStatementNode
  | IfStatementNode
  | ImportStatementNode
  | LabeledStatementNode
  | ReturnStatementNode
  | StatementBlockNode
  | SwitchStatementNode
  | ThrowStatementNode
  | TryStatementNode
  | WhileStatementNode
  | WithStatementNode
  ;

export interface ArgumentsNode extends NamedNodeBase {
  type: SyntaxType.Arguments;
}

export interface ArrayNode extends NamedNodeBase {
  type: SyntaxType.Array;
}

export interface ArrayPatternNode extends NamedNodeBase {
  type: SyntaxType.ArrayPattern;
}

export interface ArrowFunctionNode extends NamedNodeBase {
  type: SyntaxType.ArrowFunction;
  bodyNode: ExpressionNode | StatementBlockNode;
  parameterNode?: IdentifierNode;
  parametersNode?: FormalParametersNode;
}

export interface AssignmentExpressionNode extends NamedNodeBase {
  type: SyntaxType.AssignmentExpression;
  leftNode: DestructuringPatternNode | IdentifierNode | MemberExpressionNode | ParenthesizedExpressionNode | SubscriptExpressionNode;
  rightNode: ExpressionNode;
}

export interface AssignmentPatternNode extends NamedNodeBase {
  type: SyntaxType.AssignmentPattern;
  leftNode: DestructuringPatternNode | ShorthandPropertyIdentifierNode;
  rightNode: ExpressionNode;
}

export interface AugmentedAssignmentExpressionNode extends NamedNodeBase {
  type: SyntaxType.AugmentedAssignmentExpression;
  leftNode: IdentifierNode | MemberExpressionNode | ParenthesizedExpressionNode | SubscriptExpressionNode;
  rightNode: ExpressionNode;
}

export interface AwaitExpressionNode extends NamedNodeBase {
  type: SyntaxType.AwaitExpression;
}

export interface BinaryExpressionNode extends NamedNodeBase {
  type: SyntaxType.BinaryExpression;
  leftNode: ExpressionNode;
  operatorNode: UnnamedNode<"!="> | UnnamedNode<"!=="> | UnnamedNode<"%"> | UnnamedNode<"&"> | UnnamedNode<"&&"> | UnnamedNode<"*"> | UnnamedNode<"**"> | UnnamedNode<"+"> | UnnamedNode<"-"> | UnnamedNode<"/"> | UnnamedNode<"<"> | UnnamedNode<"<<"> | UnnamedNode<"<="> | UnnamedNode<"=="> | UnnamedNode<"==="> | UnnamedNode<">"> | UnnamedNode<">="> | UnnamedNode<">>"> | UnnamedNode<">>>"> | UnnamedNode<"^"> | UnnamedNode<"in"> | UnnamedNode<"instanceof"> | UnnamedNode<"|"> | UnnamedNode<"||">;
  rightNode: ExpressionNode;
}

export interface BreakStatementNode extends NamedNodeBase {
  type: SyntaxType.BreakStatement;
  labelNode?: StatementIdentifierNode;
}

export interface CallExpressionNode extends NamedNodeBase {
  type: SyntaxType.CallExpression;
  argumentsNode: ArgumentsNode | TemplateStringNode;
  functionNode: ExpressionNode | FunctionNode | IdentifierNode | MemberExpressionNode | SuperNode;
}

export interface CatchClauseNode extends NamedNodeBase {
  type: SyntaxType.CatchClause;
  bodyNode: StatementBlockNode;
  parameterNode?: DestructuringPatternNode | IdentifierNode;
}

export interface ClassNode extends NamedNodeBase {
  type: SyntaxType.Class;
  bodyNode: ClassBodyNode;
  decoratorNodes: DecoratorNode[];
  nameNode?: IdentifierNode;
}

export interface ClassBodyNode extends NamedNodeBase {
  type: SyntaxType.ClassBody;
  memberNodes: (MethodDefinitionNode | PublicFieldDefinitionNode)[];
}

export interface ClassDeclarationNode extends NamedNodeBase {
  type: SyntaxType.ClassDeclaration;
  bodyNode: ClassBodyNode;
  decoratorNodes: DecoratorNode[];
  nameNode: IdentifierNode;
}

export interface ClassHeritageNode extends NamedNodeBase {
  type: SyntaxType.ClassHeritage;
}

export interface ComputedPropertyNameNode extends NamedNodeBase {
  type: SyntaxType.ComputedPropertyName;
}

export interface ContinueStatementNode extends NamedNodeBase {
  type: SyntaxType.ContinueStatement;
  labelNode?: StatementIdentifierNode;
}

export interface DebuggerStatementNode extends NamedNodeBase {
  type: SyntaxType.DebuggerStatement;
}

export interface DecoratorNode extends NamedNodeBase {
  type: SyntaxType.Decorator;
}

export interface DoStatementNode extends NamedNodeBase {
  type: SyntaxType.DoStatement;
  bodyNode: StatementNode;
  conditionNode: ParenthesizedExpressionNode;
}

export interface EmptyStatementNode extends NamedNodeBase {
  type: SyntaxType.EmptyStatement;
}

export interface ExportClauseNode extends NamedNodeBase {
  type: SyntaxType.ExportClause;
}

export interface ExportStatementNode extends NamedNodeBase {
  type: SyntaxType.ExportStatement;
  declarationNode?: DeclarationNode;
  decoratorNodes: DecoratorNode[];
  sourceNode?: StringNode;
  valueNode?: ExpressionNode;
}

export interface ExpressionStatementNode extends NamedNodeBase {
  type: SyntaxType.ExpressionStatement;
}

export interface FinallyClauseNode extends NamedNodeBase {
  type: SyntaxType.FinallyClause;
  bodyNode: StatementBlockNode;
}

export interface ForInStatementNode extends NamedNodeBase {
  type: SyntaxType.ForInStatement;
  bodyNode: StatementNode;
  leftNode: DestructuringPatternNode | IdentifierNode | MemberExpressionNode | ParenthesizedExpressionNode | SubscriptExpressionNode;
  rightNode: ExpressionNode | SequenceExpressionNode;
}

export interface ForStatementNode extends NamedNodeBase {
  type: SyntaxType.ForStatement;
  bodyNode: StatementNode;
  conditionNode: EmptyStatementNode | ExpressionStatementNode;
  incrementNode?: ExpressionNode | SequenceExpressionNode;
  initializerNode: EmptyStatementNode | ExpressionStatementNode | LexicalDeclarationNode | VariableDeclarationNode;
}

export interface FormalParametersNode extends NamedNodeBase {
  type: SyntaxType.FormalParameters;
}

export interface FunctionNode extends NamedNodeBase {
  type: SyntaxType.Function;
  bodyNode: StatementBlockNode;
  nameNode?: IdentifierNode;
  parametersNode: FormalParametersNode;
}

export interface FunctionDeclarationNode extends NamedNodeBase {
  type: SyntaxType.FunctionDeclaration;
  bodyNode: StatementBlockNode;
  nameNode: IdentifierNode;
  parametersNode: FormalParametersNode;
}

export interface GeneratorFunctionNode extends NamedNodeBase {
  type: SyntaxType.GeneratorFunction;
  bodyNode: StatementBlockNode;
  nameNode?: IdentifierNode;
  parametersNode: FormalParametersNode;
}

export interface GeneratorFunctionDeclarationNode extends NamedNodeBase {
  type: SyntaxType.GeneratorFunctionDeclaration;
  bodyNode: StatementBlockNode;
  nameNode: IdentifierNode;
  parametersNode: FormalParametersNode;
}

export interface IfStatementNode extends NamedNodeBase {
  type: SyntaxType.IfStatement;
  alternativeNode?: StatementNode;
  conditionNode: ParenthesizedExpressionNode;
  consequenceNode: StatementNode;
}

export interface ImportNode extends NamedNodeBase {
  type: SyntaxType.Import;
}

export interface ImportClauseNode extends NamedNodeBase {
  type: SyntaxType.ImportClause;
}

export interface ImportStatementNode extends NamedNodeBase {
  type: SyntaxType.ImportStatement;
  sourceNode?: StringNode;
}

export interface JsxAttributeNode extends NamedNodeBase {
  type: SyntaxType.JsxAttribute;
}

export interface JsxClosingElementNode extends NamedNodeBase {
  type: SyntaxType.JsxClosingElement;
  nameNode: IdentifierNode | JsxNamespaceNameNode | NestedIdentifierNode;
}

export interface JsxElementNode extends NamedNodeBase {
  type: SyntaxType.JsxElement;
  close_tagNode: JsxClosingElementNode;
  open_tagNode: JsxOpeningElementNode;
}

export interface JsxExpressionNode extends NamedNodeBase {
  type: SyntaxType.JsxExpression;
}

export interface JsxFragmentNode extends NamedNodeBase {
  type: SyntaxType.JsxFragment;
}

export interface JsxNamespaceNameNode extends NamedNodeBase {
  type: SyntaxType.JsxNamespaceName;
}

export interface JsxOpeningElementNode extends NamedNodeBase {
  type: SyntaxType.JsxOpeningElement;
  attributeNodes: (JsxAttributeNode | JsxExpressionNode)[];
  nameNode: IdentifierNode | JsxNamespaceNameNode | NestedIdentifierNode;
}

export interface JsxSelfClosingElementNode extends NamedNodeBase {
  type: SyntaxType.JsxSelfClosingElement;
  attributeNodes: (JsxAttributeNode | JsxExpressionNode)[];
  nameNode: IdentifierNode | JsxNamespaceNameNode | NestedIdentifierNode;
}

export interface LabeledStatementNode extends NamedNodeBase {
  type: SyntaxType.LabeledStatement;
  labelNode: StatementIdentifierNode;
}

export interface LexicalDeclarationNode extends NamedNodeBase {
  type: SyntaxType.LexicalDeclaration;
}

export interface MemberExpressionNode extends NamedNodeBase {
  type: SyntaxType.MemberExpression;
  objectNode: ExpressionNode | IdentifierNode | MemberExpressionNode | SuperNode;
  propertyNode: PropertyIdentifierNode;
}

export interface MetaPropertyNode extends NamedNodeBase {
  type: SyntaxType.MetaProperty;
}

export interface MethodDefinitionNode extends NamedNodeBase {
  type: SyntaxType.MethodDefinition;
  bodyNode: StatementBlockNode;
  decoratorNodes: DecoratorNode[];
  nameNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;
  parametersNode: FormalParametersNode;
}

export interface NamedImportsNode extends NamedNodeBase {
  type: SyntaxType.NamedImports;
}

export interface NamespaceImportNode extends NamedNodeBase {
  type: SyntaxType.NamespaceImport;
}

export interface NestedIdentifierNode extends NamedNodeBase {
  type: SyntaxType.NestedIdentifier;
}

export interface NewExpressionNode extends NamedNodeBase {
  type: SyntaxType.NewExpression;
  argumentsNode?: ArgumentsNode;
  constructorNode: ArrayNode | ArrowFunctionNode | ClassNode | FalseNode | FunctionNode | GeneratorFunctionNode | IdentifierNode | ImportNode | MemberExpressionNode | MetaPropertyNode | NewExpressionNode | NullNode | NumberNode | ObjectNode | ParenthesizedExpressionNode | RegexNode | StringNode | SubscriptExpressionNode | TemplateStringNode | ThisNode | TrueNode | UndefinedNode;
}

export interface ObjectNode extends NamedNodeBase {
  type: SyntaxType.Object;
}

export interface ObjectPatternNode extends NamedNodeBase {
  type: SyntaxType.ObjectPattern;
}

export interface PairNode extends NamedNodeBase {
  type: SyntaxType.Pair;
  keyNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;
  valueNode: ExpressionNode;
}

export interface ParenthesizedExpressionNode extends NamedNodeBase {
  type: SyntaxType.ParenthesizedExpression;
}

export interface ProgramNode extends NamedNodeBase {
  type: SyntaxType.Program;
}

export interface PublicFieldDefinitionNode extends NamedNodeBase {
  type: SyntaxType.PublicFieldDefinition;
  propertyNode: ComputedPropertyNameNode | NumberNode | PropertyIdentifierNode | StringNode;
  valueNode?: ExpressionNode;
}

export interface RegexNode extends NamedNodeBase {
  type: SyntaxType.Regex;
  flagsNode?: RegexFlagsNode;
  patternNode: RegexPatternNode;
}

export interface RestParameterNode extends NamedNodeBase {
  type: SyntaxType.RestParameter;
}

export interface ReturnStatementNode extends NamedNodeBase {
  type: SyntaxType.ReturnStatement;
}

export interface SequenceExpressionNode extends NamedNodeBase {
  type: SyntaxType.SequenceExpression;
  leftNode: ExpressionNode;
  rightNode: ExpressionNode | SequenceExpressionNode;
}

export interface SpreadElementNode extends NamedNodeBase {
  type: SyntaxType.SpreadElement;
}

export interface StatementBlockNode extends NamedNodeBase {
  type: SyntaxType.StatementBlock;
}

export interface StringNode extends NamedNodeBase {
  type: SyntaxType.String;
}

export interface SubscriptExpressionNode extends NamedNodeBase {
  type: SyntaxType.SubscriptExpression;
  indexNode: ExpressionNode | SequenceExpressionNode;
  objectNode: ExpressionNode | SuperNode;
}

export interface SwitchBodyNode extends NamedNodeBase {
  type: SyntaxType.SwitchBody;
}

export interface SwitchCaseNode extends NamedNodeBase {
  type: SyntaxType.SwitchCase;
  valueNode: ExpressionNode | SequenceExpressionNode;
}

export interface SwitchDefaultNode extends NamedNodeBase {
  type: SyntaxType.SwitchDefault;
}

export interface SwitchStatementNode extends NamedNodeBase {
  type: SyntaxType.SwitchStatement;
  bodyNode: SwitchBodyNode;
  valueNode: ParenthesizedExpressionNode;
}

export interface TemplateStringNode extends NamedNodeBase {
  type: SyntaxType.TemplateString;
}

export interface TemplateSubstitutionNode extends NamedNodeBase {
  type: SyntaxType.TemplateSubstitution;
}

export interface TernaryExpressionNode extends NamedNodeBase {
  type: SyntaxType.TernaryExpression;
  alternativeNode: ExpressionNode;
  conditionNode: ExpressionNode;
  consequenceNode: ExpressionNode;
}

export interface ThrowStatementNode extends NamedNodeBase {
  type: SyntaxType.ThrowStatement;
}

export interface TryStatementNode extends NamedNodeBase {
  type: SyntaxType.TryStatement;
  bodyNode: StatementBlockNode;
  finalizerNode?: FinallyClauseNode;
  handlerNode?: CatchClauseNode;
}

export interface UnaryExpressionNode extends NamedNodeBase {
  type: SyntaxType.UnaryExpression;
  argumentNode: ExpressionNode;
  operatorNode: UnnamedNode<"!"> | UnnamedNode<"+"> | UnnamedNode<"-"> | UnnamedNode<"delete"> | UnnamedNode<"typeof"> | UnnamedNode<"void"> | UnnamedNode<"~">;
}

export interface UpdateExpressionNode extends NamedNodeBase {
  type: SyntaxType.UpdateExpression;
  argumentNode: ExpressionNode;
  operatorNode: UnnamedNode<"++"> | UnnamedNode<"--">;
}

export interface VariableDeclarationNode extends NamedNodeBase {
  type: SyntaxType.VariableDeclaration;
}

export interface VariableDeclaratorNode extends NamedNodeBase {
  type: SyntaxType.VariableDeclarator;
  nameNode: DestructuringPatternNode | IdentifierNode;
  valueNode?: ExpressionNode;
}

export interface WhileStatementNode extends NamedNodeBase {
  type: SyntaxType.WhileStatement;
  bodyNode: StatementNode;
  conditionNode: ParenthesizedExpressionNode;
}

export interface WithStatementNode extends NamedNodeBase {
  type: SyntaxType.WithStatement;
  bodyNode: StatementNode;
  objectNode: ParenthesizedExpressionNode;
}

export interface YieldExpressionNode extends NamedNodeBase {
  type: SyntaxType.YieldExpression;
}

export interface EscapeSequenceNode extends NamedNodeBase {
  type: SyntaxType.EscapeSequence;
}

export interface FalseNode extends NamedNodeBase {
  type: SyntaxType.False;
}

export interface HashBangLineNode extends NamedNodeBase {
  type: SyntaxType.HashBangLine;
}

export interface IdentifierNode extends NamedNodeBase {
  type: SyntaxType.Identifier;
}

export interface JsxTextNode extends NamedNodeBase {
  type: SyntaxType.JsxText;
}

export interface NullNode extends NamedNodeBase {
  type: SyntaxType.Null;
}

export interface NumberNode extends NamedNodeBase {
  type: SyntaxType.Number;
}

export interface PropertyIdentifierNode extends NamedNodeBase {
  type: SyntaxType.PropertyIdentifier;
}

export interface RegexFlagsNode extends NamedNodeBase {
  type: SyntaxType.RegexFlags;
}

export interface RegexPatternNode extends NamedNodeBase {
  type: SyntaxType.RegexPattern;
}

export interface ShorthandPropertyIdentifierNode extends NamedNodeBase {
  type: SyntaxType.ShorthandPropertyIdentifier;
}

export interface StatementIdentifierNode extends NamedNodeBase {
  type: SyntaxType.StatementIdentifier;
}

export interface SuperNode extends NamedNodeBase {
  type: SyntaxType.Super;
}

export interface ThisNode extends NamedNodeBase {
  type: SyntaxType.This;
}

export interface TrueNode extends NamedNodeBase {
  type: SyntaxType.True;
}

export interface UndefinedNode extends NamedNodeBase {
  type: SyntaxType.Undefined;
}

