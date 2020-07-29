# dts-tree-sitter

**dts-tree-sitter** generates TypeScript `.d.ts` files for interacting the AST from a given tree-sitter grammar.

## Usage

```sh
npm i @asgerf/dts-tree-sitter

npx @asgerf/dts-tree-sitter INPUT > OUTPUT.d.ts
```

where `INPUT` is used to locate a `node-types.json` file in one of the following locations:
- `${INPUT}`
- `${INPUT}/node-types.json`
- `${INPUT}/src/node-types.json`
- `node_modules/${INPUT}/src/node-types.json`

## Example

The `tree-sitter-javascript` grammar can be compiled like this:
```sh
npm i tree-sitter-javascript
npx @asgerf/dts-tree-sitter tree-sitter-javascript > generated.d.ts
```

In the resulting grammar, two of the node types look like this:
```ts
export interface ClassDeclarationNode extends SyntaxNodeBase {
  type: SyntaxType.ClassDeclaration;
  bodyNode: ClassBodyNode;
  decoratorNodes?: DecoratorNode[];
  nameNode: IdentifierNode;
}

export interface ClassBodyNode extends SyntaxNodeBase {
  type: SyntaxType.ClassBody;
  memberNodes?: (MethodDefinitionNode | PublicFieldDefinitionNode)[];
}
```

This can be used like this (see [full example](examples/javascript/index.ts)):
```ts
import * as g from "./generated";

function getMemberNames(node: g.ClassDeclarationNode) {
    let result = [];
    for (let member of node.bodyNode.memberNodes) {
        if (member.type === g.SyntaxType.MethodDefinition) {
            result.push(member.nameNode.text);
        } else {
            result.push(member.propertyNode.text);
        }
    }
    return result;
}
```

Observe TypeScript do its magic: the type check in the `if` promotes the type of `member` to a `MethodDefinitionNode`
in the 'then' branch, and to `PublicFieldDefinitionNode` in the 'else' branch.

## Typed Tree Cursors

Tree sitter's `TreeCursor` allows fast traversal of an AST, and has two properties with correlated types: `nodeType`, and `currentNode`.
Once you've checked `nodeType`, it's annoying to have to cast `currentNode` to the correponding type right afterwards:
```ts
if (cursor.nodeType === g.SyntaxType.Function) {
  let node = cursor.currentNode as g.Function; // annoying cast
}
```

There's another way, which is handy in large switches: Cast the cursor itself to a `TypedTreeCursor` before switching on `nodeType`.
Then the guarded use of `currentNode` has the expected type. For example:
```ts
function printDeclaredNames() {
    let cursor = tree.walk();
    do {
        const c = cursor as g.TypedTreeCursor;
        switch (c.nodeType) {
            case g.SyntaxType.ClassDeclaration:
            case g.SyntaxType.FunctionDeclaration:
            case g.SyntaxType.VariableDeclarator: {
                let node = c.currentNode;
                console.log(node.nameNode.text);
                break;
            }
        }
    } while(gotoPreorderSucc(cursor));
}
```
- `node` gets the type `ClassDeclarationNode | FunctionDeclarationNode | VariableDeclaratorNode`.
- This allows safe access to `node.nameNode`, since each of those types have a `name` field.
- We don't pay the cost of invoking `currentNode` for other types of nodes.

## Trouble-shooting

### I get an error about "excessive stack depth" during compilation

This happens if you compare types from the general `tree-sitter.d.ts` file with those from the generated `.d.ts` file.
Every type from `tree-sitter.d.ts` has a stronger version in the generated file; make sure you don't mix and match.


### I get `UnnamedNode` types in places where I don't expect them

This can happen if the grammar contains rules and literals with the same name. For example this grammar rule,
```js
  func: $ => seq('func', $.name, $.body)
```
will produce a named node with type `func`, while the `'func'` literal will produce an unnamed node with type `func` as well.

This means a check like `node.type === 'func'` is not an exact type check, and the type of `node` will only be restricted to `FuncNode | UnnamedNode<'func'>`. This is _not_ a bug in the generated `.d.ts` file: there really are two kinds of nodes you need to handle after that check.

Some possible solutions are:
- Change the grammar to avoid rules with the same name as a keyword.
- Write the check as `node.isNamed && node.type === 'func'`.
- Change the declared type of `node` from `SyntaxNode` to `NamedNode`.
