import Parser = require("tree-sitter");
import JavaScript = require("tree-sitter-javascript");

import * as g from "./generated";

let parser = new Parser() as g.Parser;
parser.setLanguage(JavaScript);

let tree = parser.parse(`
    function foo() {
        return function bar() {}
    }
    function baz() {}
    class C {
        f = 5;
        m() {}
    }
`);

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

function printFunctionNames() {
    let cursor = tree.walk();
    do {
        const c = cursor as g.TypedTreeCursor;
        switch (c.nodeType) {
            case g.SyntaxType.FunctionExpression:
            case g.SyntaxType.FunctionDeclaration: {
                let node = c.currentNode;
                if (node.isNamed && node.nameNode != null) {
                    console.log(node.nameNode.text);
                }
                break;
            }
            case g.SyntaxType.ClassDeclaration: {
                let node = c.currentNode;
                console.log('Class with members: ' + getMemberNames(node).join(', '));
                break;
            }
        }
    } while(gotoPreorderSucc(cursor));
}

function getMemberNames(node: g.ClassDeclarationNode) {
    let result = [];
    for (let member of node.bodyNode.memberNodes) {
        if (member.type === g.SyntaxType.MethodDefinition) {
            result.push(member.nameNode.text);
        } else if (member.type === g.SyntaxType.FieldDefinition) {
            result.push(member.propertyNode.text);
        }
    }
    return result;
}

function gotoPreorderSucc(cursor: g.TreeCursor): boolean {
    if (cursor.gotoFirstChild())
        return true;
    while (!cursor.gotoNextSibling()) {
        if (!cursor.gotoParent()) {
            return false;
        }
    }
    return true;
}

printDeclaredNames();
printFunctionNames();

function printClassName1(node: g.SyntaxNode) {
    // 'node.isNamed' is needed since there is both a named and an unnamed node whose type is 'class'.
    if (node.isNamed && node.type === g.SyntaxType.Class) {
        console.log(node.nameNode?.text);
    }
}

function printClassName2(node: g.NamedNode) {
    // If 'node' is typed as 'NamedNode' there is no need for the 'isNamed' check.
    if (node.type === g.SyntaxType.Class) {
        console.log(node.nameNode?.text);
    }
}
