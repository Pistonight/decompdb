const tuNode = JSON.parse(await Bun.stdin.text());
const findNodeByName = (node, name: string) => {
    if (node.name === name) {
        return node;
    }
    if (node.kind === "NamespaceDecl" || node.kind === "TranslationUnitDecl") {
        if (node.inner) {
            for (const innerNode of node.inner) {
                const innerResult = findNodeByName(innerNode, name);
                if (innerResult) {
                    return innerResult;
                }
            }
        }
    }
    return undefined;
}
const node = findNodeByName(tuNode, "____stage0_clang_type_parse_0x0020cd56");
if (!node) {
    throw new Error("cannot find node");
}
const nodeString = JSON.stringify(node, undefined, 2);
console.log(nodeString)
