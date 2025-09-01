const FILE = "out.json";

import fs from "node:fs";

const content = JSON.parse(await Bun.stdin.text());
const node = content.inner.filter((x) => x.name === "____dejj_type_parse");
const nodeString = JSON.stringify(node, undefined, 2);
console.log(nodeString)
