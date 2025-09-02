const FILE = "out.json";

import fs from "node:fs";

const content = JSON.parse(await Bun.stdin.text());
const node = content.inner.filter((x) => x.name === "____stage0_clang_type_parse_0x0005eb20");
const nodeString = JSON.stringify(node, undefined, 2);
console.log(nodeString)
