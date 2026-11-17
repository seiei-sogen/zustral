import grammar from "./zustral.tmLanguage";
import { writeFile } from "fs";
import * as path from "path";
const syntaxPath = path.join(
  __dirname,
  "..",
  "syntaxes",
  "zustral.tmLanguage.json"
);

writeFile(syntaxPath, JSON.stringify(grammar, undefined, 4), (err) => {
  if (err) console.error(`failed to write syntax: ${err}`);
});
