#!/usr/bin/env node

const fs = require('node:fs');

const argv = process.argv.slice(2);
const path = argv[0];

const data = fs
    .readFileSync(path, { encoding: 'utf8', flag: 'r' })
    /* Replaces the Crash.crash function with one that logs to `stderr` and exits with `-1`. */
    .replace(`var $author$project$Utils$Crash$crash = function (str) {
\tcrash:
\twhile (true) {
\t\tvar $temp$str = str;
\t\tstr = $temp$str;
\t\tcontinue crash;
\t}
};`, `var $author$project$Utils$Crash$crash = function (str) {
\tError.stackTraceLimit = Infinity;
\ttry {
\t\tthrow new Error(str);
\t} catch(e) {
\t\tconsole.error(e.stack);
\t}
\ttypeof process !== "undefined" && process.exit(1);
};`);

fs.writeFileSync(path, data, { encoding: 'utf8', flag: 'w' });