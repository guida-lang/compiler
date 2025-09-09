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
\t\tprocess.stderr.write(e.stack);
\t\tprocess.stderr.write("\\\\n");
\t}
\tprocess.exit(-1);
};`)

    /* Prevents V8 from retaining large "concatenated string" chains, which can cause OOMs.
       Tested against `rtfeldman/elm-css` compilation.
    
       See the related discussion for context: https://discourse.elm-lang.org/t/guida-compiler-was-there-are-3-elm-compilers-written-in-elm/10329/34
       and issue: https://github.com/guida-lang/compiler/issues/107
     */
    .replace(`var _Bytes_read_string = F3(function(len, bytes, offset)
{
var string = '';
\tvar end = offset + len;
\tfor (; offset < end;)
\t{
\t\tvar byte = bytes.getUint8(offset++);
\t\tstring +=
\t\t\t(byte < 128)
\t\t\t\t? String.fromCharCode(byte)
\t\t\t\t:
\t\t\t((byte & 0xE0 /* 0b11100000 */) === 0xC0 /* 0b11000000 */)
\t\t\t\t? String.fromCharCode((byte & 0x1F /* 0b00011111 */) << 6 | bytes.getUint8(offset++) & 0x3F /* 0b00111111 */)
\t\t\t\t:
\t\t\t((byte & 0xF0 /* 0b11110000 */) === 0xE0 /* 0b11100000 */)
\t\t\t\t? String.fromCharCode(
\t\t\t\t\t(byte & 0xF /* 0b00001111 */) << 12
\t\t\t\t\t| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
\t\t\t\t\t| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
\t\t\t\t)
\t\t\t\t:
\t\t\t\t(byte =
\t\t\t\t\t((byte & 0x7 /* 0b00000111 */) << 18
\t\t\t\t\t\t| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 12
\t\t\t\t\t\t| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
\t\t\t\t\t\t| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
\t\t\t\t\t) - 0x10000
\t\t\t\t, String.fromCharCode(Math.floor(byte / 0x400) + 0xD800, byte % 0x400 + 0xDC00)
\t\t\t\t);
\t}
\treturn _Utils_Tuple2(offset, string);
});`, `var _Bytes_read_string = F3(function(len, bytes, offset)
{
\tvar string = [];
\tvar end = offset + len;
\tfor (; offset < end;)
\t{
\t\tvar byte = bytes.getUint8(offset++);
\t\tstring.push(
\t\t\t(byte < 128)
\t\t\t\t? String.fromCharCode(byte)
\t\t\t\t:
\t\t\t((byte & 0xE0 /* 0b11100000 */) === 0xC0 /* 0b11000000 */)
\t\t\t\t? String.fromCharCode((byte & 0x1F /* 0b00011111 */) << 6 | bytes.getUint8(offset++) & 0x3F /* 0b00111111 */)
\t\t\t\t:
\t\t\t((byte & 0xF0 /* 0b11110000 */) === 0xE0 /* 0b11100000 */)
\t\t\t\t? String.fromCharCode(
\t\t\t\t\t(byte & 0xF /* 0b00001111 */) << 12
\t\t\t\t\t| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
\t\t\t\t\t| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
\t\t\t\t)
\t\t\t\t:
\t\t\t\t(byte =
\t\t\t\t\t((byte & 0x7 /* 0b00000111 */) << 18
\t\t\t\t\t\t| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 12
\t\t\t\t\t\t| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
\t\t\t\t\t\t| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
\t\t\t\t\t) - 0x10000
\t\t\t\t, String.fromCharCode(Math.floor(byte / 0x400) + 0xD800, byte % 0x400 + 0xDC00)
\t\t\t\t)
\t\t);
\t}
\treturn _Utils_Tuple2(offset, string.join(''));
});`);

fs.writeFileSync(path, data, { encoding: 'utf8', flag: 'w' });