const child_process = require("node:child_process");
const path = require("node:path");
const tmp = require("tmp");
const util = require('node:util');

const DEBUG_LOG = false;

describe("repl", () => {
    const tmpobj = tmp.dirSync();

    const run = (input, output, done, dir = tmpobj.name) => {
        const repl = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["repl"], {
            cwd: dir,
            stdio: "pipe"
        });

        repl.stdout.on("data", (data) => {
            if (data.toString() === "> ") {
                repl.stdin.write(input + "\n");
            } else if (!data.toString().startsWith("\x1B[90m----")) {
                if (DEBUG_LOG) {
                    console.log(util.inspect(data.toString()));
                }

                try {
                    expect(data.toString()).toBe(output);
                    done();
                } catch (error) {
                    done(error);
                } finally {
                    repl.kill();
                }
            }
        });

        repl.stderr.on("data", (data) => {
            if (DEBUG_LOG) {
                console.log(util.inspect(data.toString()));
            }

            try {
                expect(data.toString()).toMatch(output);
                done();
            } catch (error) {
                done(error);
            } finally {
                repl.kill();
            }
        });
    }

    test("1 + 1", (done) => {
        run("1 + 1", "\x1B[95m2\x1B[0m\x1B[90m : number\x1B[0m\n", done);
    }, 120_000);

    test("string", (done) => {
        run("\"Hello, World!\"", "\x1B[93m\"Hello, World!\"\x1B[0m\x1B[90m : String\x1B[0m\n", done);
    }, 120_000);

    test("empty list", (done) => {
        run("[]", "[]\x1B[90m : List a\x1B[0m\n", done);
    }, 120_000);

    test("non-empty list", (done) => {
        run("[ 'a', 'b', 'c' ]", "[\x1B[92m'a'\x1B[0m,\x1B[92m'b'\x1B[0m,\x1B[92m'c'\x1B[0m]\x1B[90m : List Char\x1B[0m\n", done);
    }, 120_000);

    test("3-tuple", (done) => {
        run("( 1, 2, 3 )", "(\x1B[95m1\x1B[0m,\x1B[95m2\x1B[0m,\x1B[95m3\x1B[0m)\x1B[90m : ( number, number1, number2 )\x1B[0m\n", done);
    }, 120_000);

    test("4-tuple", (done) => {
        run("( 1, 2, 3, 4 )", "(\x1B[95m1\x1B[0m,\x1B[95m2\x1B[0m,\x1B[95m3\x1B[0m,\x1B[95m4\x1B[0m)\x1B[90m : ( number, number1, number2, number3 )\x1B[0m\n", done);
    }, 120_000);

    test("elm application root 4-tuple", (done) => {
        run("( 1, 2, 3, 4 )", new RegExp("-- BAD TUPLE -+ REPL"), done, path.join(__dirname, "..", "assets", "some-elm-application"));
    }, 120_000);

    test("small record", (done) => {
        run("{ a = 1, b = 2 }", "{ \x1B[37ma\x1B[0m = \x1B[95m1\x1B[0m, \x1B[37mb\x1B[0m = \x1B[95m2\x1B[0m }\x1B[90m\n    : { a : number, b : number1 }\x1B[0m\n", done);
    }, 120_000);

    test("large record", (done) => {
        run("{ a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7 }", "{ \x1B[37ma\x1B[0m = \x1B[95m1\x1B[0m, \x1B[37mb\x1B[0m = \x1B[95m2\x1B[0m, \x1B[37mc\x1B[0m = \x1B[95m3\x1B[0m, \x1B[37md\x1B[0m = \x1B[95m4\x1B[0m, \x1B[37me\x1B[0m = \x1B[95m5\x1B[0m, \x1B[37mf\x1B[0m = \x1B[95m6\x1B[0m, \x1B[37mg\x1B[0m = \x1B[95m7\x1B[0m }\x1B[90m\n    : { a : number\x1B[0m\n      , b : number1\x1B[0m\n      , c : number2\x1B[0m\n      , d : number3\x1B[0m\n      , e : number4\x1B[0m\n      , f : number5\x1B[0m\n      , g : number6\x1B[0m\n      }\x1B[0m\n", done);
    }, 120_000);
});