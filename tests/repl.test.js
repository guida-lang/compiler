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
            stdio: "pipe",
            env: {
                ...process.env,
                GUIDA_HOME: path.join(dir, ".guida"),
                GUIDA_REGISTRY: "http://localhost:3210"
            }
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
});