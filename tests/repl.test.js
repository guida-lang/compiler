const child_process = require("node:child_process");
const path = require("node:path");
const tmp = require("tmp");

describe("repl", () => {
    const tmpobj = tmp.dirSync();

    const run = (input, output, done) => {
        const repl = child_process.spawn(path.join(__dirname, "../bin/index.js"), ["repl"], {
            cwd: tmpobj.name,
            stdio: "pipe",
            env: {
                ...process.env,
                GUIDA_HOME: path.join(tmpobj.name, ".guida"),
                GUIDA_REGISTRY: "http://localhost:3210"
            }
        });

        repl.stdout.on("data", (data) => {
            if (data.toString() === "> ") {
                repl.stdin.write(input + "\n");
            } else if (data.toString() === output) {
                repl.kill();
                done();
            }
        });
    }

    test("1 + 1", (done) => {
        run("1 + 1", "\x1B[95m2\x1B[0m\x1B[90m : number\x1B[0m\n", done);
    }, 120_000);

    test("string", (done) => {
        run("\"Hello, World!\"", "\x1B[93m\"Hello, World!\"\x1B[0m\x1B[90m : String\x1B[0m\n", done);
    }, 120_000);
});