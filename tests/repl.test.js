const child_process = require("node:child_process");
const path = require("node:path");

describe("repl", () => {
    test("1 + 1", (done) => {
        run("1 + 1", "\x1B[95m2\x1B[0m\x1B[90m : number\x1B[0m\n", done);
    }, 120_000);

    test("string", (done) => {
        run("\"Hello, World!\"", "\x1B[93m\"Hello, World!\"\x1B[0m\x1B[90m : String\x1B[0m\n", done);
    }, 120_000);
});

const run = (input, output, done) => {
    const repl = child_process.spawn("./bin/index.js", ["repl"], {
        cwd: path.join(__dirname, ".."),
        stdio: "pipe"
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