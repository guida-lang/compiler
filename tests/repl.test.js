const child_process = require("node:child_process");
const path = require("node:path");
const tmp = require("tmp");

describe("repl", () => {
    const tmpobj = tmp.dirSync();
    let serverProcess, timeoutId;

    const run = (input, output, done) => {
        const repl = child_process.spawn(path.join(__dirname, "../bin/index.js"), ["repl"], {
            cwd: tmpobj.name,
            stdio: "pipe",
            env: { ...process.env, GUIDA_REGISTRY: "http://localhost:3210" }
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

    beforeAll((done) => {
        // Start the package-registry server
        serverProcess = child_process.spawn("npm", ["start"], {
            cwd: path.join(__dirname, "../node_modules/package-registry"),
            stdio: "pipe",
            env: {
                ...process.env,
                DATABASE_URL: path.join(__dirname, "../assets/package-registry/stdlib.sqlite3"),
                PORT: "3210"
            },
        });

        // Wait for server to be ready
        let serverReady = false;
        const checkServer = setInterval(() => {
            child_process.exec("curl -s http://localhost:3210", (err) => {
                if (!err && !serverReady) {
                    serverReady = true;
                    clearInterval(checkServer);

                    child_process.execSync(`${path.join(__dirname, "../bin/index.js")} init --yes`, {
                        cwd: tmpobj.name,
                        stdio: "pipe",
                        env: { ...process.env, GUIDA_REGISTRY: "http://localhost:3210" }
                    });

                    done();
                }
            });
        }, 100);

        // Timeout after 5 seconds
        timeoutId = setTimeout(() => {
            if (!serverReady) {
                clearInterval(checkServer);
                done(new Error('Registry server failed to start!'));
            }
        }, 5000);
    });

    afterAll((done) => {
        clearTimeout(timeoutId);
        serverProcess?.kill();
        done();
    });

    test("1 + 1", (done) => {
        run("1 + 1", "\x1B[95m2\x1B[0m\x1B[90m : number\x1B[0m\n", done);
    }, 120_000);

    test("string", (done) => {
        run("\"Hello, World!\"", "\x1B[93m\"Hello, World!\"\x1B[0m\x1B[90m : String\x1B[0m\n", done);
    }, 120_000);
});