const path = require("path");
const childProcess = require("child_process");

describe("tuples", () => {
    test("allows 3+ tuples", () => {
        expect(() => {
            childProcess.execSync(
                `../../bin/index.js make src/GuidaTupleN.guida`,
                {
                    cwd: path.join(__dirname, "..", "assets", "some-guida-application"), env: {
                        ...process.env,
                        GUIDA_HOME: path.join(__dirname, "..", "assets", "some-guida-application", ".guida"),
                        GUIDA_REGISTRY: "http://localhost:3210"
                    }
                }
            );
        }).not.toThrow();
    });

    test("fails to compile in Elm projects", (done) => {
        childProcess.exec(
            `../../bin/index.js make src/GuidaTupleN.guida`,
            {
                cwd: path.join(__dirname, "..", "assets", "some-elm-application"), env: {
                    ...process.env,
                    GUIDA_HOME: path.join(__dirname, "..", "assets", "some-elm-application", ".guida"),
                    GUIDA_REGISTRY: "https://package.elm-lang.org"
                },
            }, (err, _stdout, stderr) => {
                expect(err).toBeDefined();
                expect(stderr).toMatch("UNEXPECTED FILE EXTENSION");
                done();
            }
        );
    });
});