const path = require("path");
const childProcess = require("child_process");

describe("tuples", () => {
    test("allows 3+ tuples", () => {
        expect(() => {
            childProcess.execSync(
                `../../bin/index.js make src/GuidaTupleN.guida`,
                {
                    cwd: path.join(__dirname, "..", "assets", "some-guida-application")
                }
            );
        }).not.toThrow();
    });

    test("fails to compile in Elm projects", (done) => {
        childProcess.exec(
            `../../bin/index.js make src/GuidaTupleN.guida`,
            {
                cwd: path.join(__dirname, "..", "assets", "some-elm-application")
            }, (err, _stdout, stderr) => {
                expect(err).toBeDefined();
                expect(stderr).toMatch("UNEXPECTED FILE EXTENSION");
                done();
            }
        );
    });
});