const fs = require("fs");
const path = require("path");
const childProcess = require("child_process");
const os = require("os");

describe("tuples", () => {
    test("allows 3+ tuples", () => {
        expect(() => {
            childProcess.execSync(
                `../../../bin/index.js make src/GuidaTupleN.guida`,
                { cwd: path.join(__dirname, "assets", "some-application") }
            );
        }).not.toThrow();
    });
});