const path = require("path");
const childProcess = require("child_process");

describe("maybe map", () => {
    test("performance for large mapping sequence", () => {
        const start = Date.now();

        childProcess.execSync(
            `../../bin/index.js make src/MaybeMap.elm`,
            { cwd: path.join(__dirname, "..", "assets", "some-application") }
        );

        const duration = Date.now() - start;
        expect(duration).toBeLessThan(10_000);
    });
});