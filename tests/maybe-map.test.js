const path = require("path");
const child_process = require("child_process");

describe("maybe map", () => {
    test("performance for large mapping sequence", () => {
        const start = Date.now();

        child_process.execSync(
            `../../bin/index.js make src/MaybeMap.elm`,
            {
                cwd: path.join(__dirname, "..", "assets", "some-elm-application"),
            }
        );

        const duration = Date.now() - start;
        expect(duration).toBeLessThan(10_000);
    });
});