const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");

describe("guida init command", () => {
    const tmpobj = tmp.dirSync();

    it("initializes project structure", () => {
        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} init --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe",
            env: {
                ...process.env,
                GUIDA_HOME: path.join(tmpobj.name, ".guida"),
                GUIDA_REGISTRY: "http://localhost:3210"
            }
        });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            "type": "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            "dependencies": {
                "direct": {
                    "guida-lang/stdlib": "1.0.0"
                },
                "indirect": {}
            },
            "test-dependencies": {
                "direct": {},
                "indirect": {}
            }
        });

        expect(fs.existsSync(path.join(tmpobj.name, "src"))).toBe(true);
        expect(fs.existsSync(path.join(tmpobj.name, "tests"))).toBe(true);

        expect(fs.existsSync(path.join(tmpobj.name, "tests", "Example.guida"))).toBe(true);
        expect(fs.readFileSync(path.join(tmpobj.name, "tests", "Example.guida"), "utf-8")).toEqual(`module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "Implement our first test. See https://guida-lang.org/docs/1.0.0/commands/test for how to do this!"
`);
    });
});
