const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");

describe("guida init command", () => {
    it("initializes application", () => {
        const tmpobj = tmp.dirSync();

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} init --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
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

    it("initializes package", () => {
        const tmpobj = tmp.dirSync();

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} init --package --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            "type": "package",
            "name": "author/project",
            "summary": "helpful summary of your project, less than 80 characters",
            "license": "BSD-3-Clause",
            "version": "1.0.0",
            "exposed-modules": [],
            "guida-version": "1.0.0 <= v < 2.0.0",
            "dependencies": {
                "guida-lang/stdlib": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {}
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
