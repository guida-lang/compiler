const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");
const util = require('node:util');

describe("guida test command", () => {
    it("fails if missing source directory from elm.json", (done) => {
        const tmpobj = tmp.dirSync();

        fs.writeFileSync(path.join(tmpobj.name, "elm.json"), JSON.stringify({
            type: "application",
            "source-directories": [
                "src"
            ],
            "elm-version": "0.19.1",
            dependencies: {
                direct: {
                    "elm/browser": "1.0.2",
                    "elm/core": "1.0.5",
                    "elm/html": "1.0.1"
                },
                indirect: {
                    "elm/json": "1.1.4",
                    "elm/time": "1.0.0",
                    "elm/url": "1.0.0",
                    "elm/virtual-dom": "1.0.5"
                }
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }));

        const test = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["test"], {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        test.stderr.on("data", (data) => {
            try {
                expect(util.stripVTControlCharacters(data.toString())).toMatch(new RegExp("-- MISSING SOURCE DIRECTORY -+ elm.json"));
                done();
            } catch (error) {
                done(error);
            } finally {
                test.kill();
            }
        });
    });

    it("fails if no elm-explorations/test on test-dependencies for elm.json", (done) => {
        const tmpobj = tmp.dirSync();

        fs.writeFileSync(path.join(tmpobj.name, "elm.json"), JSON.stringify({
            type: "application",
            "source-directories": [
                "src"
            ],
            "elm-version": "0.19.1",
            dependencies: {
                direct: {
                    "elm/browser": "1.0.2",
                    "elm/core": "1.0.5",
                    "elm/html": "1.0.1"
                },
                indirect: {
                    "elm/json": "1.1.4",
                    "elm/time": "1.0.0",
                    "elm/url": "1.0.0",
                    "elm/virtual-dom": "1.0.5"
                }
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }));

        fs.mkdirSync(path.join(tmpobj.name, "src"));

        const test = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["test"], {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        test.stderr.on("data", (data) => {
            try {
                expect(data.toString()).toMatch(new RegExp(`You must have "elm-explorations/test" in your "test-dependencies" or "dependencies" to run guida-test.`));
                done();
            } catch (error) {
                done(error);
            } finally {
                test.kill();
            }
        });
    });

    it("runs tests with application elm.json", (done) => {
        const tmpobj = tmp.dirSync();

        fs.writeFileSync(path.join(tmpobj.name, "elm.json"), JSON.stringify({
            type: "application",
            "source-directories": [
                "src"
            ],
            "elm-version": "0.19.1",
            dependencies: {
                direct: {
                    "elm/browser": "1.0.2",
                    "elm/core": "1.0.5",
                    "elm/html": "1.0.1"
                },
                indirect: {
                    "elm/json": "1.1.4",
                    "elm/time": "1.0.0",
                    "elm/url": "1.0.0",
                    "elm/virtual-dom": "1.0.5"
                }
            },
            "test-dependencies": {
                direct: {
                    "elm-explorations/test": "2.2.0"
                },
                indirect: {
                    "elm/bytes": "1.0.8",
                    "elm/random": "1.0.0"
                }
            }
        }));

        fs.mkdirSync(path.join(tmpobj.name, "src"));
        fs.mkdirSync(path.join(tmpobj.name, "tests"));

        fs.writeFileSync(path.join(tmpobj.name, "tests", "Example.elm"), `module Example exposing (suite)

import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \\_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \\_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz Fuzz.string "restores the original string if you run it again" <|
                \\randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]`);

        const test = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["test"], {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        let stdout = "";

        test.stdout.on("data", (data) => {
            stdout += data.toString();
        });

        test.on("close", (code) => {
            const strippedOutput = util.stripVTControlCharacters(stdout);

            expect(strippedOutput).toMatch("TEST RUN PASSED");
            expect(strippedOutput).toMatch(new RegExp("Passed: +3"));
            expect(code).toBe(0);

            done();
        });
    }, 120_000);

    it("runs tests with package elm.json", (done) => {
        const tmpobj = tmp.dirSync();

        fs.writeFileSync(path.join(tmpobj.name, "elm.json"), JSON.stringify({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "elm-version": "0.19.1 <= v < 1.0.0",
            dependencies: {
                "elm/core": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {
                "elm-explorations/test": "2.2.0 <= v < 3.0.0"
            }
        }));

        fs.mkdirSync(path.join(tmpobj.name, "src"));
        fs.mkdirSync(path.join(tmpobj.name, "tests"));

        fs.writeFileSync(path.join(tmpobj.name, "tests", "Example.elm"), `module Example exposing (suite)

import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \\_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \\_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz Fuzz.string "restores the original string if you run it again" <|
                \\randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]`);

        const test = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["test"], {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        let stdout = "";

        test.stdout.on("data", (data) => {
            stdout += data.toString();
        });

        test.on("close", (code) => {
            const strippedOutput = util.stripVTControlCharacters(stdout);

            expect(strippedOutput).toMatch("TEST RUN PASSED");
            expect(strippedOutput).toMatch(new RegExp("Passed: +3"));
            expect(code).toBe(0);

            done();
        });
    }, 120_000);

    it("runs tests with application guida.json", (done) => {
        const tmpobj = tmp.dirSync();

        fs.writeFileSync(path.join(tmpobj.name, "guida.json"), JSON.stringify({
            type: "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            dependencies: {
                direct: {
                    "guida-lang/stdlib": "1.0.0"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }));

        fs.mkdirSync(path.join(tmpobj.name, "src"));
        fs.mkdirSync(path.join(tmpobj.name, "tests"));

        fs.writeFileSync(path.join(tmpobj.name, "tests", "Example.elm"), `module Example exposing (suite)

import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \\_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \\_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz Fuzz.string "restores the original string if you run it again" <|
                \\randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]`);

        const test = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["test"], {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        let stdout = "";

        test.stdout.on("data", (data) => {
            stdout += data.toString();
        });

        test.on("close", (code) => {
            const strippedOutput = util.stripVTControlCharacters(stdout);

            expect(strippedOutput).toMatch("TEST RUN PASSED");
            expect(strippedOutput).toMatch(new RegExp("Passed: +3"));
            expect(code).toBe(0);

            done();
        });
    }, 120_000);

    it("runs tests with package guida.json", (done) => {
        const tmpobj = tmp.dirSync();

        fs.writeFileSync(path.join(tmpobj.name, "guida.json"), JSON.stringify({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "guida-version": "1.0.0 <= v < 2.0.0",
            dependencies: {
                "guida-lang/stdlib": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {}
        }));

        fs.mkdirSync(path.join(tmpobj.name, "src"));
        fs.mkdirSync(path.join(tmpobj.name, "tests"));

        fs.writeFileSync(path.join(tmpobj.name, "tests", "Example.elm"), `module Example exposing (suite)

import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \\_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \\_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz Fuzz.string "restores the original string if you run it again" <|
                \\randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]`);

        const test = child_process.spawn(path.join(__dirname, "..", "bin", "index.js"), ["test"], {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        let stdout = "";

        test.stdout.on("data", (data) => {
            stdout += data.toString();
        });

        test.on("close", (code) => {
            const strippedOutput = util.stripVTControlCharacters(stdout);

            expect(strippedOutput).toMatch("TEST RUN PASSED");
            expect(strippedOutput).toMatch(new RegExp("Passed: +3"));
            expect(code).toBe(0);

            done();
        });
    }, 120_000);
});