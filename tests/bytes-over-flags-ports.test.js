const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");
const util = require('node:util');
const { describe } = require("node:test");

describe("Bytes over Flags and Ports", () => {
    it("Elm project with bytes flags", (done) => {
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
                    "elm/bytes": "1.0.8",
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
        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.elm"), `module Main exposing (main)

import Bytes exposing (Bytes)


main : Program Bytes () ()
main =
    Platform.worker
        { init = \\flags -> ( (), Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }`);

        try {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} make src/Main.elm --output=/dev/null`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        } catch (e) {
            const strippedMessage = util.stripVTControlCharacters(e.message);

            expect(strippedMessage).toMatch("BAD FLAGS");
            expect(strippedMessage).toMatch(new RegExp("Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and\\s+JSON values."));
            expect(e.status).toBe(1);

            return done();
        }

        done(new Error("Expected guida make to fail due to type error in flags"));
    });

    it("Elm project with bytes ports", (done) => {
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
                    "elm/bytes": "1.0.8",
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
        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.elm"), `port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode as BE



-- PORTS


port sendBytes : Bytes -> Cmd msg


port receiveBytes : (Bytes -> msg) -> Sub msg



-- MAIN


main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), sendBytes (BE.encode (BE.sequence [])) )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> receiveBytes (\\_ -> ())
        }`);

        try {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} make src/Main.elm --output=/dev/null`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        } catch (e) {
            const strippedMessage = util.stripVTControlCharacters(e.message);

            expect(strippedMessage).toMatch("PORT ERROR");
            expect(strippedMessage).toMatch("The `receiveBytes` port is trying to transmit a `Bytes` value");
            expect(strippedMessage).toMatch("The `sendBytes` port is trying to transmit a `Bytes` value");
            expect(e.status).toBe(1);

            return done();
        }

        done(new Error("Expected guida make to fail due to type error in flags"));
    });

    it("Guida Project with elm file with bytes flags", (done) => {
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
        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.elm"), `module Main exposing (main)

import Bytes exposing (Bytes)


main : Program Bytes () ()
main =
    Platform.worker
        { init = \\flags -> ( (), Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }`);

        try {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} make src/Main.elm --output=/dev/null`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        } catch (e) {
            const strippedMessage = util.stripVTControlCharacters(e.message);

            expect(strippedMessage).toMatch("BAD FLAGS");
            expect(strippedMessage).toMatch(new RegExp("Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and\\s+JSON values."));

            expect(e.status).toBe(1);

            return done();
        }

        done(new Error("Expected guida make to fail due to type error in flags"));
    });

    it("Guida Project with elm file with bytes ports", (done) => {
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
        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.elm"), `port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode as BE



-- PORTS


port sendBytes : Bytes -> Cmd msg


port receiveBytes : (Bytes -> msg) -> Sub msg



-- MAIN


main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), sendBytes (BE.encode (BE.sequence [])) )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> receiveBytes (\\_ -> ())
        }`);

        try {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} make src/Main.elm --output=/dev/null`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        } catch (e) {
            const strippedMessage = util.stripVTControlCharacters(e.message);

            expect(strippedMessage).toMatch("PORT ERROR");
            expect(strippedMessage).toMatch("The `receiveBytes` port is trying to transmit a `Bytes` value");
            expect(strippedMessage).toMatch("The `sendBytes` port is trying to transmit a `Bytes` value");
            expect(strippedMessage).toMatch(new RegExp("Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and\\s+JSON values."));

            expect(e.status).toBe(1);

            return done();
        }

        done(new Error("Expected guida make to fail due to type error in flags"));
    });

    it("Guida Project with guida file with bytes flags", (done) => {
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
        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.guida"), `module Main exposing (main)

import Bytes exposing (Bytes)


main : Program Bytes () ()
main =
    Platform.worker
        { init = \\flags -> ( (), Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }`);

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} make src/Main.guida --output=/dev/null`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        done();
    });

    it("Guida Project with guida file with bytes ports", (done) => {
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
        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.guida"), `port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode as BE



-- PORTS


port sendBytes : Bytes -> Cmd msg


port receiveBytes : (Bytes -> msg) -> Sub msg



-- MAIN


main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), sendBytes (BE.encode (BE.sequence [])) )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> receiveBytes (\\_ -> ())
        }`);

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} make src/Main.guida --output=/dev/null`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        done();
    });
});
