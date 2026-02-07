const fs = require("node:fs");
const path = require("node:path");
const process = require("node:process");
const os = require("node:os");
const tmp = require("tmp");
const guida = require("..");

const config = () => {
    return {
        env: {},
        writeFile: async (path, data) => {
            fs.writeFileSync(path, data);
        },
        readFile: async (path) => {
            return await fs.readFileSync(path);
        },
        details: (path) => {
            const stats = fs.statSync(path);

            return Promise.resolve({
                type: stats.isFile() ? "file" : "directory",
                createdAt: Math.trunc(stats.birthtimeMs)
            });
        },
        createDirectory: (path) => {
            return new Promise((resolve, _reject) => {
                fs.mkdir(path, (_err) => {
                    resolve();
                });
            });
        },
        readDirectory: (path) => {
            return new Promise((resolve, _reject) => {
                fs.readdir(path, { recursive: false }, (err, files) => {
                    if (err) { throw err; }
                    resolve({ files });
                });
            });
        },
        getCurrentDirectory: () => {
            return Promise.resolve(process.cwd());
        },
        homedir: () => {
            return Promise.resolve(os.homedir());
        }
    };
};

describe("guida api", () => {
    it("init - initializes application", async () => {
        const tmpobj = tmp.dirSync();
        process.chdir(tmpobj.name);

        await guida.init(config(), { package: false });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            "type": "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            "dependencies": {
                "direct": {
                    "guida-lang/stdlib": "1.0.1"
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

    it("make - compiles simple application", async () => {
        const tmpobj = tmp.dirSync();
        process.chdir(tmpobj.name);

        await guida.init(config(), { package: false });

        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.guida"), `module Main exposing (main)

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }`);

        const result = await guida.make(config(), path.join(tmpobj.name, "src", "Main.guida"));
        expect(result).toHaveProperty("output", expect.any(String));
    });

    it("getDefinitionLocation - simple example", async () => {
        const tmpobj = tmp.dirSync();
        process.chdir(tmpobj.name);

        await guida.init(config(), { package: false });

        fs.writeFileSync(path.join(tmpobj.name, "src", "Main.guida"), `module Main exposing (main)

main : Program () Int ()
main =
    Platform.worker
        { init = \\_ -> ( add 1 1, Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }

add : Int -> Int -> Int
add a b =
    a + b`);

        const location = await guida.getDefinitionLocation(
            config(),
            { path: path.join(tmpobj.name, "src", "Main.guida"), position: { line: 5, character: 25 } }
        )

        expect(location).toEqual({
            path: path.join(tmpobj.name, "src", "Main.guida"),
            range: {
                start: { line: 11, character: 0 },
                end: { line: 11, character: 3 }
            }
        });
    });
});