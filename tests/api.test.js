const fs = require("node:fs");
const path = require("node:path");
const process = require("node:process");
const os = require("node:os");
const tmp = require("tmp");
const guida = require("..");

const config = (env = {}) => {
    return {
        env,
        writeFile: async (path, data) => {
            return new Promise((resolve, _reject) => {
                fs.writeFile(path, data, (err) => {
                    if (err) throw err;
                    resolve();
                });
            });
        },
        readFile: async (path) => {
            return new Promise((resolve, _reject) => {
                fs.readFile(path, (err, data) => {
                    if (err) throw err;
                    resolve(data);
                });
            });
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

    it("make - sequential builds", async () => {
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

        const firstRunResult = await guida.make(config(), path.join(tmpobj.name, "src", "Main.guida"));
        expect(firstRunResult).toHaveProperty("output", expect.any(String));

        const secondRunResult = await guida.make(config(), path.join(tmpobj.name, "src", "Main.guida"));
        expect(secondRunResult).toHaveProperty("output", expect.any(String));
    });

    describe("getDefinitionLocation", () => {
        let assertLocation;
        let expressionsCommentLine, typeAnnotationsCommentLine;
        let fnRange, tTypeRange;
        let utilFnExpected, utilFn2Expected, utilTTypeExpected, utilUTypeExpected;
        let utilU1Expected, utilU2Expected;

        beforeAll(async () => {
            const tmpobj = tmp.dirSync();
            process.chdir(tmpobj.name);

            assertLocation = async (position, expected) => {
                const initialPath = path.join(tmpobj.name, "src", "Main.guida");
                const location = await guida.getDefinitionLocation(config(), { path: initialPath, position: position });

                expect(location).toEqual(Object.assign({ path: path.join(tmpobj.name, "src", "Main.guida") }, expected));
            }

            await guida.init(config(), { package: false });

            fs.writeFileSync(path.join(tmpobj.name, "src", "Main.guida"), `module Main exposing (..)

import Util exposing (U(..), fn2)

-- EXPRESSIONS

fn = ()

varFn = fn
varQualFn = Util.fn
listFn = [ fn ]
negatedFn = -fn
binopsFn = fn + fn
lambdaFn = \\_ -> fn
callFn = fn fn
ifFn = if fn then fn else fn
letFn = let _ = fn in fn
caseFn = case fn of _ -> fn
accessFn = fn.foo
updateFn = { fn | foo = fn }
recordFn = { foo = fn }
tupleFn = (fn, fn, fn, fn)
parensFn = (fn)

exposedVarFn = fn2
qualTypeFn = Util.U1
exposedTypeFn = U2

-- TYPE ANNOTATIONS

type T = T1

tLambda : T -> T
tLambda = ()

tType : T T
tType = ()

tTypeQual : Util.T T
tTypeQual = ()

tRecord : { t : T }
tRecord = ()

tTuple : (T, T, T, T)
tTuple = ()

exposedTType : U
exposedTType = ()
`);

            expressionsCommentLine = 4;
            typeAnnotationsCommentLine = 28;

            fnRange = { range: { start: { line: expressionsCommentLine + 2, character: 0 }, end: { line: expressionsCommentLine + 2, character: 2 } } };
            tTypeRange = { range: { start: { line: typeAnnotationsCommentLine + 2, character: 5 }, end: { line: typeAnnotationsCommentLine + 2, character: 6 } } };

            fs.writeFileSync(path.join(tmpobj.name, "src", "Util.guida"), `module Util exposing (..)

fn = ()

fn2 = ()

type T = T1 | T2

type U = U1 | U2
`);

            utilFnExpected = { path: path.join(tmpobj.name, "src", "Util.guida"), range: { start: { line: 2, character: 0 }, end: { line: 2, character: 2 } } }
            utilFn2Expected = { path: path.join(tmpobj.name, "src", "Util.guida"), range: { start: { line: 4, character: 0 }, end: { line: 4, character: 3 } } }
            utilTTypeExpected = { path: path.join(tmpobj.name, "src", "Util.guida"), range: { start: { line: 6, character: 5 }, end: { line: 6, character: 6 } } }
            utilUTypeExpected = { path: path.join(tmpobj.name, "src", "Util.guida"), range: { start: { line: 8, character: 5 }, end: { line: 8, character: 6 } } }
            utilU1Expected = { path: path.join(tmpobj.name, "src", "Util.guida"), range: { start: { line: 8, character: 9 }, end: { line: 8, character: 11 } } }
            utilU2Expected = { path: path.join(tmpobj.name, "src", "Util.guida"), range: { start: { line: 8, character: 14 }, end: { line: 8, character: 16 } } }
        });

        // EXPRESSIONS
        it("varFn", async () => { await assertLocation({ line: expressionsCommentLine + 4, character: 8 }, fnRange); });

        it("varQualFn", async () => { await assertLocation({ line: expressionsCommentLine + 5, character: 12 }, utilFnExpected); });

        it("listFn", async () => { await assertLocation({ line: expressionsCommentLine + 6, character: 11 }, fnRange); });

        it("negatedFn", async () => { await assertLocation({ line: expressionsCommentLine + 7, character: 13 }, fnRange); });

        it("binopsFn op", async () => { await assertLocation({ line: expressionsCommentLine + 8, character: 11 }, fnRange); });
        it("binopsFn final", async () => { await assertLocation({ line: expressionsCommentLine + 8, character: 16 }, fnRange); });

        it("lambdaFn", async () => { await assertLocation({ line: expressionsCommentLine + 9, character: 17 }, fnRange); });

        it("callFn func", async () => { await assertLocation({ line: expressionsCommentLine + 10, character: 9 }, fnRange); });
        it("callFn arg", async () => { await assertLocation({ line: expressionsCommentLine + 10, character: 12 }, fnRange); });

        it("ifFn condition", async () => { await assertLocation({ line: expressionsCommentLine + 11, character: 10 }, fnRange); });
        it("ifFn branch", async () => { await assertLocation({ line: expressionsCommentLine + 11, character: 18 }, fnRange); });
        it("ifFn finally", async () => { await assertLocation({ line: expressionsCommentLine + 11, character: 26 }, fnRange); });

        it("letFn def", async () => { await assertLocation({ line: expressionsCommentLine + 12, character: 16 }, fnRange); });
        it("letFn body", async () => { await assertLocation({ line: expressionsCommentLine + 12, character: 22 }, fnRange); });

        it("caseFn subject", async () => { await assertLocation({ line: expressionsCommentLine + 13, character: 14 }, fnRange); });
        it("caseFn body", async () => { await assertLocation({ line: expressionsCommentLine + 13, character: 25 }, fnRange); });

        it("accessFn record", async () => { await assertLocation({ line: expressionsCommentLine + 14, character: 11 }, fnRange); });

        it("updateFn name", async () => { await assertLocation({ line: expressionsCommentLine + 15, character: 13 }, fnRange); });
        it("updateFn field", async () => { await assertLocation({ line: expressionsCommentLine + 15, character: 24 }, fnRange); });

        it("recordFn", async () => { await assertLocation({ line: expressionsCommentLine + 16, character: 19 }, fnRange); });

        it("tupleFn 1st", async () => { await assertLocation({ line: expressionsCommentLine + 17, character: 11 }, fnRange); });
        it("tupleFn 2nd", async () => { await assertLocation({ line: expressionsCommentLine + 17, character: 15 }, fnRange); });
        it("tupleFn 3rd", async () => { await assertLocation({ line: expressionsCommentLine + 17, character: 19 }, fnRange); });
        it("tupleFn 4th", async () => { await assertLocation({ line: expressionsCommentLine + 17, character: 23 }, fnRange); });

        it("parensFn", async () => { await assertLocation({ line: expressionsCommentLine + 18, character: 12 }, fnRange); });

        it("exposedVarFn", async () => { await assertLocation({ line: expressionsCommentLine + 20, character: 15 }, utilFn2Expected); });
        it("qualTypeFn", async () => { await assertLocation({ line: expressionsCommentLine + 21, character: 13 }, utilU1Expected); });
        it("exposedTypeFn", async () => { await assertLocation({ line: expressionsCommentLine + 22, character: 16 }, utilU2Expected); });

        // TYPE ANNOTATIONS
        it("lambdaType arg", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 4, character: 10 }, tTypeRange); });
        it("lambdaType result", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 4, character: 15 }, tTypeRange); });

        it("tType name", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 7, character: 8 }, tTypeRange); });
        it("tType arg", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 7, character: 10 }, tTypeRange); });

        it("tTypeQual name", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 10, character: 12 }, utilTTypeExpected); });
        it("tTypeQual arg", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 10, character: 19 }, tTypeRange); });

        it("tRecord", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 13, character: 16 }, tTypeRange); });

        it("tTuple 1st", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 16, character: 10 }, tTypeRange); });
        it("tTuple 2nd", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 16, character: 13 }, tTypeRange); });
        it("tTuple 3rd", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 16, character: 16 }, tTypeRange); });
        it("tTuple 4th", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 16, character: 19 }, tTypeRange); });

        it("exposedTType", async () => { await assertLocation({ line: typeAnnotationsCommentLine + 19, character: 15 }, utilUTypeExpected); });
    });
});