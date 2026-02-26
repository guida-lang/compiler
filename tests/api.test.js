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
        let mainPath, utilPath;
        let assertLocation;
        let expressionsCommentLine, unionTypeAnnotationsCommentLine, aliasTypeAnnotationsCommentLine, portsCommentLine;
        let fnRange, tTypeRange, userTypeRange;
        let utilFnExpected, utilFn2Expected, utilTTypeExpected, utilUTypeExpected;
        let utilU1Expected, utilU2Expected, carTypeExpected, sendMessageFnExpected, messageReceiverFnExpected;

        beforeAll(async () => {
            const tmpobj = tmp.dirSync();
            process.chdir(tmpobj.name);

            mainPath = path.join(tmpobj.name, "src", "Main.guida");
            utilPath = path.join(tmpobj.name, "src", "Util.guida");

            assertLocation = async (position, expected) => {
                const location = await guida.getDefinitionLocation(config(), { path: mainPath, position: position });
                expect(location).toEqual(Object.assign({ path: mainPath }, expected));
            }

            await guida.init(config(), { package: false });

            fs.writeFileSync(mainPath, `module Main exposing (..)

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

-- UNION TYPE ANNOTATIONS

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

-- ALIAS TYPE ANNOTATIONS

type alias User = { name: String }

aliasLambda : User -> User
aliasLambda = ()

qualAliasTypeFn = Util.Car

-- PORTS

sendMessageFn = Util.sendMessage "Hello"

messageReceiverSub = Util.messageReceiver (\\_ -> ())
`);

            expressionsCommentLine = 4;
            unionTypeAnnotationsCommentLine = 28;
            aliasTypeAnnotationsCommentLine = 50;
            portsCommentLine = 59;

            fnRange = { range: { start: { line: expressionsCommentLine + 2, character: 0 }, end: { line: expressionsCommentLine + 2, character: 2 } } };
            tTypeRange = { range: { start: { line: unionTypeAnnotationsCommentLine + 2, character: 5 }, end: { line: unionTypeAnnotationsCommentLine + 2, character: 6 } } };
            userTypeRange = { range: { start: { line: aliasTypeAnnotationsCommentLine + 2, character: 11 }, end: { line: aliasTypeAnnotationsCommentLine + 2, character: 15 } } };

            fs.writeFileSync(utilPath, `port module Util exposing (..)

fn = ()

fn2 = ()

type T = T1 | T2

type U = U1 | U2

type alias Car = { sold: Bool }

port sendMessage : String -> Cmd msg

port messageReceiver : (String -> msg) -> Sub msg
`);

            utilFnExpected = { path: utilPath, range: { start: { line: 2, character: 0 }, end: { line: 2, character: 2 } } };
            utilFn2Expected = { path: utilPath, range: { start: { line: 4, character: 0 }, end: { line: 4, character: 3 } } };
            utilTTypeExpected = { path: utilPath, range: { start: { line: 6, character: 5 }, end: { line: 6, character: 6 } } };
            utilUTypeExpected = { path: utilPath, range: { start: { line: 8, character: 5 }, end: { line: 8, character: 6 } } };
            utilU1Expected = { path: utilPath, range: { start: { line: 8, character: 9 }, end: { line: 8, character: 11 } } };
            utilU2Expected = { path: utilPath, range: { start: { line: 8, character: 14 }, end: { line: 8, character: 16 } } };
            carTypeExpected = { path: utilPath, range: { start: { line: 10, character: 11 }, end: { line: 10, character: 14 } } };
            sendMessageFnExpected = { path: utilPath, range: { start: { line: 12, character: 5 }, end: { line: 12, character: 16 } } };
            messageReceiverFnExpected = { path: utilPath, range: { start: { line: 14, character: 5 }, end: { line: 14, character: 20 } } };
        });

        // IMPORTS
        it("importUtil", async () => { await assertLocation({ line: 2, character: 7 }, { path: utilPath, range: { start: { line: 0, character: 12 }, end: { line: 0, character: 16 } } }); });
        it("importUtilFn2", async () => { await assertLocation({ line: 2, character: 29 }, utilFn2Expected); });

        // EXPRESSIONS
        it("varFn", async () => { await assertLocation({ line: expressionsCommentLine + 4, character: 8 }, fnRange); });
        it("varFn (end position)", async () => { await assertLocation({ line: expressionsCommentLine + 4, character: 10 }, fnRange); });

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

        // UNION TYPE ANNOTATIONS
        it("lambdaType arg", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 4, character: 10 }, tTypeRange); });
        it("lambdaType result", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 4, character: 15 }, tTypeRange); });

        it("tType name", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 7, character: 8 }, tTypeRange); });
        it("tType arg", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 7, character: 10 }, tTypeRange); });

        it("tTypeQual name", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 10, character: 12 }, utilTTypeExpected); });
        it("tTypeQual arg", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 10, character: 19 }, tTypeRange); });

        it("tRecord", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 13, character: 16 }, tTypeRange); });

        it("tTuple 1st", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 16, character: 10 }, tTypeRange); });
        it("tTuple 2nd", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 16, character: 13 }, tTypeRange); });
        it("tTuple 3rd", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 16, character: 16 }, tTypeRange); });
        it("tTuple 4th", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 16, character: 19 }, tTypeRange); });

        it("exposedTType", async () => { await assertLocation({ line: unionTypeAnnotationsCommentLine + 19, character: 15 }, utilUTypeExpected); });

        // ALIAS TYPE ANNOTATIONS
        it("aliasLambda", async () => { await assertLocation({ line: aliasTypeAnnotationsCommentLine + 4, character: 15 }, userTypeRange); });

        it("qualAliasTypeFn", async () => { await assertLocation({ line: aliasTypeAnnotationsCommentLine + 7, character: 18 }, carTypeExpected); });

        // PORTS
        it("sendMessageFn", async () => { await assertLocation({ line: portsCommentLine + 2, character: 16 }, sendMessageFnExpected); });

        it("messageReceiverSub", async () => { await assertLocation({ line: portsCommentLine + 4, character: 21 }, messageReceiverFnExpected); });
    });

    describe("findReferences", () => {
        let mainPath;
        let assertReferences;

        beforeAll(async () => {
            const tmpobj = tmp.dirSync();
            process.chdir(tmpobj.name);

            mainPath = path.join(tmpobj.name, "src", "Main.guida");

            assertReferences = async (position, expected) => {
                const refs = await guida.findReferences(config(), { path: mainPath, position: position });
                expect(refs).toEqual(expected);
            }

            await guida.init(config(), { package: false });

            fs.writeFileSync(mainPath, `module Main exposing (..)

fn a = a + a
single a = a
none a = 1
list a = [ a ]
neg a = -a
call a = id a
iff a = if a then a else a
lett a = let x = a in a
casee a = case a of _ -> a
acc a = a.x
upd a = { a | x = a }
rec a = { x = a }
tup a = ( a, a, a )
par a = (a)
ptup (a, b) = a
palias (x as a) = a
ppar (a) = a
`);
        });

        it("fn (argument)", async () => {
            await assertReferences({ line: 2, character: 3 }, [
                { path: mainPath, range: { start: { line: 2, character: 3 }, end: { line: 2, character: 4 } } },
                { path: mainPath, range: { start: { line: 2, character: 7 }, end: { line: 2, character: 8 } } },
                { path: mainPath, range: { start: { line: 2, character: 11 }, end: { line: 2, character: 12 } } }
            ]);
        });

        it("fn (first usage)", async () => {
            await assertReferences({ line: 2, character: 7 }, [
                { path: mainPath, range: { start: { line: 2, character: 3 }, end: { line: 2, character: 4 } } },
                { path: mainPath, range: { start: { line: 2, character: 7 }, end: { line: 2, character: 8 } } },
                { path: mainPath, range: { start: { line: 2, character: 11 }, end: { line: 2, character: 12 } } }
            ]);
        });

        it("fn (second usage)", async () => {
            await assertReferences({ line: 2, character: 11 }, [
                { path: mainPath, range: { start: { line: 2, character: 3 }, end: { line: 2, character: 4 } } },
                { path: mainPath, range: { start: { line: 2, character: 7 }, end: { line: 2, character: 8 } } },
                { path: mainPath, range: { start: { line: 2, character: 11 }, end: { line: 2, character: 12 } } }
            ]);
        });

        it("singleFn (argument)", async () => {
            await assertReferences({ line: 3, character: 7 }, [
                { path: mainPath, range: { start: { line: 3, character: 7 }, end: { line: 3, character: 8 } } },
                { path: mainPath, range: { start: { line: 3, character: 11 }, end: { line: 3, character: 12 } } }
            ]);
        });

        it("no usage (argument)", async () => {
            await assertReferences({ line: 4, character: 5 }, []);
        });

        it("list (argument)", async () => {
            await assertReferences({ line: 5, character: 5 }, [
                { path: mainPath, range: { start: { line: 5, character: 5 }, end: { line: 5, character: 6 } } },
                { path: mainPath, range: { start: { line: 5, character: 11 }, end: { line: 5, character: 12 } } }
            ]);
        });

        it("negate (argument)", async () => {
            await assertReferences({ line: 6, character: 4 }, [
                { path: mainPath, range: { start: { line: 6, character: 4 }, end: { line: 6, character: 5 } } },
                { path: mainPath, range: { start: { line: 6, character: 9 }, end: { line: 6, character: 10 } } }
            ]);
        });

        it("call (argument)", async () => {
            await assertReferences({ line: 7, character: 5 }, [
                { path: mainPath, range: { start: { line: 7, character: 5 }, end: { line: 7, character: 6 } } },
                { path: mainPath, range: { start: { line: 7, character: 12 }, end: { line: 7, character: 13 } } }
            ]);
        });

        it("if (argument)", async () => {
            await assertReferences({ line: 8, character: 4 }, [
                { path: mainPath, range: { start: { line: 8, character: 4 }, end: { line: 8, character: 5 } } },
                { path: mainPath, range: { start: { line: 8, character: 11 }, end: { line: 8, character: 12 } } },
                { path: mainPath, range: { start: { line: 8, character: 18 }, end: { line: 8, character: 19 } } },
                { path: mainPath, range: { start: { line: 8, character: 25 }, end: { line: 8, character: 26 } } }
            ]);
        });

        it("let (argument)", async () => {
            await assertReferences({ line: 9, character: 5 }, [
                { path: mainPath, range: { start: { line: 9, character: 5 }, end: { line: 9, character: 6 } } },
                { path: mainPath, range: { start: { line: 9, character: 17 }, end: { line: 9, character: 18 } } },
                { path: mainPath, range: { start: { line: 9, character: 22 }, end: { line: 9, character: 23 } } }
            ]);
        });

        it("case (argument)", async () => {
            await assertReferences({ line: 10, character: 6 }, [
                { path: mainPath, range: { start: { line: 10, character: 6 }, end: { line: 10, character: 7 } } },
                { path: mainPath, range: { start: { line: 10, character: 15 }, end: { line: 10, character: 16 } } },
                { path: mainPath, range: { start: { line: 10, character: 25 }, end: { line: 10, character: 26 } } }
            ]);
        });

        it("access (argument)", async () => {
            await assertReferences({ line: 11, character: 4 }, [
                { path: mainPath, range: { start: { line: 11, character: 4 }, end: { line: 11, character: 5 } } },
                { path: mainPath, range: { start: { line: 11, character: 8 }, end: { line: 11, character: 9 } } }
            ]);
        });

        it("update (argument)", async () => {
            await assertReferences({ line: 12, character: 4 }, [
                { path: mainPath, range: { start: { line: 12, character: 4 }, end: { line: 12, character: 5 } } },
                { path: mainPath, range: { start: { line: 12, character: 10 }, end: { line: 12, character: 11 } } },
                { path: mainPath, range: { start: { line: 12, character: 18 }, end: { line: 12, character: 19 } } }
            ]);
        });

        it("record (argument)", async () => {
            await assertReferences({ line: 13, character: 4 }, [
                { path: mainPath, range: { start: { line: 13, character: 4 }, end: { line: 13, character: 5 } } },
                { path: mainPath, range: { start: { line: 13, character: 14 }, end: { line: 13, character: 15 } } }
            ]);
        });

        it("tuple (argument)", async () => {
            await assertReferences({ line: 14, character: 4 }, [
                { path: mainPath, range: { start: { line: 14, character: 4 }, end: { line: 14, character: 5 } } },
                { path: mainPath, range: { start: { line: 14, character: 10 }, end: { line: 14, character: 11 } } },
                { path: mainPath, range: { start: { line: 14, character: 13 }, end: { line: 14, character: 14 } } },
                { path: mainPath, range: { start: { line: 14, character: 16 }, end: { line: 14, character: 17 } } }
            ]);
        });

        it("parens (argument)", async () => {
            await assertReferences({ line: 15, character: 4 }, [
                { path: mainPath, range: { start: { line: 15, character: 4 }, end: { line: 15, character: 5 } } },
                { path: mainPath, range: { start: { line: 15, character: 9 }, end: { line: 15, character: 10 } } }
            ]);
        });

        it("tuple pattern binder (argument)", async () => {
            await assertReferences({ line: 16, character: 6 }, [
                { path: mainPath, range: { start: { line: 16, character: 6 }, end: { line: 16, character: 7 } } },
                { path: mainPath, range: { start: { line: 16, character: 14 }, end: { line: 16, character: 15 } } }
            ]);
        });

        it("alias pattern binder (argument)", async () => {
            await assertReferences({ line: 17, character: 13 }, [
                { path: mainPath, range: { start: { line: 17, character: 13 }, end: { line: 17, character: 14 } } },
                { path: mainPath, range: { start: { line: 17, character: 18 }, end: { line: 17, character: 19 } } }
            ]);
        });

        it("paren pattern binder (argument)", async () => {
            await assertReferences({ line: 18, character: 6 }, [
                { path: mainPath, range: { start: { line: 18, character: 6 }, end: { line: 18, character: 7 } } },
                { path: mainPath, range: { start: { line: 18, character: 11 }, end: { line: 18, character: 12 } } }
            ]);
        });
    });
});