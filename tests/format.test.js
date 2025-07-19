const fs = require("node:fs");
const path = require("node:path");
const childProcess = require("child_process");
const os = require("os");
const tmpDir = os.tmpdir();

const defaultModule = {
    header: "module Main exposing (..)",
    docs: "",
    imports: [],
    infixes: [],
    declarations: ["fn = ()"]
}
const fullExample = {
    ...defaultModule
    , docs: `{-| Tons of useful functions that get imported by default.

# Math
@docs Int, Float, (+), (-), (*), (/), (//), (^)

# Int to Float / Float to Int
@docs toFloat, round, floor, ceiling, truncate

# Equality
@docs (==), (/=)

# Comparison

These functions only work on \`comparable\` types. This includes numbers,
characters, strings, lists of comparable things, and tuples of comparable
things.

@docs (<), (>), (<=), (>=), max, min, compare, Order

# Booleans
@docs Bool, not, (&&), (||), xor

# Append Strings and Lists
@docs (++)

# Fancier Math
@docs modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e

# Angles
@docs degrees, radians, turns

# Trigonometry
@docs pi, cos, sin, tan, acos, asin, atan, atan2

# Polar Coordinates
@docs toPolar, fromPolar

# Floating Point Checks
@docs isNaN, isInfinite

# Function Helpers
@docs identity, always, (<|), (|>), (<<), (>>), Never, never

-}`
    , imports: [
        "-- IMPORTS",
        "import {- import1 -} Module1",
        "-- second import comment",
        "import {- import2.1 -} Module2 {- import2.2 -} as {- import2.3 -} M",
        "import {- import3.1 -} Module3 {- import3.2 -} exposing {- import3.3 -} (..)",
        "import {- import4.1 -} Module4 {- import4.2 -} exposing {- import4.3 -} ({- import4.4 -} fn1 {- import4.5 -}, {- import4.6 -} fn2 {- import4.7 -})",
        `import -- import5.1
            Module5 -- import5.2
            exposing -- import5.3
            (fn1 -- import5.4
            , fn2 -- import5.5
            )`
    ], infixes: [
        "-- INFIX OPERATORS",
        "infix {- infix2 -} right {- infix3 -} 0 {- infix4 -} (<|) {- infix5 -} = {- infix6 -} apL",
        "-- second infix comment",
        `infix -- infix7
            left -- infix8
            0 -- infix9
            (|>) -- infix10
            = -- infix11
            apR`,
    ], declarations: [
        "-- DECLARATIONS",
        "{-| unit comment -}",
        "unitFn = ()"
    ]
}

const examples = [
    // HEADERS
    ["Header", [
        { title: "no effects", filename: "NoEffects", module: defaultModule },
        { title: "ports", filename: "Ports", module: { ...defaultModule, header: "port module Main exposing (..)" } },
        { title: "manager", filename: "Manager", module: { ...defaultModule, header: "effect module Main where { command = MyCmd } exposing (..)" } },
        { title: "single-line exposing", filename: "SingleLineExposing", module: { ...defaultModule, header: "module Main exposing (fn1, fn2)" } },
        { title: "multi-line exposing", filename: "MultiLineExposing", module: { ...defaultModule, header: "module Main exposing (fn1\n , fn2)" } },
        { title: "all multi-line", filename: "AllMultiLineHeader", module: { ...defaultModule, header: "module\n Main\n exposing\n (fn1\n , fn2\n )" } },
    ]],
    // DOCS
    ["Docs", [
        { title: "basic", filename: "BasicDocs", module: { ...defaultModule, docs: "{-| some documentation\n-}" } },
    ]],
    // IMPORTS
    ["Imports", [
        { title: "basic", filename: "BasicImports", module: { ...defaultModule, imports: ["import Module1"] } },
        { title: "alias", filename: "AliasImports", module: { ...defaultModule, imports: ["import Module1 as M"] } },
        { title: "exposing open", filename: "ExposingOpenImports", module: { ...defaultModule, imports: ["import Module1 exposing (..)"] } },
        { title: "exposing specific", filename: "ExposingSpecificImports", module: { ...defaultModule, imports: ["import Module1 exposing (fn1, fn2)"] } },
        { title: "all multi-line", filename: "AllMultiLineImports", module: { ...defaultModule, imports: ["import\n Module1\n exposing\n (fn1\n , fn2\n )"] } },
    ]],
    // INFIXES
    ["Infixes", [
        {
            title: "basic", filename: "BasicInfixes", module: {
                ...defaultModule, infixes: [
                    "infix right 0 (<|) = apL",
                    "infix left  0 (|>) = apR",
                    "infix right 2 (||) = or",
                    "infix non   4 (<)  = lt",
                    "infix non   4 (>)  = gt",
                    "infix non   4 (<=) = le"
                ]
            }
        },
    ]],
    // VALUE DECLARATIONS
    ["Declarations", [
        { title: "unit type", filename: "UnitTypeDeclarations", module: { ...defaultModule, declarations: ["fn : ()\nfn = ()"] } },
        { title: "tuple type", filename: "TupleTypeDeclarations", module: { ...defaultModule, declarations: ["fn : ((), ())\nfn = ((), ())"] } },
        { title: "var type", filename: "VarTypeDeclarations", module: { ...defaultModule, declarations: ["fn : a -> a\nfn a = a"] } },
        { title: "unqualified type", filename: "UnqualifiedTypeDeclarations", module: { ...defaultModule, declarations: ["fn : List a -> List a\nfn list = list"] } },
        { title: "qualified type", filename: "QualifiedTypeDeclarations", module: { ...defaultModule, declarations: ["fn : Dict.Dict a -> Dict.Dict a\nfn dict = dict"] } },
        { title: "argument w/ parentheses type", filename: "ArgumentWithParenthesesTypeDeclarations", module: { ...defaultModule, declarations: ["fn : List (Maybe a)\nfn = []"] } },
        { title: "multiple declarations", filename: "MultipleDeclarations", module: { ...defaultModule, declarations: ["fn1 = ()\nfn2 = ()"] } },
        { title: "let block", filename: "LetBlockDeclarations", module: { ...defaultModule, declarations: ["fn = let _ = () in ()"] } },
        { title: "anonymous function", filename: "AnonymousFunctionDeclarations", module: { ...defaultModule, declarations: ["fn = \\_ -> ()"] } },
        { title: "anonymous function argument", filename: "AnonymousFunctionArgDeclarations", module: { ...defaultModule, declarations: ["fn = List.map (\\_ -> ())"] } },
        { title: "pipe operator", filename: "PipeOperatorDeclarations", module: { ...defaultModule, declarations: ["fn = \"\"\n |> String.trim"] } },
        { title: "list", filename: "ListDeclarations", module: { ...defaultModule, declarations: ["fn = [1,2,3]"] } },
        { title: "multi-line list", filename: "MultiLineListDeclarations", module: { ...defaultModule, declarations: ["fn = [\n 1,\n 2,3]"] } },
        { title: "argument w/ parentheses", filename: "ArgumentWithParenthesesDeclarations", module: { ...defaultModule, declarations: ["fn input = String.toInt (String.trim input)"] } },
        {
            title: "literals", filename: "LiteralDeclarations", module: {
                ...defaultModule, declarations: [
                    "decimalInt = 123",
                    "hexadecimalInt = 0xff",
                    "decimalFloat = 3.14",
                    "exponentFloat = 6.022e23",
                    // TODO/FIXME "smallExponentFloat = 1e3",
                    "char = 'a'",
                    "singleQuotedString = \"hello world!\"",
                    "tripleQuotedString = \"\"\"multiline\n strings\"\"\"",
                ]
            }
        },
    ]],
    // UNION DECLARATIONS
    ["Union", [
        { title: "single variant", filename: "SingleTypeUnionDeclarations", module: { ...defaultModule, declarations: ["type A = A"] } },
    ]],
    // ALIAS DECLARATIONS
    ["Alias", [
        { title: "integer", filename: "IntergerAliasDeclarations", module: { ...defaultModule, declarations: ["type alias A = Int"] } },
        { title: "single field record", filename: "SingleFieldRecordAliasDeclarations", module: { ...defaultModule, declarations: ["type alias A = { age: Int }"] } },
        { title: "multi-line record", filename: "MultiLineRecordAliasDeclarations", module: { ...defaultModule, declarations: ["type alias A = { age: Int\n , name: String }"] } },
    ]],
    // PORT DECLARATIONS
    ["Port", [
        { title: "in", filename: "InPortDeclarations", module: { ...defaultModule, declarations: ["port messageReceiver : (String -> msg) -> Sub msg"] } },
        { title: "out", filename: "OutPortDeclarations", module: { ...defaultModule, declarations: ["port sendMessage : String -> Cmd msg"] } },
    ]],
    // COMMENTS
    ["Comments", [
        { title: "single-line before header", filename: "SingleLineBeforeHeaderComments", module: { ...defaultModule, header: ["-- COMMENT\nmodule Main exposing (..)"] } },
        { title: "multi-line header", filename: "MultiLineHeaderComments", module: { ...defaultModule, header: ["module {- C1 -} Main {- C2 -} exposing {- C3 -} ({- C4 -}..{- C5 -})"] } },
        { title: "single-line header", filename: "SingleLineHeaderComments", module: { ...defaultModule, header: ["module -- C1\n Main -- C2\n exposing -- C3\n (..)"] } },
        { title: "single-line declaration", filename: "SingleLineDeclarationComments", module: { ...defaultModule, declarations: ["-- COMMENT", "fn = ()"] } },
        { title: "infix", filename: "InfixComments", module: { ...defaultModule, infixes: ["infix {- 1 -} right {- 2 -} 0 {- 3 -} (<|) {- 4 -} = {- 5 -} apL"] } },
        { title: "full-example", filename: "FullExample", module: fullExample }
    ]],
]

describe("format", () => {
    describe.each(examples)("%s", (example, modules) => {
        test.each(modules)("$title", ({ filename, module }) => {
            const moduleFilename = `${tmpDir}/GuidaTest${example}${filename}${process.pid}.Elm`;
            const elmOutput = `${tmpDir}/GuidaTestElmOutput${example}${filename}${process.pid}.Elm`;
            const guidaOutput = `${tmpDir}/GuidaTestGuidaOutput${example}${filename}${process.pid}.Elm`;

            fs.writeFileSync(moduleFilename, generateModule(module));

            childProcess.execSync(`elm-format ${moduleFilename} --output ${elmOutput}`, {
                cwd: path.join(__dirname, "..")
            });

            childProcess.execSync(`./bin/index.js format ${moduleFilename} --output ${guidaOutput}`, {
                cwd: path.join(__dirname, "..")
            });

            expect(fs.readFileSync(guidaOutput).toString()).toBe(fs.readFileSync(elmOutput).toString());
        });
    });
});

const generateModule = ({ header, docs, imports, infixes, declarations }) => {
    return `${header}
${docs}
${imports.join("\n")}
${infixes.join("\n")}
${declarations.join("\n")}`;
}