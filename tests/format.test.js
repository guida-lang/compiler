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
        { title: "basic", filename: "BasicInfixes", module: { ...defaultModule, infixes: ["infix right 0 (<|) = apL"] } },
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