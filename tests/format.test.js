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
        "import {- import1 -} Module1 -- first import comment",
        "-- second import comment",
        "import {- import2.1 -} Module2 {- import2.2 -} as {- import2.3 -} M {- import2.4 -}",
        "import {- import3.1 -} Module3 {- import3.2 -} exposing {- import3.3 -} ({- import3.4 -} .. {- import3.5 -})",
        "import {- import4.1 -} Module4 {- import4.2 -} exposing {- import4.3 -} ({- import4.4 -} fn1 {- import4.5 -}, {- import4.6 -} fn2 {- import4.7 -})",
        `import -- import5.1
        Module5 -- import5.2
        exposing -- import5.3
        ( -- import5.4
          fn1 -- import5.5
        , -- import5.6
          fn2 -- import5.7
        )`,
        "import {- import6.1 -} Module6 {- import6.2 -} as {- import6.3 -} M exposing {- import6.4 -} (..)",
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
        "{-| port in comment -}",
        "port {- port-in1 -} messageReceiver {- port-in2 -} : {- port-in3 -} ( {- port-in4 -} String {- port-in5 -} -> {- port-in6 -} msg {- port-in7 -}) {- port-in8 -} -> {- port-in9 -} Sub {- port-in10 -} msg {- port-in11 -}",
        "{-| port out comment -}",
        "port {- port-out1 -} sendMessage {- port-out2 -} : {- port-out3 -} String {- port-out4 -} -> {- port-out5 -} Cmd {- port-out6 -} msg {- port-out7 -}",
        "{-| char comment -}",
        "charFn {- char1 -} : {- char2 -} Char {- char3 -}",
        "charFn {- char4 -} = {- char5 -} 'a' {- char6 -}",
        "{-| string comment -}",
        "stringFn {- string1 -} : {- string2 -} String {- string3 -}",
        "stringFn {- string4 -} = {- string5 -} \"hello world!\" {- string6 -}",
        "{-| multi-line string comment -}",
        "stringMultiLineFn {- multi-line-string1 -} : {- multi-line-string2 -} String {- multi-line-string3 -}",
        "stringMultiLineFn {- multi-line-string4 -} = {- multi-line-string5 -}\n \"\"\"\n This is useful for holding JSON or other\n content that has \"quotation marks\".\n \"\"\" {- multi-line-string6 -}",
        "{-| int comment -}",
        "intFn {- int1 -} : {- int2 -} Int {- int3 -}",
        "intFn {- int4 -} = {- int5 -} 123 {- int6 -}",
        "{-| float comment -}",
        "floatFn {- float1 -} : {- float2 -} Float {- float3 -}",
        "floatFn {- float4 -} = {- float5 -} 3.14 {- float6 -}",
        "{-| lowVar comment -}",
        "lowVarFn {- lowVar1 -} : {- lowVar2 -} a {- lowVar3 -} -> {- lowVar4 -} a {- lowVar5 -}",
        "lowVarFn {- lowVar6 -} a {- lowVar7 -} = {- lowVar8 -} a {- lowVar9 -}",
        "{-| capVar comment -}",
        "capVarFn {- capVar1 -} : {- capVar2 -} Order {- capVar3 -}",
        "capVarFn {- capVar4 -} = {- capVar5 -} EQ {- capVar6 -}",
        "{-| lowVarQual comment -}",
        "lowVarQualFn {- lowVarQual1 -} : {- lowVarQual2 -} Float {- lowVarQual3 -}",
        "lowVarQualFn {- lowVarQual4 -} = {- lowVarQual5 -} Basics.e {- lowVarQual6 -}",
        "{-| capVarQual comment -}",
        "capVarQualFn {- capVarQual1 -} : {- capVarQual2 -} Basics.Order {- capVarQual3 -}",
        "capVarQualFn {- capVarQual4 -} = {- capVarQual5 -} Basics.EQ {- capVarQual6 -}",
        "{-| list comment -}",
        "listFn {- list1 -} : {- list2 -} List {- list3 -} Int {- list4 -}",
        "listFn {- list5 -} = {- list6 -} [ {- list7 -} 1 {- list8 -}, {- list9 -} 2 {- list10 -}, {- list11 -} 3 {- list12 -} ] {- list13 -}",
        "{-| op comment -}",
        "opFn {- op1 -} : {- op2 -} Int {- op3 -} -> {- op4 -} Int {- op5 -} -> {- op6 -} Int {- op7 -}",
        "opFn {- op8 -} = {- op9 -} (+) {- op10 -}",
        "{-| negate comment -}",
        "negateFn {- negate1 -} : {- negate2 -} Int {- negate3 -}",
        "negateFn {- negate4 -} = {- negate5 -} -4 {- negate6 -}",
        "{-| binops comment -}",
        "binopsFn {- binops1 -} : {- binops2 -} Int {- binops3 -}",
        "binopsFn {- binops4 -} = {- binops5 -} 1 {- binops6 -} + {- binops7 -} 2 {- binops8 -}",
        "{-| lambda arguments comment -}",
        "lambdaArgFn {- lambdaArg1 -} : {- lambdaArg2 -} ( {- lambdaArg3 -} Int {- lambdaArg4 -} -> {- lambdaArg5 -} Bool {- lambdaArg6 -} ) {- lambdaArg7 -} -> List {- lambdaArg8 -} String {- lambdaArg9 -} -> {- lambdaArg10 -} Int {- lambdaArg11 -}",
        "lambdaArgFn {- lambdaArg12 -} f {- lambdaArg13 -} = {- lambdaArg14 -} () {- lambdaArg15 -}",
        "{-| call comments -}",
        "callFn {- call1 -} : {- call2 -} Int {- call3 -}",
        "callFn {- call4 -} = {- call5 -} negate {- call6 -} 1 {- call7 -}",
        "{-| call multiple arguments comments -}",
        "callMultiArgsFn {- callMultiArgs1 -} : {- callMultiArgs2 -} Int {- callMultiArgs3 -}",
        "callMultiArgsFn {- callMultiArgs4 -} = {- callMultiArgs5 -} max {- callMultiArgs6 -} 1 {- callMultiArgs7 -} 2 {- callMultiArgs8 -}",
        "{-| if comments -}",
        "ifFn {- if1 -} : {- if2 -} Int {- if3 -}",
        "ifFn {- if4 -} = {- if5 -} if {- if5 -} True {- if6 -} then {- if7 -} 1 {- if8 -} else  {- if9 -} 2 {- if10 -}",
        "{-| let comments -}",
        "letFn {- let1 -} : {- let2 -} Int {- let3 -}",
        "letFn {- let4 -} = {- let5 -} let {- let6 -} val {- let7 -} = {- let8 -} 42 {- let9 -} in {- let10 -} val {- let11 -}",
        "{-| let multiple comments -}",
        "letMultipleFn {- letMultiple1 -} : {- letMultiple2 -} Int {- letMultiple3 -}",
        "letMultipleFn {- letMultiple4 -} = {- letMultiple5 -}\n let\n  {- letMultiple6 -}\n  val1 {- letMultiple7 -} = {- letMultiple8 -} 42 {- letMultiple9 -}\n  {- letMultiple10 -}\n  val2 {- letMultiple11 -} = {- letMultiple12 -} 43 {- letMultiple13 -}\n in  {- letMultiple14 -} val {- letMultiple15 -}",
        "{-| let destructure comments -}",
        "letDestructureFn {- letDestructure1 -} : {- letDestructure2 -} Int {- letDestructure3 -}",
        "letDestructureFn {- letDestructure4 -} = {- letDestructure5 -} let {- letDestructure6 -} { {- letDestructure7 -} val {- letDestructure8 -} } {- letDestructure9 -} = {- letDestructure10 -} someRecord {- letDestructure11 -} in  {- letDestructure12 -} val {- letDestructure13 -}",
        "{-| let signature and arguments comments -}",
        "letSignatureArgsFn {- letSignatureArgs1 -} : {- letSignatureArgs2 -} Int {- letSignatureArgs3 -}",
        "letSignatureArgsFn {- letSignatureArgs4 -} = {- letSignatureArgs5 -}\n let\n  {- letSignatureArgs6 -}\n  val1 {- letSignatureArgs7 -} : {- letSignatureArgs8 -} a {- letSignatureArgs9 -}\n  val1 {- letSignatureArgs10 -} a {- letSignatureArgs11 -} = {- letSignatureArgs12 -} 42 {- letSignatureArgs13 -}\n in  {- letSignatureArgs14 -} val {- letSignatureArgs15 -}",
        "{-| case comments -}",
        "caseFn {- case1 -} : {- case2 -} Int {- case3 -}",
        `caseFn {- case4 -} = {- case5 -} case {- case6 -} () {- case7 -} of {- case8 -}
        ({- case9 -} 1 {- case10 -}, {- case11 -} 2 {- case12 -}) -> {- case13 -} 3 {- case14 -}
        [{- case15 -}] -> {- case16 -} 4 {- case17 -}
        [{- case18 -} 1 {- case19 -}] -> {- case20 -} 5 {- case21 -}
        [{- case22 -} 1 {- case23 -}, {- case24 -} 2 {- case25 -}] -> {- case26 -} 6 {- case27 -}
        1 {- case28 -} :: {- case29 -} 2 {- case30 -} :: {- case31 -} 3 {- case32 -} -> {- case33 -} 7 {- case34 -}
        1 {- case35 -} as {- case36 -} x {- case37 -} -> {- case38 -} 8 {- case39 -}
        ({- case40 -} 1 {- case41 -} :: {- case42 -} [{- case43 -}] {- case44 -}) {- case45 -} as {- case46 -} y {- case47 -} -> {- case48 -} 9 {- case49 -}
        (1) {- case50 -} as {- case51 -} z {- case52 -} -> {- case53 -} 10 {- case54 -}
        _ {- case55 -} -> {- case56 -} 8 {- case57 -}`,
        "{-| acces comments -}",
        "accesFn {- acces1 -} : {- acces2 -} Int {- acces3 -}",
        "accesFn {- acces4 -} = {- acces5 -} a.b {- acces6 -}",
        "{-| update comments -}",
        "updateFn {- update1 -} : {- update2 -} { {- update3 -} x {- update4 -} | {- update5 -} a {- update6 -} : {- update7 -} Int {- update8 -} , {- update9 -} b {- update10 -} : {- update11 -} Int {- update12 -} } {- update13 -}",
        "updateFn {- update14 -} = {- update15 -} { {- update16 -} y {- update17 -} | {- update18 -} a {- update19 -} = {- update20 -} 1 {- update21 -} , {- update22 -} b {- update23 -} = {- update24 -} 2 {- update25 -} } {- update26 -}",
        "{-| record comments -}",
        "recordFn {- record1 -} : {- record2 -} { {- record3 -} a {- record4 -} : {- record5 -} Int {- record6 -} , {- record7 -} b {- record8 -} : {- record9 -} Int {- record10 -} } {- record11 -}",
        "recordFn {- record12 -} = {- record13 -} { {- record14 -} a {- record15 -} = {- record16 -} 1 {- record17 -} , {- record18 -} b {- record19 -} = {- record20 -} 2 {- record21 -} } {- record22 -}",
        "{-| empty record comments -}",
        "emptyRecordFn {- emptyRecord1 -} : {- emptyRecord2 -} { {- emptyRecord3 -} } {- emptyRecord4 -}",
        "emptyRecordFn {- emptyRecord5 -} = {- emptyRecord6 -} { {- emptyRecord7 -} } {- emptyRecord22 -}",
        "{-| unit comment -}",
        "unitFn {- unit1 -} : {- unit2 -} () {- unit3 -}",
        "unitFn {- unit4 -} = {- unit5 -} () {- unit6 -}",
        "{-| tuple comment -}",
        "tupleFn {- tuple1 -} : {- tuple2 -} ( {- tuple3 -} Int {- tuple4 -}, {- tuple5 -} Int {- tuple6 -} ) {- tuple7 -}",
        "tupleFn {- tuple8 -} = {- tuple9 -} ( {- tuple10 -} 1 {- tuple11 -}, {- tuple12 -} 2 {- tuple13 -} ) {- tuple14 -}",
        "{-| shader comment -}",
        "shaderFn {- shader1 -} : {- shader2 -} WebGL.Shader {- shader3 -} Vertex {- shader4 -} Uniforms {- shader5 -} { {- shader6 -} vcolor {- shader7 -} : {- shader8 -} Vec3 {- shader9 -} } {- shader10 -}",
        "shaderFn {- shader11 -} = {- shader12 -} [glsl|\n        attribute vec3 position;\n        attribute vec3 color;\n        uniform mat4 perspective;\n        varying vec3 vcolor;\n\n        void main () {\n            gl_Position = perspective * vec4(position, 1.0);\n            vcolor = color;\n        }\n    |] {- shader13 -}",
        "{-| lambda comment -}",
        "lambdaFn {- lambda1 -} : {- lambda2 -} Int {- lambda3 -} -> {- lambda4 -} () {- lambda5 -}",
        "lambdaFn {- lambda6 -} = {- lambda7 -} \\_ {- lambda8 -} -> {- lambda9 -} () {- lambda10 -}",
        "{-| trailing lambda comment -}",
        "trailingLambdaFn {- trailingLambda1 -} : {- trailingLambda2 -} Int {- trailingLambda3 -} -> {- trailingLambda4 -} () {- trailingLambda5 -}",
        "trailingLambdaFn {- trailingLambda6 -} = {- trailingLambda7 -} \\ {- trailingLambda8 -} a {- trailingLambda9 -} b {- trailingLambda10 -} -> {- trailingLambda11 -} () {- trailingLambda12 -}",
        "{-| record case pattern comment -}",
        "recordCasePatternFn {- recordCasePatternFn1 -} : {- recordCasePatternFn2 -} () {- recordCasePatternFn3 -}",
        `recordCasePatternFn {- recordCasePatternFn4 -} = {- recordCasePatternFn5 -} case {- recordCasePatternFn6 -} () {- recordCasePatternFn7 -} of
            {- recordCasePatternFn8 -}  { {- recordCasePatternFn9 -} a {- recordCasePatternFn10 -}, {- recordCasePatternFn11 -} b {- recordCasePatternFn12 -} } {- recordCasePatternFn13 -} -> ()`,
        "{-| union type with arguments comment -}",
        "type {- UnionArgs1 -} UnionTypeArgs {- UnionArgs2 -} a {- UnionArgs3 -} b {- UnionArgs4 -} c {- UnionArgs5 -} = UnionTypeArgs1 {- UnionArgs6 -} a {- UnionArgs7 -} b {- UnionArgs8 -} c {- UnionArgs9 -} d {- UnionArgs10 -} | {- UnionArgs11 -} UnionTypeArgs2 {- UnionArgs12 -} a {- UnionArgs13 -} b {- UnionArgs14 -} c {- UnionArgs15 -} d {- UnionArgs16 -}",
        "{-| parentheses comment -}",
        "parenthesesFn {- parenthesesFn1 -} : {- parenthesesFn2 -} Int {- parenthesesFn3 -}",
        "parenthesesFn {- parenthesesFn4 -} = {- parenthesesFn5 -} ({- parenthesesFn6 -} -1 {- parenthesesFn7 -}) {- parenthesesFn8 -} + {- parenthesesFn9 -} (-2 {- parenthesesFn10 -})",
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
        { title: "duplicate docs", filename: "ExposingDocs", module: { ...defaultModule, header: "module Main exposing (fn)", docs: "{-|\n@docs fn, fn\n-}", declarations: ["fn = ()"] } },
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
        { title: "multi-line signature", filename: "MultiLineSignatureDeclarations", module: { ...defaultModule, declarations: ["fn : a\n -> a", "fn = ()"] } },
        { title: "multi-line type signature", filename: "MultiLineTypeSignatureDeclarations", module: { ...defaultModule, declarations: ["fn : List\n a", "fn = ()"] } },
        { title: "multi-line qualified type signature", filename: "MultiLineQualifiedTypeSignatureDeclarations", module: { ...defaultModule, declarations: ["fn : Map.Dict\n k\n v", "fn = ()"] } },
        { title: "multi-line tuple signature", filename: "MultiLineTupleSignatureDeclarations", module: { ...defaultModule, declarations: ["fn : (a\n , b)", "fn = ()"] } },
        { title: "remove unnecessary parentheses", filename: "RemoveUnnecessaryParenthesesDeclarations", module: { ...defaultModule, declarations: ["fn = ((add) 1) 2"] } },
        { title: "argument w/ parentheses", filename: "ArgumentWithParenthesesDeclarations", module: { ...defaultModule, declarations: ["fn input = String.toInt (String.trim input)"] } },
        {
            title: "literals", filename: "LiteralDeclarations", module: {
                ...defaultModule, declarations: [
                    "decimalInt = 123",
                    "hexadecimalInt = 0xff",
                    "decimalFloat = 3.14",
                    "exponentFloat = 6.022e23",
                    // TODO "smallExponentFloat = 1e3",
                    `chars = ['a', '\\n', '\\t', '\t', '\\\\', '\\"', '\\'', '\u{2028}', '\u{2029}', 'ͷ']`,
                    `singleQuotedString = "hello world! Characters: \\n \\t \t \\\\ \\" \\' ' \u{2028} \u{2029} ͷ"`,
                    `tripleQuotedString = """multiline\nstrings\nwith 'single quotes' and \\"double quotes\\"\nCharacters: \\n \\t \t \\\\ \\" \\' ' \u{2028} \u{2029} ͷ"""`,
                ]
            }
        },
        { title: "multi-line record", filename: "MultiLineRecordDeclarations", module: { ...defaultModule, declarations: [`recordFn =\n { age = 23\n , name =\n "John"\n }`] } },
    ]],
    // UNION DECLARATIONS
    ["Union", [
        { title: "single variant", filename: "SingleTypeUnionDeclarations", module: { ...defaultModule, declarations: ["type A = A"] } },
        { title: "keep original order", filename: "KeepOriginalOrderUnionDeclarations", module: { ...defaultModule, declarations: ["type A = A | B | C"] } },
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
        { title: "port header", filename: "PortHeaderComments", module: { ...defaultModule, header: ["{- C1 -}\nport {- C2 -} module {- C3 -} Main {- C4 -} exposing {- C5 -} (..)"] } },
        { title: "manager header", filename: "ManagerHeaderComments", module: { ...defaultModule, header: ["{- C1 -}\neffect {- C2 -} module {- C3 -} Main {- C4 -} where {- C5 -} { {- C6 -} command {- C7 -} = {- C8 -} MyCmd {- C9 -} , {- C10 -} subscription {- C11 -} = {- C12 -} MySub {- C13 -} } {- C14 -} exposing {- C15 -} (..)"] } },
        { title: "header exposed", filename: "HeaderExposedComments", module: { ...defaultModule, header: ["module Main exposing ({- C1 -} A {- C2 -} ({- C3 -}..{- C4 -}), {- C5 -} B {- C6 -} ({- C7 -}..{- C8 -}), {- C9 -} C {- C10 -}, {- C11 -} fn {- C12 -})"] } },
        { title: "single-line declaration", filename: "SingleLineDeclarationComments", module: { ...defaultModule, declarations: ["-- COMMENT", "fn = ()"] } },
        { title: "infix", filename: "InfixComments", module: { ...defaultModule, infixes: ["infix {- 1 -} right {- 2 -} 0 {- 3 -} (<|) {- 4 -} = {- 5 -} apL"] } },
        {
            title: "top-level after lambda", filename: "TopLevelAfterLambdaComments", module: {
                ...defaultModule, declarations: [
                    "lambdaFn = \\_ -> ()",
                    "-- COMMENT",
                    "anotherFn = 2"
                ]
            }
        },
        {
            title: "top-level after nested lambda", filename: "TopLevelAfterNestedLambdaComments", module: {
                ...defaultModule, declarations: [
                    "pipeFn = fn <| \\_ -> ()",
                    "-- COMMENT",
                    "anotherFn = 2"
                ]
            }
        },
        {
            title: "top-level after union type", filename: "TopLevelAfterUnionTypeComments", module: {
                ...defaultModule, declarations: [
                    "type A = A",
                    "-- COMMENT",
                    "fn = 1"
                ]
            }
        },
        {
            title: "trailing top-level", filename: "TrailingTopLevelComments", module: {
                ...defaultModule, declarations: [
                    "fn = 1",
                    "-- COMMENT",
                ]
            }
        },
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
    //     console.log(`${header}
    // ${docs}
    // ${imports.join("\n")}
    // ${infixes.join("\n")}
    // ${declarations.join("\n")}`);
    return `${header}
${docs}
${imports.join("\n")}
${infixes.join("\n")}
${declarations.join("\n")}`;
}