const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");
const util = require("node:util");

const setupProject = () => {
    const tmpobj = tmp.dirSync();

    child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} init --yes`, {
        cwd: tmpobj.name,
        stdio: "pipe"
    });

    fs.writeFileSync(path.join(tmpobj.name, "src", "WarningFlags.guida"), `module WarningFlags exposing (main)

import List exposing (map)

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }

noTypeAnnotationFn unusedVar =
    1
`);

    return tmpobj;
};

const runMake = (cwd, args) => {
    const result = child_process.spawnSync(
        path.join(__dirname, "..", "bin", "index.js"),
        ["make", "src/WarningFlags.guida", "--output", "warning-flags.js", ...args],
        { cwd, encoding: "utf-8" }
    );

    return {
        status: result.status,
        stdout: util.stripVTControlCharacters(result.stdout || ""),
        stderr: util.stripVTControlCharacters(result.stderr || "")
    };
};

describe("guida make warning flags", () => {
    it("suppresses warnings with --no-warnings", () => {
        const tmpobj = setupProject();

        const result = runMake(tmpobj.name, ["--no-warnings"]);

        expect(result.status).toBe(0);
        expect(result.stderr).not.toMatch(/UNUSED IMPORT|UNUSED VARIABLE|MISSING TYPE ANNOTATION/);
        expect(fs.existsSync(path.join(tmpobj.name, "warning-flags.js"))).toBe(true);
    });

    it("fails with --deny-warnings when warnings are present", () => {
        const tmpobj = setupProject();

        const result = runMake(tmpobj.name, ["--deny-warnings"]);

        expect(result.status).toBe(1);
        expect(result.stderr).toMatch(/UNUSED IMPORT/);
        expect(result.stderr).toMatch(/UNUSED VARIABLE/);
        expect(result.stderr).toMatch(/MISSING TYPE ANNOTATION/);
    });

    it("does not write output file with --deny-warnings", () => {
        const tmpobj = setupProject();

        const result = runMake(tmpobj.name, ["--deny-warnings"]);

        expect(result.status).toBe(1);
        expect(fs.existsSync(path.join(tmpobj.name, "warning-flags.js"))).toBe(false);
    });

    it("fails when combining --no-warnings and --deny-warnings", () => {
        const tmpobj = setupProject();

        const result = runMake(tmpobj.name, ["--no-warnings", "--deny-warnings"]);

        expect(result.status).toBe(1);
        expect(result.stderr).toMatch(/CLASHING FLAGS/);
        expect(result.stderr).toMatch(/--no-warnings/);
        expect(result.stderr).toMatch(/--deny-warnings/);
    });
});
