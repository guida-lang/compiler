const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");

describe("guida upgrade command", () => {
    it("upgrades direct dependencies on application elm.json", () => {
        const tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(tmpobj.name, "src"));

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
                    "elm/html": "1.0.1",
                    "evancz/elm-playground": "1.0.0"
                },
                indirect: {
                    "elm/json": "1.1.4",
                    "elm/svg": "1.0.1",
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

        const output = child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} upgrade --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        }).toString("utf-8");

        const upgraded = JSON.parse(fs.readFileSync(path.join(tmpobj.name, "elm.json"), "utf-8"));

        expect(output).toContain("Planned changes:");
        expect(output).toContain("evancz/elm-playground");
        expect(upgraded.dependencies.direct["evancz/elm-playground"]).toBe("1.0.3");
    });

    it("upgrades package constraints on package elm.json", () => {
        const tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(tmpobj.name, "src"));

        fs.writeFileSync(path.join(tmpobj.name, "elm.json"), JSON.stringify({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "elm-version": "0.19.1 <= v < 1.0.0",
            dependencies: {
                "elm/core": "1.0.0 <= v < 2.0.0",
                "evancz/elm-playground": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {
                "elm/html": "1.0.0 <= v < 2.0.0"
            }
        }));

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} upgrade --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        const upgraded = JSON.parse(fs.readFileSync(path.join(tmpobj.name, "elm.json"), "utf-8"));

        expect(upgraded.dependencies["elm/core"]).toBe("1.0.5 <= v < 2.0.0");
        expect(upgraded.dependencies["evancz/elm-playground"]).toBe("1.0.3 <= v < 2.0.0");
        expect(upgraded["test-dependencies"]["elm/html"]).toBe("1.0.1 <= v < 2.0.0");
    });

    it("prints up-to-date message when nothing can be upgraded", () => {
        const tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(tmpobj.name, "src"));

        fs.writeFileSync(path.join(tmpobj.name, "guida.json"), JSON.stringify({
            type: "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            dependencies: {
                direct: {
                    "guida-lang/stdlib": "1.0.1"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }));

        const output = child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} upgrade --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        }).toString("utf-8");

        expect(output).toContain("Everything is already up to date!");
    });

    it("fails with clear message when no valid upgrade is possible", () => {
        const tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(tmpobj.name, "src"));

        fs.writeFileSync(path.join(tmpobj.name, "elm.json"), JSON.stringify({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "elm-version": "0.19.1 <= v < 1.0.0",
            dependencies: {
                "elm/core": "1.0.0 <= v < 2.0.0",
                "elm/json": "0.0.1 <= v < 1.0.0"
            },
            "test-dependencies": {}
        }));

        expect(() => {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} upgrade --yes`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        }).toThrow("CANNOT FIND COMPATIBLE UPGRADE");
    });
});
