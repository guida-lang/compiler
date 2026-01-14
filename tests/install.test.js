const fs = require("node:fs");
const path = require("node:path");
const child_process = require("node:child_process");
const tmp = require("tmp");

describe("guida install command", () => {
    it("installs elm package on application elm.json", () => {
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

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install evancz/elm-playground --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "elm.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "elm.json"), "utf-8"))).toEqual({
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
                    "evancz/elm-playground": "1.0.3"
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
        });
    });

    it("installs elm package on package elm.json", () => {
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
                "elm/core": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {}
        }));

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install evancz/elm-playground --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "elm.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "elm.json"), "utf-8"))).toEqual({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "elm-version": "0.19.1 <= v < 1.0.0",
            dependencies: {
                "elm/core": "1.0.0 <= v < 2.0.0",
                "evancz/elm-playground": "1.0.3 <= v < 2.0.0"
            },
            "test-dependencies": {}
        });
    });

    it("fails when trying to install guida package on application elm.json", () => {
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

        expect(() => {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install guida-lang/project-metadata-utils --yes`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        }).toThrow("UNKNOWN PACKAGE");
    });

    it("fails when trying to install guida package on package elm.json", () => {
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
                "elm/core": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {}
        }));

        expect(() => {
            child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install guida-lang/project-metadata-utils --yes`, {
                cwd: tmpobj.name,
                stdio: "pipe"
            });
        }).toThrow("UNKNOWN PACKAGE");
    });

    it("installs elm package on application guida.json", () => {
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
                    "guida-lang/stdlib": "1.0.0"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }));

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install evancz/elm-playground --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            type: "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            dependencies: {
                direct: {
                    "guida-lang/stdlib": "1.0.0",
                    "evancz/elm-playground": "1.0.3"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        });
    });

    it("installs elm package on package guida.json", () => {
        const tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(tmpobj.name, "src"));

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

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install evancz/elm-playground --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "guida-version": "1.0.0 <= v < 2.0.0",
            dependencies: {
                "guida-lang/stdlib": "1.0.0 <= v < 2.0.0",
                "evancz/elm-playground": "1.0.3 <= v < 2.0.0"
            },
            "test-dependencies": {}
        });
    });

    it("installs guida package on application guida.json", () => {
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
                    "guida-lang/stdlib": "1.0.0"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }));

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install guida-lang/project-metadata-utils --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            type: "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            dependencies: {
                direct: {
                    "guida-lang/stdlib": "1.0.0",
                    "guida-lang/project-metadata-utils": "1.0.0"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        });
    });

    it("installs guida package on package guida.json", () => {
        const tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(tmpobj.name, "src"));

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

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install guida-lang/project-metadata-utils --yes`, {
            cwd: tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            type: "package",
            name: "author/project",
            summary: "helpful summary of your project, less than 80 characters",
            license: "BSD-3-Clause",
            version: "1.0.0",
            "exposed-modules": [],
            "guida-version": "1.0.0 <= v < 2.0.0",
            dependencies: {
                "guida-lang/stdlib": "1.0.0 <= v < 2.0.0",
                "guida-lang/project-metadata-utils": "1.0.0 <= v < 2.0.0"
            },
            "test-dependencies": {}
        });
    });

    it("installs elm package on elm.json and guida package on guida.json, with same GUIDA_HOME", () => {
        // ELM
        const elm_tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(elm_tmpobj.name, "src"));

        fs.writeFileSync(path.join(elm_tmpobj.name, "elm.json"), JSON.stringify({
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

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install evancz/elm-playground --yes`, {
            cwd: elm_tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(elm_tmpobj.name, "elm.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(elm_tmpobj.name, "elm.json"), "utf-8"))).toEqual({
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
                    "evancz/elm-playground": "1.0.3"
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
        });

        // GUIDA
        const guida_tmpobj = tmp.dirSync();

        fs.mkdirSync(path.join(guida_tmpobj.name, "src"));

        fs.writeFileSync(path.join(guida_tmpobj.name, "guida.json"), JSON.stringify({
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

        child_process.execSync(`${path.join(__dirname, "..", "bin", "index.js")} install evancz/elm-playground --yes`, {
            cwd: guida_tmpobj.name,
            stdio: "pipe"
        });

        expect(fs.existsSync(path.join(guida_tmpobj.name, "guida.json"))).toBe(true);
        expect(JSON.parse(fs.readFileSync(path.join(guida_tmpobj.name, "guida.json"), "utf-8"))).toEqual({
            type: "application",
            "source-directories": [
                "src"
            ],
            "guida-version": "1.0.0",
            dependencies: {
                direct: {
                    "guida-lang/stdlib": "1.0.0",
                    "evancz/elm-playground": "1.0.3"
                },
                indirect: {}
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        });
    });
});
