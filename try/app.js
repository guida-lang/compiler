const guida = require("guida");

const { createFs } = require("indexeddb-fs");
const fs = createFs({ databaseName: "guida-fs" });

const config = {
    XMLHttpRequest: globalThis.XMLHttpRequest,
    env: {
        GUIDA_REGISTRY: "https://guida-package-registry.fly.dev"
    },
    writeFile: fs.writeFile,
    readFile: fs.readFile,
    details: fs.details,
    createDirectory: fs.createDirectory,
    readDirectory: fs.readDirectory,
    getCurrentDirectory: async () => "root",
    homedir: async () => "root"
};

window.addEventListener("load", async () => {
    await fs.createDirectory("root/src");
    await fs.writeFile("root/elm.json", `{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}`);

    const code = document.getElementById("code");

    const mode = document.getElementById("mode");
    const sourcemaps = document.getElementById("sourcemaps-input");
    const format = document.getElementById("format");
    const run = document.getElementById("run");

    const dependency = document.getElementById("dependency");
    const install = document.getElementById("install");
    const uninstall = document.getElementById("uninstall");

    const preview = document.getElementById("preview");

    format.addEventListener("click", async () => {
        const result = await guida.format(config, code.value);

        if (Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(JSON.parse(result.error));
        } else {
            code.value = result.output;
        }
    });

    run.addEventListener("click", async () => {
        const path = "root/src/Main.guida";
        await fs.writeFile(path, code.value);

        const result = await guida.make(config, path, {
            debug: mode.value === "debug",
            optimize: mode.value === "prod",
            sourcemaps: sourcemaps.checked
        });

        if (Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(result.error);
        } else {
            preview.srcdoc = result.output;
        }
    });

    install.addEventListener("click", async () => {
        const result = await guida.install(config, dependency.value);

        if (result && Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(result.error);
        }
    });

    uninstall.addEventListener("click", async () => {
        const result = await guida.uninstall(config, dependency.value);

        if (result && Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(result.error);
        }
    });
});