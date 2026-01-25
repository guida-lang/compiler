const guida = require("guida");

const { createFs } = require("indexeddb-fs");
const fs = createFs({ databaseName: "guida-fs" });

const config = {
    XMLHttpRequest: globalThis.XMLHttpRequest,
    env: {},
    writeFile: fs.writeFile,
    readFile: fs.readFile,
    details: fs.details,
    createDirectory: fs.createDirectory,
    readDirectory: fs.readDirectory,
    getCurrentDirectory: async () => "root",
    homedir: async () => "root"
};

const defaultGuidaJson = `{
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
}`;

const defaultElmJson = `{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.1"
        },
        "indirect": {
            "elm/json": "1.1.4",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.5"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}`;

window.addEventListener("load", async () => {
    await fs.createDirectory("root/src");
    await fs.writeFile("root/guida.json", defaultGuidaJson);

    const code = document.getElementById("code");

    const guidaJson = document.getElementById("guida-json");
    const elmJson = document.getElementById("elm-json");

    const jsonConfig = document.getElementById("json-config");

    const mode = document.getElementById("mode");
    const sourcemaps = document.getElementById("sourcemaps-input");
    const format = document.getElementById("format");
    const run = document.getElementById("run");

    const dependency = document.getElementById("dependency");
    const install = document.getElementById("install");
    const uninstall = document.getElementById("uninstall");

    const preview = document.getElementById("preview");

    let jsonConfigValue = "guida";

    const updateJsonConfig = async () => {
        let content;

        if (jsonConfigValue === "guida") {
            content = await fs.readFile("root/guida.json");
        } else {
            content = await fs.readFile("root/elm.json");
        }

        jsonConfig.value = content;
    };

    guidaJson.addEventListener("input", async () => {
        jsonConfigValue = "guida";
        await fs.removeFile("root/elm.json");
        await fs.writeFile("root/guida.json", defaultGuidaJson);
        await updateJsonConfig();
    });

    elmJson.addEventListener("input", async () => {
        jsonConfigValue = "elm";
        await fs.removeFile("root/guida.json");
        await fs.writeFile("root/elm.json", defaultElmJson);
        await updateJsonConfig();
    });

    format.addEventListener("click", async () => {
        const result = await guida.format(config, code.value);

        if (Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(JSON.parse(result.error));
        } else {
            code.value = result.output;
        }
    });

    run.addEventListener("click", async () => {
        let path;

        if (jsonConfigValue === "guida") {
            path = "root/src/Main.guida";

            if (await fs.exists("root/src/Main.elm")) {
                await fs.removeFile("root/src/Main.elm");
            }
            await fs.writeFile("root/guida.json", jsonConfig.value);
        } else {
            path = "root/src/Main.elm";

            if (await fs.exists("root/src/Main.guida")) {
                await fs.removeFile("root/src/Main.guida");
            }

            await fs.writeFile("root/elm.json", jsonConfig.value);
        }

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

        updateJsonConfig();
    });

    uninstall.addEventListener("click", async () => {
        const result = await guida.uninstall(config, dependency.value);

        if (result && Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(result.error);
        }

        updateJsonConfig();
    });
});