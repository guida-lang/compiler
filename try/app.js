const guida = require("guida");

window.addEventListener("load", async () => {
    const app = await guida.init({ GUIDA_REGISTRY: "/proxy/https://package.elm-lang.org" });

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
        const result = await app.format(code.value);

        if (Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(JSON.parse(result.error));
        } else {
            code.value = result.output;
        }
    });

    run.addEventListener("click", async () => {
        const result = await app.make(code.value, {
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
        const result = await app.install(dependency.value);

        if (result && Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(result.error);
        }
    });

    uninstall.addEventListener("click", async () => {
        const result = await app.uninstall(dependency.value);

        if (result && Object.prototype.hasOwnProperty.call(result, "error")) {
            console.error(result.error);
        }
    });
});