import guida from "guida";

window.addEventListener("load", async () => {
    const app = await guida.init();

    const code = document.getElementById("code");
    const mode = document.getElementById("mode");
    const sourcemaps = document.getElementById("sourcemaps-input");
    const format = document.getElementById("format");
    const run = document.getElementById("run");
    const preview = document.getElementById("preview");

    format.addEventListener("click", async () => {
        const result = await app.format(code.value);

        if (result.hasOwnProperty("error")) {
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

        if (result.hasOwnProperty("error")) {
            console.error(result.error);
        } else {
            preview.srcdoc = result.output;
        }
    });
});