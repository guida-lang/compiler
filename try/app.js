import guida from "guida";

window.addEventListener("load", async () => {
    const app = await guida.init();

    const code = document.getElementById("code");
    const run = document.getElementById("run");
    const preview = document.getElementById("preview");

    run.addEventListener("click", async () => {
        const result = await app.make(code.value);

        if (result.hasOwnProperty("error")) {
            console.error(JSON.parse(result.error));
        } else {
            preview.srcdoc = result.output;
        }
    });
});