const { newServer } = require("mock-xmlhttprequest");
const JSZip = require("jszip");

const runGuida = function (config, args) {
    return new Promise((resolve) => {
        const lockedFiles = {};

        const download = function (method, url) {
            const that = this;

            const xhr = new config.XMLHttpRequest();
            xhr.open(method, url, true);
            xhr.responseType = "arraybuffer";

            xhr.onload = async () => {
                const headers = xhr.getAllResponseHeaders().trim().split(/[\r\n]+/).reduce(function (acc, line) {
                    const parts = line.split(": ");
                    const header = parts.shift();
                    const value = parts.join(": ");
                    acc[header] = value;
                    return acc;
                }, {});

                if (xhr.status >= 200 && xhr.status < 300) {
                    const hashBuffer = await crypto.subtle.digest("SHA-1", xhr.response);
                    const sha = Array.from(new Uint8Array(hashBuffer)).map(byte => byte.toString(16).padStart(2, "0")).join("");

                    const jsZip = new JSZip();
                    jsZip.loadAsync(xhr.response).then(function async(zip) {
                        const archive = [];

                        Promise.all(Object.entries(zip.files).map(async ([_, file]) => {
                            return file.async("text").then((eData) => {
                                archive.push({
                                    eRelativePath: file.name,
                                    eData
                                });
                            });
                        })).then(() => {
                            that.send({ sha, archive });
                        });
                    });
                } else if (headers.location) {
                    download.apply(this, [method, headers.location]);
                }
            };

            xhr.onerror = function () {
                console.error("Network error during ZIP file download.");
            };

            xhr.ontimeout = function () {
                console.error("ZIP file download timed out.");
            };

            xhr.send();
        };

        const server = newServer();

        server.post("hPutStr", (request) => {
            const fd = parseInt(request.requestHeaders.getHeader("fd"));

            if (fd === 1) {
                console.log(request.body);
            } else if (fd === 2) {
                console.error(request.body);
            } else {
                throw new Error(`Invalid file descriptor: ${fd}`);
            }

            request.respond(200);
        });

        server.post("writeString", async (request) => {
            const path = request.requestHeaders.getHeader("path");

            await config.writeFile(path, request.body);
            request.respond(200);
        });

        server.post("read", async (request) => {
            const content = await config.readFile(request.body);
            request.respond(200, null, content);
        });

        server.post("getArchive", (request) => {
            download.apply({
                send: ({ sha, archive }) => {
                    request.respond(200, null, JSON.stringify({ sha, archive }));
                }
            }, ["GET", request.body]);
        });

        server.post("exitWith", (request) => {
            console.log(`Exited with ${request.body}`);
        });

        server.post("lockFile", (request) => {
            const path = request.body;

            if (lockedFiles[path]) {
                lockedFiles[path].subscribers.push(request);
            } else {
                lockedFiles[path] = { subscribers: [] };
                request.respond(200);
            }
        });

        server.post("unlockFile", (request) => {
            const path = request.body;

            if (lockedFiles[path]) {
                const subscriber = lockedFiles[path].subscribers.shift();

                if (subscriber) {
                    subscriber.respond(200);
                } else {
                    delete lockedFiles[path];
                }

                request.respond(200);
            } else {
                console.error(`Could not find locked file "${path}"!`);
            }
        });

        server.post("binaryDecodeFileOrFail", async (request) => {
            const data = await config.readFile(request.body);
            request.respond(200, null, data.buffer);
        });

        server.post("write", async (request) => {
            const path = request.requestHeaders.getHeader("path");

            await config.writeFile(path, request.body);
            request.respond(200);
        });

        server.post("envLookupEnv", (request) => {
            const envVar = config.env[request.body] ?? null;
            request.respond(200, null, JSON.stringify(envVar));
        });

        // API
        server.post("getArgs", (request) => {
            request.respond(200, null, JSON.stringify(args));
        });

        server.post("exitWithResponse", (request) => {
            resolve(JSON.parse(request.body));
        });

        // Catch non-implemented functionality
        server.post(/^\w+$/, (request) => {
            throw new Error(`${request.url} handler not implemented!`);
        });

        server.setDefaultHandler((request) => {
            const headers = request.requestHeaders.getHash();

            var xhr = new config.XMLHttpRequest();
            xhr.open(request.method, request.url, true);

            for (const key in headers) {
                if (Object.prototype.hasOwnProperty.call(headers, key) && key !== "user-agent") {
                    xhr.setRequestHeader(key, headers[key]);
                }
            }

            xhr.onload = function () {
                request.respond(200, null, this.responseText);
            };

            xhr.send(request.body);
        });

        server.install();

        const { Elm } = require("./guida.min.js");

        Elm.API.Main.init();
    });
};

module.exports = {
    init: async (config, options) => {
        return await runGuida(config, {
            command: "init",
            package: !!options.package
        });
    },
    make: async (config, path, options) => {
        return await runGuida(config, {
            command: "make",
            path,
            debug: !!options.debug,
            optimize: !!options.optimize,
            sourcemaps: !!options.sourcemaps
        });
    },
    format: async (config, content) => {
        return await runGuida(config, { command: "format", content });
    },
    install: async (config, pkg) => {
        return await runGuida(config, { command: "install", pkg });
    },
    uninstall: async (config, pkg) => {
        return await runGuida(config, { command: "uninstall", pkg });
    },
    diagnostics: async (config, args) => {
        return await runGuida(config, { command: "diagnostics", ...args });
    },
    getDefinitionLocation: async (config, args) => {
        return await runGuida(config, { command: "get-definition-location", ...args });
    }
};