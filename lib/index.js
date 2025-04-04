const { createFs } = require("indexeddb-fs");
const { newServer } = require("mock-xmlhttprequest");
const JSZip = require("jszip");

const savedXMLHttpRequest = globalThis.XMLHttpRequest;
const fs = createFs({ databaseName: "guida-fs" });

const runGuida = function (extraEnv, args) {
    return new Promise((resolve) => {
        let mVarsNextCounter = 0;
        const mVars = {};
        const lockedFiles = {};

        const env = Object.assign({
            GUIDA_HOME: "root/.guida",
        }, extraEnv);

        const download = function (method, url) {
            const that = this;

            const xhr = new savedXMLHttpRequest();
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

            await fs.writeFile(path, request.body);
            request.respond(200);
        });

        server.post("read", async (request) => {
            const content = await fs.readFile(request.body);
            request.respond(200, null, content);
        });


        server.post("getArchive", (request) => {
            download.apply({
                send: ({ sha, archive }) => {
                    request.respond(200, null, JSON.stringify({ sha, archive }));
                }
            }, ["GET", request.body]);
        });

        server.post("dirDoesFileExist", async (request) => {
            try {
                const stats = await fs.details(request.body);
                console.log("dirDoesFileExist", request.body, stats);
                request.respond(200, null, stats.type === "file");
            } catch (_err) {
                request.respond(200, null, false);
            }
        });

        server.post("dirCreateDirectoryIfMissing", async (request) => {
            const { createParents, filename } = JSON.parse(request.body);
            let directories = [filename];

            if (createParents) {
                directories = filename.split('/').filter(Boolean);
                directories = directories.map((_, index) => directories.slice(0, index + 1).join('/'));
            }

            await directories.reduce(async (previousPromise, directory) => {
                await previousPromise;

                try {
                    await fs.details(directory);
                } catch (_err) {
                    await fs.createDirectory(directory);
                }
            }, Promise.resolve());

            request.respond(200);
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

        server.post("dirGetModificationTime", async (request) => {
            const stats = await fs.details(request.body);
            request.respond(200, null, stats.createdAt);
        });

        server.post("dirDoesDirectoryExist", async (request) => {
            try {
                const stats = await fs.details(request.body);
                request.respond(200, null, stats.type === "directory");
            } catch (_err) {
                request.respond(200, null, false);
            }
        });

        server.post("dirCanonicalizePath", (request) => {
            request.respond(200, null, request.body);
        });

        server.post("dirListDirectory", async (request) => {
            const { files } = await fs.readDirectory(request.body);
            request.respond(200, null, JSON.stringify(files));
        });

        server.post("binaryDecodeFileOrFail", async (request) => {
            const data = await fs.readFile(request.body);
            request.respond(200, null, data.buffer);
        });

        server.post("write", async (request) => {
            const path = request.requestHeaders.getHeader("path");

            await fs.writeFile(path, request.body);
            request.respond(200);
        });

        server.post("dirGetCurrentDirectory", (request) => {
            request.respond(200, null, "root");
        });

        server.post("envLookupEnv", (request) => {
            const envVar = env[request.body] ?? null;
            request.respond(200, null, JSON.stringify(envVar));
        });

        server.post("dirGetAppUserDataDirectory", (request) => {
            request.respond(200, null, `root/.${request.body}`);
        });

        // MVARS
        server.post("newEmptyMVar", (request) => {
            mVarsNextCounter += 1;
            mVars[mVarsNextCounter] = { subscribers: [], value: undefined };
            request.respond(200, null, mVarsNextCounter);
        });

        server.post("readMVar", (request) => {
            const id = request.body;

            if (typeof mVars[id].value === "undefined") {
                mVars[id].subscribers.push({ action: "read", request });
            } else {
                request.respond(200, null, mVars[id].value.buffer);
            }
        });

        server.post("takeMVar", (request) => {
            const id = request.body;

            if (typeof mVars[id].value === "undefined") {
                mVars[id].subscribers.push({ action: "take", request });
            } else {
                const value = mVars[id].value;
                mVars[id].value = undefined;

                if (
                    mVars[id].subscribers.length > 0 &&
                    mVars[id].subscribers[0].action === "put"
                ) {
                    const subscriber = mVars[id].subscribers.shift();
                    mVars[id].value = subscriber.value;
                    request.respond(200);
                }

                request.respond(200, null, value.buffer);
            }
        });

        server.post("putMVar", (request) => {
            const id = request.requestHeaders.getHeader("id");
            const value = request.body;

            if (typeof mVars[id].value === "undefined") {
                mVars[id].value = value;

                mVars[id].subscribers = mVars[id].subscribers.filter((subscriber) => {
                    if (subscriber.action === "read") {
                        subscriber.request.respond(200, null, value.buffer);
                    }

                    return subscriber.action !== "read";
                });

                const subscriber = mVars[id].subscribers.shift();

                if (subscriber) {
                    subscriber.request.respond(200, null, value.buffer);

                    if (subscriber.action === "take") {
                        mVars[id].value = undefined;
                    }
                }

                request.respond(200);
            } else {
                mVars[id].subscribers.push({ action: "put", request, value });
            }
        });

        // BROWSER
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
            console.log("defaultHandler", request.url);

            const headers = request.requestHeaders.getHash();

            var xhr = new savedXMLHttpRequest();
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

        Elm.Browser.Main.init();
    });
}

const elmJsonContent = `{
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
}`;

module.exports = {
    init: async (extraEnv) => {
        await fs.writeFile("root/elm.json", elmJsonContent);
        await fs.createDirectory("root/src");

        return {
            make: async (content, options) => {
                await fs.writeFile("root/src/Main.guida", content);

                return await runGuida(extraEnv, {
                    command: "make",
                    path: "src/Main.guida",
                    debug: !!options.debug,
                    optimize: !!options.optimize,
                    sourcemaps: !!options.sourcemaps
                });
            },
            format: async (content) => {
                return await runGuida(extraEnv, { command: "format", content });
            },
            install: async (pkg) => {
                return await runGuida(extraEnv, { command: "install", pkg });
            },
            uninstall: async (pkg) => {
                return await runGuida(extraEnv, { command: "uninstall", pkg });
            }
        };
    }
};