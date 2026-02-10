const { newServer } = require("mock-xmlhttprequest");
const JSZip = require("jszip");

const runGuida = function (config, args) {
    return new Promise((resolve) => {
        let mVarsNextCounter = 0;
        const mVars = {};
        const lockedFiles = {};

        const download = function (method, url) {
            fetch(url, { method }).then((response) => {
                if (response.ok) {
                    return response.arrayBuffer().then((buffer) => {
                        return crypto.subtle.digest("SHA-1", buffer).then((hashBuffer) => {
                            const sha = Array.from(new Uint8Array(hashBuffer)).map(byte => byte.toString(16).padStart(2, "0")).join("");

                            const jsZip = new JSZip();
                            jsZip.loadAsync(buffer).then((zip) => {
                                const archive = [];

                                Promise.all(Object.entries(zip.files).map(async ([_, file]) => {
                                    return file.async("text").then((eData) => {
                                        archive.push({
                                            eRelativePath: file.name,
                                            eData
                                        });
                                    });
                                })).then(() => {
                                    this.send({ sha, archive });
                                });
                            });
                        });
                    });
                } else if (response.headers.get("location")) {
                    download.apply(this, [method, response.headers.get("location")]);
                }
            }).catch(() => {
                console.error("Network error during ZIP file download.");
            });
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
            const data = await config.readFile(request.body);
            request.respond(200, null, data.toString());
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

        server.post("dirDoesFileExist", async (request) => {
            try {
                const stats = await config.details(request.body);
                request.respond(200, null, stats.type === "file");
            } catch (_err) {
                request.respond(200, null, false);
            }
        });

        server.post("dirCreateDirectoryIfMissing", async (request) => {
            const { createParents, filename } = JSON.parse(request.body);
            let directories = [filename];
            let prefix = filename.startsWith("/") ? "/" : "";

            if (createParents) {
                directories = filename.split('/').filter(Boolean);
                directories = directories.map((_, index) => prefix + directories.slice(0, index + 1).join('/'));
            }

            await directories.reduce(async (previousPromise, directory) => {
                await previousPromise;

                try {
                    await config.details(directory);
                } catch (_err) {
                    await config.createDirectory(directory);
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
            const stats = await config.details(request.body);
            request.respond(200, null, stats.createdAt);
        });

        server.post("dirDoesDirectoryExist", async (request) => {
            try {
                const stats = await config.details(request.body);
                request.respond(200, null, stats.type === "directory");
            } catch (_err) {
                request.respond(200, null, false);
            }
        });

        server.post("dirCanonicalizePath", (request) => {
            request.respond(200, null, request.body);
        });

        server.post("dirListDirectory", async (request) => {
            const { files } = await config.readDirectory(request.body);
            request.respond(200, null, JSON.stringify(files));
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

        server.post("dirGetCurrentDirectory", async (request) => {
            const currentDir = await config.getCurrentDirectory();
            request.respond(200, null, currentDir);
        });

        server.post("envLookupEnv", (request) => {
            const envVar = config.env[request.body] ?? null;
            request.respond(200, null, JSON.stringify(envVar));
        });

        server.post("dirGetAppUserDataDirectory", async (request) => {
            const homedir = await config.homedir();
            request.respond(200, null, `${homedir}/.${request.body}`);
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

            fetch(request.url, {
                method: request.method,
                headers: headers,
                body: request.body
            }).then((response) => {
                return response.text().then((text) => {
                    request.respond(200, null, text);
                });
            }).catch((err) => {
                console.error("Fetch error:", err);
                request.respond(0);
            });
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
    make: async (config, path, options = {}) => {
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