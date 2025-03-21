const { createFs } = require("indexeddb-fs");
const { newServer } = require("mock-xmlhttprequest");
const JSZip = require("jszip");

const savedXMLHttpRequest = globalThis.XMLHttpRequest;
const fs = createFs({ databaseName: "guida-fs" });

// const rl = readline.createInterface({
//     input: process.stdin,
//     output: process.stdout,
// });

let nextCounter = 0, mVarsNextCounter = 0;
let stateT = { imports: {}, types: {}, decls: {} };
const mVars = {};
const lockedFiles = {};
const processes = {};

const env = {
    GUIDA_HOME: "root/.guida",
};

const download = function (method, url) {
    const that = this;

    const xhr = new savedXMLHttpRequest();
    xhr.open(method, `/proxy/${url}`, true);
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

server.post("getLine", (request) => {
    console.error("getLine", request);

    // rl.on("line", (value) => {
    //     request.respond(200, null, value);
    // });
});

server.post("hPutStr", (request) => {
    const fd = parseInt(request.requestHeaders.getHeader("fd"));

    if (fd === 1) {
        console.log(request.body);
    } else if (fd === 2) {
        console.error(request.body);
    } else {
        throw new Error(`Invalid file descriptor: ${fd}`);
    }
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

server.post("readStdin", (request) => {
    console.error("readStdin", request);

    // fs.readFile(0, (err, data) => {
    //     if (err) throw err;
    //     request.respond(200, null, data.toString());
    // });
});

server.post("getArchive", (request) => {
    download.apply({
        send: ({ sha, archive }) => {
            request.respond(200, null, JSON.stringify({ sha, archive }));
        }
    }, ["GET", request.body]);
});

server.post("httpUpload", (request) => {
    console.error("httpUpload", request);

    // const { urlStr, headers, parts } = JSON.parse(request.body);
    // const url = new URL(urlStr);
    // const client = url.protocol == "https:" ? https : http;

    // const form = new FormData();

    // parts.forEach((part) => {
    //     switch (part.type) {
    //         case "FilePart":
    //             form.append(part.name, fs.createReadStream(part.filePath));
    //             break;

    //         case "JsonPart":
    //             form.append(part.name, JSON.stringify(part.value), {
    //                 contentType: "application/json",
    //                 filepath: part.filePath,
    //             });
    //             break;

    //         case "StringPart":
    //             form.append(part.name, part.string);
    //             break;
    //     }
    // });

    // const req = client.request(url, {
    //     method: "POST",
    //     headers: { ...headers, ...form.getHeaders() },
    // });

    // form.pipe(req);

    // req.on("response", (res) => {
    //     res.on("end", () => {
    //         request.respond(200);
    //     });
    // });

    // req.on("error", (err) => {
    //     throw err;
    // });
});

server.post("withFile", (request) => {
    console.error("withFile", request);

    // const mode = request.requestHeaders.getHeader("mode");

    // fs.open(request.body, mode, (err, fd) => {
    //     if (err) throw err;
    //     request.respond(200, null, fd);
    // });
});

server.post("hFileSize", (request) => {
    console.error("hFileSize", request);

    // fs.fstat(request.body, (err, stats) => {
    //     if (err) throw err;
    //     request.respond(200, null, stats.size);
    // });
});

server.post("withCreateProcess", (request) => {
    console.error("withCreateProcess", request);

    // let createProcess = JSON.parse(request.body);

    // tmp.file((err, path, fd, _cleanupCallback) => {
    //     if (err) throw err;

    //     const reader = fs.createReadStream(path);

    //     reader.on("open", (_fd) => {
    //         nextCounter += 1;
    //         processes[nextCounter] = child_process.spawn(
    //             createProcess.cmdspec.cmd,
    //             createProcess.cmdspec.args,
    //             {
    //                 stdio: [
    //                     createProcess.stdin,
    //                     createProcess.stdout,
    //                     createProcess.stderr,
    //                 ],
    //             }
    //         );

    //         request.respond(200, null, JSON.stringify({ stdinHandle: fd, ph: nextCounter }));
    //     });

    //     reader.on("data", (chunk) => {
    //         processes[nextCounter].stdin.end(chunk);
    //     });
    // });
});

server.post("hClose", (request) => {
    console.error("hClose", request);

    // const fd = parseInt(request.body);
    // fs.close(fd);
    // request.respond(200);
});

server.post("waitForProcess", (request) => {
    console.error("waitForProcess", request);

    // processes[request.body].on("exit", (code) => {
    //     request.respond(200, null, code);
    // });
});

server.post("exitWith", (request) => {
    console.error("exitWith", request);

    // rl.close();
    // process.exit(request.body);
});

server.post("dirFindExecutable", (request) => {
    console.error("dirFindExecutable", request);

    // const path = which.sync(request.body, { nothrow: true }) ?? null;
    // request.respond(200, null, JSON.stringify(path));
});

server.post("replGetInputLine", (request) => {
    console.error("replGetInputLine", request);

    // rl.question(request.body, (value) => {
    //     request.respond(200, null, JSON.stringify(value));
    // });
});

server.post("dirDoesFileExist", async (request) => {
    try {
        const stats = await fs.details(request.body);
        request.respond(200, null, stats.type === "file");
    } catch (error) {
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
        } catch (error) {
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
        rl.close();
        process.exit(255);
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
    } catch (error) {
        request.respond(200, null, false);
    }
});

server.post("dirCanonicalizePath", (request) => {
    request.respond(200, null, request.body);
});

server.post("dirListDirectory", (request) => {
    console.error("dirListDirectory", request);

    // fs.readdir(request.body, { recursive: false }, (err, files) => {
    //     if (err) throw err;
    //     request.respond(200, null, JSON.stringify(files));
    // });
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

server.post("dirRemoveFile", (request) => {
    console.error("dirRemoveFile", request);

    // fs.unlink(request.body, (err) => {
    //     if (err) throw err;
    //     request.respond(200);
    // });
});

server.post("dirRemoveDirectoryRecursive", (request) => {
    console.error("dirRemoveDirectoryRecursive", request);

    // fs.rm(request.body, { recursive: true, force: true }, (err) => {
    //     if (err) throw err;
    //     request.respond(200);
    // });
});

server.post("dirWithCurrentDirectory", (request) => {
    console.error("dirWithCurrentDirectory", request);

    // try {
    //     process.chdir(request.body);
    //     request.respond(200);
    // } catch (err) {
    //     console.error(`chdir: ${err}`);
    // }
});

server.post("envGetArgs", (request) => {
    // request.respond(200, null, JSON.stringify(process.argv.slice(2)));
    // request.respond(200, null, JSON.stringify(["init", "--yes"]));
    request.respond(200, null, JSON.stringify(["make", "src/Main.elm"]));
});

server.post("dirGetCurrentDirectory", (request) => {
    request.respond(200, null, "root");
});

server.post("envLookupEnv", (request) => {
    const envVar = env[request.body] ?? null; // process.env[request.body] ?? null;
    request.respond(200, null, JSON.stringify(envVar));
});

server.post("dirGetAppUserDataDirectory", (request) => {
    request.respond(200, null, `root/.${request.body}`);
});

server.post("putStateT", (request) => {
    console.error("putStateT", request);

    // stateT = request.body;
    // request.respond(200);
});

server.post("getStateT", (request) => {
    console.error("getStateT", request);

    // request.respond(200, null, stateT.buffer);
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

server.setDefaultHandler((request) => {
    console.log("defaultHandler", request.url);

    const headers = request.requestHeaders.getHash();

    var xhr = new savedXMLHttpRequest();
    xhr.open(request.method, `/proxy/${request.url}`, true);

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

fs.createDirectory("root/src").then(() => {
    const elmJson = fs.writeFile("root/elm.json", `{
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
    const mainElm = fs.writeFile("root/src/Main.elm", `module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]`);

    Promise.all([elmJson, mainElm]).then(() => {
        const { Elm } = require("../bin/guida.js");

        Elm.Terminal.Main.init();
    });
});

fs.readFile("root/index.html").then((content) => {
    const iframe = document.createElement("iframe");
    iframe.style.width = "100%";
    iframe.style.height = "100%";
    iframe.srcdoc = content;
    document.body.appendChild(iframe);
});