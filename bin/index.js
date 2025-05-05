#!/usr/bin/env node

const fs = require("node:fs");
const child_process = require("node:child_process");
const readline = require("node:readline");
const os = require("node:os");
const http = require("node:http");
const https = require("node:https");
const resolve = require("node:path").resolve;
const zlib = require("node:zlib");
const crypto = require("node:crypto");
const AdmZip = require("adm-zip");
const which = require("which");
const tmp = require("tmp");
const FormData = require("form-data");
const { newServer } = require("mock-xmlhttprequest");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

let nextCounter = 0, mVarsNextCounter = 0;
let stateT = { imports: {}, types: {}, decls: {} };
const mVars = {};
const lockedFiles = {};
const processes = {};

const download = function (method, url) {
  const req = https.request(url, { method }, (res) => {
    if (res.statusCode >= 200 && res.statusCode < 300) {
      let chunks = [];

      res.on("data", (chunk) => {
        chunks.push(chunk);
      });

      res.on("end", () => {
        const buffer = Buffer.concat(chunks);
        const zip = new AdmZip(buffer);

        const sha = crypto.createHash("sha1").update(buffer).digest("hex");

        const archive = zip.getEntries().map(function (entry) {
          return {
            eRelativePath: entry.entryName,
            eData: zip.readAsText(entry),
          };
        });

        this.send({ sha, archive });
      });
    } else if (res.headers.location) {
      download.apply(this, [method, res.headers.location]);
    }
  });

  req.on("error", (e) => {
    console.error(e);
  });

  req.end();
};

const server = newServer();

server.post("getLine", (request) => {
  rl.on("line", (value) => {
    request.respond(200, null, value);
  });
});

server.post("hPutStr", (request) => {
  const fd = parseInt(request.requestHeaders.getHeader("fd"));

  fs.write(fd, request.body, (err) => {
    if (err) throw err;
    request.respond(200);
  });
});

server.post("writeString", (request) => {
  const path = request.requestHeaders.getHeader("path");

  fs.writeFile(path, request.body, (err) => {
    if (err) throw err;
    request.respond(200);
  });
});

server.post("read", (request) => {
  fs.readFile(request.body, (err, data) => {
    if (err) throw err;
    request.respond(200, null, data.toString());
  });
});

server.post("readStdin", (request) => {
  fs.readFile(0, (err, data) => {
    if (err) throw err;
    request.respond(200, null, data.toString());
  });
});

server.post("getArchive", (request) => {
  download.apply({
    send: ({ sha, archive }) => {
      request.respond(200, null, JSON.stringify({ sha, archive }));
    }
  }, ["GET", request.body]);
});

server.post("httpUpload", (request) => {
  const { urlStr, headers, parts } = JSON.parse(request.body);
  const url = new URL(urlStr);
  const client = url.protocol == "https:" ? https : http;

  const form = new FormData();

  parts.forEach((part) => {
    switch (part.type) {
      case "FilePart":
        form.append(part.name, fs.createReadStream(part.filePath));
        break;

      case "JsonPart":
        form.append(part.name, JSON.stringify(part.value), {
          contentType: "application/json",
          filepath: part.filePath,
        });
        break;

      case "StringPart":
        form.append(part.name, part.string);
        break;
    }
  });

  const req = client.request(url, {
    method: "POST",
    headers: { ...headers, ...form.getHeaders() },
  });

  form.pipe(req);

  req.on("response", (res) => {
    res.on("end", () => {
      request.respond(200);
    });
  });

  req.on("error", (err) => {
    throw err;
  });
});

server.post("withFile", (request) => {
  const mode = request.requestHeaders.getHeader("mode");

  fs.open(request.body, mode, (err, fd) => {
    if (err) throw err;
    request.respond(200, null, fd);
  });
});

server.post("hFileSize", (request) => {
  fs.fstat(request.body, (err, stats) => {
    if (err) throw err;
    request.respond(200, null, stats.size);
  });
});

server.post("withCreateProcess", (request) => {
  let createProcess = JSON.parse(request.body);

  tmp.file((err, path, fd) => {
    if (err) throw err;

    nextCounter += 1;

    fs.createReadStream(path)
      .on("data", (chunk) => {
        processes[nextCounter].stdin.write(chunk);
      })
      .on("close", () => {
        processes[nextCounter].stdin.end();
      });

    processes[nextCounter] = child_process.spawn(
      createProcess.cmdspec.cmd,
      createProcess.cmdspec.args,
      {
        stdio: [
          createProcess.stdin,
          createProcess.stdout,
          createProcess.stderr,
        ],
      }
    );

    request.respond(200, null, JSON.stringify({ stdinHandle: fd, ph: nextCounter }));
  });
});

server.post("hClose", (request) => {
  const fd = parseInt(request.body);
  fs.close(fd);
  request.respond(200);
});

server.post("waitForProcess", (request) => {
  const ph = parseInt(request.body);
  processes[ph].on("exit", (code) => {
    request.respond(200, null, code);
  });
});

server.post("exitWith", (request) => {
  rl.close();
  process.exit(request.body);
});

server.post("dirFindExecutable", (request) => {
  const path = which.sync(request.body, { nothrow: true }) ?? null;
  request.respond(200, null, JSON.stringify(path));
});

server.post("replGetInputLine", (request) => {
  rl.question(request.body, (value) => {
    request.respond(200, null, JSON.stringify(value));
  });
});

server.post("dirDoesFileExist", (request) => {
  fs.stat(request.body, (err, stats) => {
    request.respond(200, null, !err && stats.isFile());
  });
});

server.post("dirCreateDirectoryIfMissing", (request) => {
  const { createParents, filename } = JSON.parse(request.body);
  fs.mkdir(filename, { recursive: createParents }, (_err) => {
    request.respond(200);
  });
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

server.post("dirGetModificationTime", (request) => {
  fs.stat(request.body, (err, stats) => {
    if (err) throw err;
    request.respond(200, null, parseInt(stats.mtimeMs, 10));
  });
});

server.post("dirDoesDirectoryExist", (request) => {
  fs.stat(request.body, (err, stats) => {
    request.respond(200, null, !err && stats.isDirectory());
  });
});

server.post("dirCanonicalizePath", (request) => {
  request.respond(200, null, resolve(request.body));
});

server.post("dirListDirectory", (request) => {
  fs.readdir(request.body, { recursive: false }, (err, files) => {
    if (err) throw err;
    request.respond(200, null, JSON.stringify(files));
  });
});

server.post("binaryDecodeFileOrFail", (request) => {
  fs.readFile(request.body, (err, data) => {
    if (err) throw err;
    request.respond(200, null, data.buffer);
  });
});

server.post("write", (request) => {
  const path = request.requestHeaders.getHeader("path");

  fs.writeFile(path, request.body, (err) => {
    if (err) throw err;
    request.respond(200);
  });
});

server.post("dirRemoveFile", (request) => {
  fs.unlink(request.body, (err) => {
    if (err) throw err;
    request.respond(200);
  });
});

server.post("dirRemoveDirectoryRecursive", (request) => {
  fs.rm(request.body, { recursive: true, force: true }, (err) => {
    if (err) throw err;
    request.respond(200);
  });
});

server.post("dirWithCurrentDirectory", (request) => {
  try {
    process.chdir(request.body);
    request.respond(200);
  } catch (err) {
    console.error(`chdir: ${err}`);
  }
});

server.post("envGetArgs", (request) => {
  request.respond(200, null, JSON.stringify(process.argv.slice(2)));
});

server.post("dirGetCurrentDirectory", (request) => {
  request.respond(200, null, process.cwd());
});

server.post("envLookupEnv", (request) => {
  const envVar = process.env[request.body] ?? null;
  request.respond(200, null, JSON.stringify(envVar));
});

server.post("dirGetAppUserDataDirectory", (request) => {
  request.respond(200, null, `${os.homedir()}/.${request.body}`);
});

server.post("putStateT", (request) => {
  stateT = request.body;
  request.respond(200);
});

server.post("getStateT", (request) => {
  request.respond(200, null, stateT.buffer);
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

// NODE.JS SPECIFIC
server.post("nodeGetDirname", (request) => {
  request.respond(200, null, __dirname);
});

server.post("nodeMathRandom", (request) => {
  request.respond(200, null, Math.random());
});

server.setDefaultHandler((request) => {
  const url = new URL(request.url);
  const client = url.protocol == "https:" ? https : http;

  const req = client.request(url, {
    method: request.method,
    headers: request.requestHeaders
  }, (res) => {
    let chunks = [];

    res.on("data", (chunk) => {
      chunks.push(chunk);
    });

    res.on("end", () => {
      const buffer = Buffer.concat(chunks);
      const encoding = res.headers["content-encoding"];

      if (encoding == "gzip") {
        zlib.gunzip(buffer, (err, decoded) => {
          if (err) throw err;
          request.respond(200, null, decoded && decoded.toString());
        });
      } else if (encoding == "deflate") {
        zlib.inflate(buffer, (err, decoded) => {
          if (err) throw err;
          request.respond(200, null, decoded && decoded.toString());
        });
      } else {
        request.respond(200, null, buffer.toString());
      }
    });
  });

  req.on("error", (err) => {
    throw err;
  });

  req.end();
});

server.install();

const { Elm } = require("./guida.min.js");

Elm.Terminal.Main.init();
