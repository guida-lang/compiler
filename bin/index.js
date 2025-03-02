#!/usr/bin/env node

const { newMockXhr } = require('mock-xmlhttprequest');
const MockXhr = newMockXhr();

const handlers = {
  getLine: (request) => {
  rl.on("line", (value) => {
    request.respond(200, {}, value);
  });
  },
  hPutStr: (request) => {
      const { fd, content } = JSON.parse(request.body);
      fs.write(fd, content, (err) => {
        if (err) throw err;
      request.respond(200);
      });
  },
  writeString: (request) => {
    let { path, content } = JSON.parse(request.body);
    fs.writeFile(path, content, (err) => {
      if (err) throw err;
      request.respond(200);
    });
  },
  read: (request) => {
    fs.readFile(request.body, (err, data) => {
      if (err) throw err;
      request.respond(200, null, data.toString());
    });
  },
  readStdin: (request) => {
    fs.readFile(0, (err, data) => {
      if (err) throw err;
      request.respond(200, null, data.toString());
    });
  },
  getArchive: (request) => {
    download.apply({
      send: ({ sha, archive }) => {
        request.respond(200, null, JSON.stringify({ sha, archive }));
      }
    },
      // FIXME hardcoded index 0 
      [0, "GET", request.body]);
  },
  httpUpload: (request) => {
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
  },
  withFile: (request) => {
    let { filename, mode } = JSON.parse(request.body);
    fs.open(filename, mode, (err, fd) => {
      if (err) throw err;
      request.respond(200, null, fd);
    });
  },
  hFileSize: (request) => {
    fs.fstat(request.body, (err, stats) => {
      if (err) throw err;
      request.respond(200, null, stats.size);
    });
  },
  withCreateProcess: (request) => {
    let { createProcess } = JSON.parse(request.body);
    tmp.file((err, path, fd, cleanupCallback) => {
      if (err) throw err;

      const reader = fs.createReadStream(path);

      reader.on("open", (_fd) => {
        nextCounter += 1;
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

      reader.on("data", (chunk) => {
        processes[nextCounter].stdin.end(chunk);
      });
    });
  },
  hClose: (request) => {
    fs.close(request.body);
    request.respond(200);
  },
  waitForProcess: (request) => {
    processes[request.body].on("exit", (code) => {
      request.respond(200, null, code);
    });
  },
  exitWith: (request) => {
    rl.close();
    process.exit(request.body);
  },
  dirFindExecutable: (request) => {
    request.respond(200, null, which.sync(request.body, { nothrow: true }));
  },
  replGetInputLine: (request) => {
    rl.question(request.body, (value) => {
      request.respond(200, null, value);
    });
  },
  dirDoesFileExist: (request) => {
    fs.stat(request.body, (err, stats) => {
      request.respond(200, null, !err && stats.isFile());
    });
  },
  dirCreateDirectoryIfMissing: (request) => {
    const { createParents, filename } = JSON.parse(request.body);
    fs.mkdir(filename, { recursive: createParents }, (err) => {
      request.respond(200);
    });
  },
  lockFile: (request) => {
    const path = request.body;

    if (lockedFiles[path]) {
      lockedFiles[path].subscribers.push(request);
    } else {
      lockedFiles[path] = { subscribers: [] };
      request.respond(200);
    }
  },
  unlockFile: (request) => {
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
  },
  dirGetModificationTime: (request) => {
    fs.stat(request.body, (err, stats) => {
      if (err) throw err;
      request.respond(200, null, parseInt(stats.mtimeMs, 10));
    });
  },

  dirDoesDirectoryExist: (request) => {
    fs.stat(request.body, (err, stats) => {
      request.respond(200, null, !err && stats.isDirectory());
    });
  },
  dirCanonicalizePath: (request) => {
    request.respond(200, null, resolve(request.body));
  },
  dirListDirectory: (request) => {
    fs.readdir(request.body, { recursive: false }, (err, files) => {
      if (err) throw err;
      request.respond(200, null, JSON.stringify(files));
    });
  },
  binaryDecodeFileOrFail: (request) => {
    fs.readFile(request.body, (err, data) => {
      if (err) throw err;
      request.respond(200, null, data.toString());
    });
  },
  write: (request) => {
    const { fd, content } = JSON.parse(request.body);
    fs.writeFile(fd, JSON.stringify(content), (err) => {
      if (err) throw err;
      request.respond(200);
    });
  },
  dirRemoveFile: (request) => {
    fs.unlink(request.body, (err) => {
      if (err) throw err;
      request.respond(200);
    });
  },
  dirRemoveDirectoryRecursive: (request) => {
    fs.rm(request.body, { recursive: true, force: true }, (err) => {
      if (err) throw err;
      request.respond(200);
    });
  },
  dirWithCurrentDirectory: (request) => {
    try {
      process.chdir(request.body);
      request.respond(200);
    } catch (err) {
      console.error(`chdir: ${err}`);
    }
  },
  // MVARS
  newEmptyMVar: (request) => {
    nextCounter += 1;
    mVars[nextCounter] = { subscribers: [], value: undefined };
    request.respond(200, null, nextCounter);
  },
  readMVar: (request) => {
    const id = request.body;
    if (typeof mVars[id].value === "undefined") {
      mVars[id].subscribers.push({ action: "read", request });
    } else {
      request.respond(200, null, JSON.stringify(mVars[id].value));
    }
  },
  takeMVar: (request) => {
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

      request.respond(200, null, JSON.stringify(value));
    }
  },
  putMVar: (request) => {
    const { id, value } = JSON.parse(request.body);
    if (typeof mVars[id].value === "undefined") {
      mVars[id].value = value;

      mVars[id].subscribers = mVars[id].subscribers.filter((subscriber) => {
        if (subscriber.action === "read") {
          subscriber.request.respond(200, null, JSON.stringify(value));
        }

        return subscriber.action !== "read";
      });

      const subscriber = mVars[id].subscribers.shift();

      if (subscriber) {
        subscriber.request.respond(200, null, JSON.stringify(value));

        if (subscriber.action === "take") {
          mVars[id].value = undefined;
        }
      }

      request.respond(200);
    } else {
      mVars[id].subscribers.push({ action: "put", request, value });
    }
  }
};

const savedXMLHttpRequest = globalThis.XMLHttpRequest;
MockXhr.onSend = (request) => {
  const handler = handlers[request.url];

  if (handler) {
    handler(request);
  } else {
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
  }
};

// Install in the global context so "new XMLHttpRequest()" creates MockXhr instances
globalThis.XMLHttpRequest = MockXhr;

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
const { Elm } = require("./guida.min.js");
const FormData = require("form-data");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

let nextCounter = 0;
const mVars = {};
const lockedFiles = {};
const processes = {};

const download = function (index, method, url) {
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

        this.send({ index, sha, archive });
      });
    } else if (res.headers.location) {
      download.apply(this, [index, method, res.headers.location]);
    }
  });

  req.on("error", (e) => {
    console.error(e);
  });

  req.end();
};

const app = Elm.Terminal.Main.init({
  flags: {
    args: process.argv.slice(2),
    currentDirectory: process.cwd(),
    envVars: Object.entries(process.env),
    homedir: os.homedir(),
    progName: "guida"
  }
});