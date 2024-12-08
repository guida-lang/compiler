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

        this.send({ index, value: [sha, archive] });
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
    envVars: Object.keys(process.env).reduce((acc, key) => { acc.push([key, process.env[key]]); return acc }, []),
    homedir: os.homedir(),
    progName: "guida"
  }
});

app.ports.sendGetLine.subscribe(function (index) {
  rl.on("line", (value) => {
    app.ports.recvGetLine.send({ index, value });
  });
});

app.ports.sendHPutStr.subscribe(function ({ index, fd, content }) {
  fs.write(fd, content, (err) => {
    if (err) throw err;
    app.ports.recvHPutStr.send(index);
  });
});

app.ports.sendWriteString.subscribe(function ({ index, path, content }) {
  fs.writeFile(path, content, (err) => {
    if (err) throw err;
    app.ports.recvWriteString.send(index);
  });
});

app.ports.sendRead.subscribe(function ({ index, fd }) {
  fs.readFile(fd, (err, data) => {
    if (err) throw err;
    app.ports.recvRead.send({ index, value: data.toString() });
  });
});

app.ports.sendHttpFetch.subscribe(function ({ index, method, urlStr, headers }) {
  const url = new URL(urlStr);
  const client = url.protocol == "https:" ? https : http;

  const req = client.request(url, {
    method
    , headers: headers.reduce((acc, [key, value]) => { acc[key] = value; return acc }, {})
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
          app.ports.recvHttpFetch.send({ index, value: decoded && decoded.toString() });
        });
      } else if (encoding == "deflate") {
        zlib.inflate(buffer, (err, decoded) => {
          if (err) throw err;
          app.ports.recvHttpFetch.send({ index, value: decoded && decoded.toString() });
        });
      } else {
        app.ports.recvHttpFetch.send({ index, value: buffer.toString() });
      }
    });
  });

  req.on("error", (err) => {
    throw err;
  });

  req.end();
});

app.ports.sendGetArchive.subscribe(function ({ index, method, url }) {
  download.apply(app.ports.recvGetArchive, [index, method, url]);
});

app.ports.sendHttpUpload.subscribe(function ({ index, urlStr, headers, parts }) {
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
      app.ports.recvHttpUpload.send(index);
    });
  });

  req.on("error", (err) => {
    throw err;
  });
});

app.ports.sendHFlush.subscribe(function ({ index, fd }) {
  app.ports.recvHFlush.send(index);
});

app.ports.sendWithFile.subscribe(function ({ index, filename, mode }) {
  fs.open(filename, mode, (err, fd) => {
    if (err) throw err;
    app.ports.recvWithFile.send({ index, value: fd });
  });
});

app.ports.sendHFileSize.subscribe(function ({ index, fd }) {
  fs.fstat(fd, (err, stats) => {
    if (err) throw err;
    app.ports.recvHFileSize.send({ index, value: stats.size });
  });
});

app.ports.sendProcWithCreateProcess.subscribe(function ({ index, createProcess }) {
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

      app.ports.recvProcWithCreateProcess.send({ index, value: { stdinHandle: fd, ph: nextCounter } });
    });

    reader.on("data", (chunk) => {
      processes[nextCounter].stdin.end(chunk);
    });
  });
});

app.ports.sendHClose.subscribe(function ({ index, fd }) {
  fs.close(fd);
  app.ports.recvHClose.send(index);
});

app.ports.sendProcWaitForProcess.subscribe(function ({ index, ph }) {
  processes[ph].on("exit", (code) => {
    app.ports.recvProcWaitForProcess.send({ index, value: code });
  });
});

app.ports.sendExitWith.subscribe(function (code) {
  rl.close();
  process.exit(code);
});

app.ports.sendDirFindExecutable.subscribe(function ({ index, name }) {
  app.ports.recvDirFindExecutable.send({ index, value: which.sync(name, { nothrow: true }) });
});

app.ports.sendReplGetInputLine.subscribe(function ({ index, prompt }) {
  rl.question(prompt, (value) => {
    app.ports.recvReplGetInputLine.send({ index, value });
  });
});

app.ports.sendDirDoesFileExist.subscribe(function ({ index, filename }) {
  app.ports.recvDirDoesFileExist.send({ index, value: fs.existsSync(filename) });
});

app.ports.sendDirCreateDirectoryIfMissing.subscribe(function ({ index, createParents, filename }) {
  fs.mkdir(filename, { recursive: createParents }, (err) => {
    app.ports.recvDirCreateDirectoryIfMissing.send(index);
  });
});

app.ports.sendLockFile.subscribe(function ({ index, path }) {
  if (lockedFiles[path]) {
    lockedFiles[path].subscribers.push(index);
  } else {
    lockedFiles[path] = { subscribers: [] };
    app.ports.recvLockFile.send(index);
  }
});

app.ports.sendUnlockFile.subscribe(function ({ index, path }) {
  if (lockedFiles[path]) {
    const subscriber = lockedFiles[path].subscribers.shift();

    if (subscriber) {
      app.ports.recvUnlockFile.send(subscriber);
    } else {
      delete lockedFiles[path];
    }

    app.ports.recvUnlockFile.send(index);
  } else {
    console.error(`Could not find locked file "${path}"!`);
    rl.close();
    process.exit(255);
  }
});

app.ports.sendDirGetModificationTime.subscribe(function ({ index, filename }) {
  fs.stat(filename, (err, stats) => {
    if (err) throw err;
    app.ports.recvDirGetModificationTime.send({ index, value: parseInt(stats.mtimeMs, 10) });
  });
});

app.ports.sendDirDoesDirectoryExist.subscribe(function ({ index, path }) {
  app.ports.recvDirDoesDirectoryExist.send({ index, value: fs.existsSync(path) });
});

app.ports.sendDirCanonicalizePath.subscribe(function ({ index, path }) {
  app.ports.recvDirCanonicalizePath.send({ index, value: resolve(path) });
});

app.ports.sendBinaryDecodeFileOrFail.subscribe(function ({ index, filename }) {
  fs.readFile(filename, (err, data) => {
    if (err) throw err;
    app.ports.recvBinaryDecodeFileOrFail.send({ index, value: JSON.parse(data.toString()) });
  });
});

app.ports.sendWrite.subscribe(function ({ index, fd, content }) {
  fs.writeFile(fd, JSON.stringify(content), (err) => {
    if (err) throw err;
    app.ports.recvWrite.send(index);
  });
});

app.ports.sendDirRemoveFile.subscribe(function ({ index, path }) {
  fs.unlink(path, (err) => {
    if (err) throw err;
    app.ports.recvDirRemoveFile.send(index);
  });
});

app.ports.sendDirRemoveDirectoryRecursive.subscribe(function ({ index, path }) {
  fs.rm(path, { recursive: true, force: true }, (err) => {
    if (err) throw err;
    app.ports.recvDirRemoveDirectoryRecursive.send(index);
  });
});

app.ports.sendDirWithCurrentDirectory.subscribe(function ({ index, path }) {
  try {
    process.chdir(path);
    app.ports.recvDirWithCurrentDirectory.send(index);
  } catch (err) {
    console.error(`chdir: ${err}`);
  }
});

app.ports.sendReplGetInputLineWithInitial.subscribe(function ({ index, prompt, left, right }) {
  rl.question(prompt + left + right, (value) => {
    app.ports.recvReplGetInputLineWithInitial.send({ index, value });
  });
});

// MVARS

app.ports.sendNewEmptyMVar.subscribe(function (index) {
  nextCounter += 1;
  mVars[nextCounter] = { subscribers: [], value: undefined };
  app.ports.recvNewEmptyMVar.send({ index, value: nextCounter });
});

app.ports.sendReadMVar.subscribe(function ({ index, id }) {
  if (typeof mVars[id].value === "undefined") {
    mVars[id].subscribers.push({ index, action: "read" });
  } else {
    app.ports.recvReadMVar.send({ index, value: mVars[id].value });
  }
});

app.ports.sendTakeMVar.subscribe(function ({ index, id }) {
  if (typeof mVars[id].value === "undefined") {
    mVars[id].subscribers.push({ index, action: "take" });
  } else {
    const value = mVars[id].value;
    mVars[id].value = undefined;

    if (
      mVars[id].subscribers.length > 0 &&
      mVars[id].subscribers[0].action === "put"
    ) {
      const subscriber = mVars[id].subscribers.shift();
      mVars[id].value = subscriber.value;
      app.ports.recvPutMVar.send(subscriber.index);
    }

    app.ports.recvReadMVar.send({ index, value });
  }
});

app.ports.sendPutMVar.subscribe(function ({ index, id, value }) {
  if (typeof mVars[id].value === "undefined") {
    mVars[id].value = value;

    mVars[id].subscribers = mVars[id].subscribers.filter((subscriber) => {
      if (subscriber.action === "read") {
        app.ports.recvReadMVar.send({ index: subscriber.index, value });
      }

      return subscriber.action !== "read";
    });

    const subscriber = mVars[id].subscribers.shift();

    if (subscriber) {
      app.ports.recvReadMVar.send({ index: subscriber.index, value });

      if (subscriber.action === "take") {
        mVars[id].value = undefined;
      }
    }

    app.ports.recvPutMVar.send(index);
  } else {
    mVars[id].subscribers.push({ index, action: "put", value });
  }
});
