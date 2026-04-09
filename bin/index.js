#!/usr/bin/env node

const fs = require("node:fs");
const readline = require("node:readline");
const http = require("node:http");
const https = require("node:https");
const zlib = require("node:zlib");
const FormData = require("form-data");
const { newServer } = require("mock-xmlhttprequest");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

let stateT = { imports: {}, types: {}, decls: {} };
const lockedFiles = {};

const server = newServer();

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

server.post("putStateT", (request) => {
  stateT = request.body;
  request.respond(200);
});

server.post("getStateT", (request) => {
  request.respond(200, null, stateT.buffer);
});

server.setDefaultHandler((request) => {
  const url = new URL(request.url);
  const client = url.protocol == "https:" ? https : http;

  const req = client.request(url, {
    method: request.method,
    headers: request.requestHeaders.getHash()
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

const { Guida } = require("./guida.min.js");

Guida.Terminal.Main.init();
