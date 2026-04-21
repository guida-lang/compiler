#!/usr/bin/env node

const http = require("node:http");
const https = require("node:https");
const zlib = require("node:zlib");
const { newServer } = require("mock-xmlhttprequest");

const server = newServer();

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

const { Guida } = require("./guida.js");

Guida.Terminal.Main.init();
