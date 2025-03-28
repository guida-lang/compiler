#!/usr/bin/env node

const express = require("express");
const cors = require("cors");
const { createProxyMiddleware } = require("http-proxy-middleware");
const path = require("path");

const app = express();

app.use("/proxy/", cors());
app.use("/proxy/", createProxyMiddleware({
    router: (req) => new URL(req.url.substring(1)),
    pathRewrite: (_path, req) => (new URL(req.url.substring(1))).pathname,
    changeOrigin: true,
    followRedirects: true,
    logger: console
}))

app.use(express.static(path.join(__dirname, "public")));

app.listen(8088, () => {
    console.info("proxy server is running on http://127.0.0.1:8088");
});