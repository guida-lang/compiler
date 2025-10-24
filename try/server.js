#!/usr/bin/env node

const express = require("express");
const path = require("path");

const app = express();

app.use(express.static(path.join(__dirname, "public")));

app.listen(8088, () => {
    console.info("proxy server is running on http://127.0.0.1:8088");
});