// @ts-check
import { test, expect } from "@playwright/test";
import { pathToFileURL } from "node:url";
import { execSync } from "node:child_process";
import path from "node:path";

const guidaAppPath = path.join(__dirname, "..", "assets", "bytes-over-flags-ports");

/**
 * @param {string} filepath
 * @param {string} output
 * @returns {Buffer}
 */
const guidaMake = (filepath, output) => {
    return execSync(`../../bin/index.js make ${filepath} --output=tmp/${output}`, {
        cwd: guidaAppPath
    });
};

/**
 * @param {string} filename
 * @returns {string}
 */
const fileURL = (filename) => {
    return pathToFileURL(path.join(guidaAppPath, filename)).toString();
};

test("Bytes over Flags and Ports", async ({ page }) => {
    guidaMake("src/Main.guida", "index.js");

    await page.goto(fileURL("index.html"));

    // Expects page to have a title of Bytes over Flags and Ports.
    await expect(page).toHaveTitle("Bytes over Flags and Ports");

    // Expects page to contain "Width: 7".
    await expect(page.getByTestId("message")).toContainText("Received bytes back from JavaScript! Width: 7");
});