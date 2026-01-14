// @ts-check
import { test, expect } from "@playwright/test";
import { pathToFileURL } from "node:url";
import { execSync } from "node:child_process";
import path from "node:path";

const guidaAppPath = path.join(__dirname, "..", "assets", "some-guida-application");

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
  return pathToFileURL(path.join(guidaAppPath, "tmp", filename)).toString();
};

test("Hello", async ({ page }) => {
  guidaMake("src/Hello.guida", "hello.html");

  await page.goto(fileURL("hello.html"));

  // Expects page to have a title of Hello.
  await expect(page).toHaveTitle("Hello");

  // Expects page to contain "Hello!".
  await expect(page.getByTestId("hello")).toContainText("Hello!");
});

test("Buttons", async ({ page }) => {
  guidaMake("src/Buttons.guida", "buttons.html");

  await page.goto(fileURL("buttons.html"));

  // Expects page to have a title of Buttons.
  await expect(page).toHaveTitle("Buttons");

  // Expects number to equal 0.
  await expect(page.getByTestId("number")).toContainText("0");

  // Click plus button.
  await page.getByTestId("plus").click();

  // Expects number to equal 1.
  await expect(page.getByTestId("number")).toContainText("1");
});