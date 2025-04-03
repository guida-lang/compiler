import { defineConfig, globalIgnores } from "eslint/config";
import globals from "globals";
import js from "@eslint/js";
import pluginJest from "eslint-plugin-jest";


export default defineConfig([
  globalIgnores([
    "bin/guida.js",
    "bin/guida.min.js",
    "lib/guida.js",
    "lib/guida.min.js",
  ]),
  { files: ["**/*.{js,mjs,cjs}"] },
  { files: ["**/*.js"], languageOptions: { sourceType: "commonjs" } },
  { files: ["bin/**/*.{js,mjs,cjs}"], languageOptions: { globals: globals.node } },
  { files: ["lib/**/*.{js,mjs,cjs}"], languageOptions: { globals: globals.browser } },
  { files: ["try/**/*.{js,mjs,cjs}"], languageOptions: { globals: { ...globals.browser, ...globals.node } } },
  {
    files: ["**/*.{js,mjs,cjs}"],
    plugins: { js },
    extends: ["js/recommended"],
    rules: {
      "no-unused-vars": ["error", {
        "argsIgnorePattern": "^_",
        "caughtErrorsIgnorePattern": "^_"
      }]
    }
  },
  {
    files: ["**/*.test.js"],
    plugins: { jest: pluginJest },
    languageOptions: {
      globals: { ...globals.node, ...pluginJest.environments.globals.globals },
    },
    rules: {
      "no-empty": ["error", { "allowEmptyCatch": true }],
      "jest/no-disabled-tests": "warn",
      "jest/no-focused-tests": "error",
      "jest/no-identical-title": "error",
      "jest/prefer-to-have-length": "warn",
      "jest/valid-expect": "error",
    },
  },
]);