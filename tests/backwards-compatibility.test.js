const fs = require("node:fs");
const path = require("node:path");
const childProcess = require("child_process");
const os = require("os");
const tmpDir = os.tmpdir();

const defaultFlags = ["no-flags", "debug", "optimize"];

const examples = [
  // HTML
  ["Hello", defaultFlags],
  ["Groceries", defaultFlags],
  ["Shapes", defaultFlags],
  // User Input
  ["Buttons", defaultFlags],
  ["TextFields", defaultFlags],
  ["Forms", defaultFlags],
  // Random
  ["Numbers", defaultFlags],
  ["Cards", defaultFlags],
  ["Positions", defaultFlags],
  // HTTP
  ["Book", defaultFlags],
  ["Quotes", defaultFlags],
  // Time
  ["CurrentTime", defaultFlags],
  ["Clock", defaultFlags],
  // Files
  ["Upload", ["no-flags", "debug"]],
  ["DragAndDrop", ["no-flags", "debug"]],
  ["ImagePreviews", defaultFlags],
  // WebGL
  ["Triangle", defaultFlags],
  ["Cube", defaultFlags],
  ["Crate", defaultFlags],
  ["Thwomp", defaultFlags],
  ["FirstPerson", defaultFlags],
  // Playground
  ["Picture", defaultFlags],
  ["Animation", defaultFlags],
  ["Mouse", defaultFlags],
  ["Keyboard", defaultFlags],
  ["Turtle", defaultFlags],
  ["Mario", defaultFlags],
];

const replaceKnownDifferencesOutput = function (filePath) {
  const content = readFileContent(filePath);
  return replaceKnownDifferencesGuida(replaceKnownDifferencesNewEscapedCode(content));
};

const replaceKnownDifferencesReport = function (filePath) {
  const content = readFileContent(filePath);
  return replaceKnownDifferencesGuida(content);
};

const replaceKnownDifferencesDocs = function (filePath) {
  const content = readFileContent(filePath);
  return replaceKnownDifferencesGuida(content);
};

const replaceKnownDifferencesNewEscapedCode = function (content) {
  return content.replace("__END__\n", "__END__").replace(/\/\/__START__$(?:(?!__START__)[\s\S])*?\/\/__END__/gm, "")
};

const replaceKnownDifferencesGuida = function (content) {
  return content
    // versions
    .replaceAll("\"versions\":{\"guida\":\"1.0.0\"}", "\"versions\":{\"elm\":\"0.19.1\"}")
    // new documentation links
    .replaceAll("https://guida-lang.org/docs/1.0.0/hints/bad-recursion", "https://elm-lang.org/0.19.1/bad-recursion")
    .replaceAll("https://guida-lang.org/docs/1.0.0/hints/optimize", "https://elm-lang.org/0.19.1/optimize")
    .replaceAll("https://guida-lang.org/docs/1.0.0/hints/tuples", "https://elm-lang.org/0.19.1/tuples")
    // other minor differences
    .replaceAll("> for more\\ncomprehensive advice on working with large chunks of data in Guida.", "> for more\\ncomprehensive advice on working with large chunks of data in Elm.")
    .replaceAll("> for more comprehensive advice on\\nworking with large chunks of data in Guida.", "> for more comprehensive advice on\\nworking with large chunks of data in Elm.");
};

const readFileContent = function (filePath) {
  return fs.readFileSync(filePath).toString();
};

const generateCommandFlags = function (flag) {
  if (flag === "no-flags") {
    return "";
  } else {
    return `--${flag}`;
  }
};

describe("backwards compatibility", () => {
  describe.each(examples)(
    "produces the same code as elm for the %s example",
    (example, currentFlags) => {
      test.each(currentFlags)("%s", (flag) => {
        const elmOutput = `${tmpDir}/guida-test-elm-${example}-${flag}-${process.pid}.js`;
        const guidaOutput = `${tmpDir}/guida-test-guida-${example}-${flag}-${process.pid}.js`;
        const commandFlag = generateCommandFlags(flag);

        try {
          childProcess.execSync(
            `elm make src/${example}.elm ${commandFlag} --output ${elmOutput}`,
            { cwd: path.join(__dirname, "..", "examples") }
          );
        } catch (e) {
          console.error(e);
        }

        try {
          childProcess.execSync(
            `../bin/index.js make src/${example}.elm ${commandFlag} --output ${guidaOutput}`,
            { cwd: path.join(__dirname, "..", "examples"), env: { ...process.env, GUIDA_REGISTRY: "https://package.elm-lang.org" } }
          );
        } catch (e) {
          console.error(e);
        }

        expect(replaceKnownDifferencesOutput(guidaOutput)).toBe(readFileContent(elmOutput));
      });
    }
  );

  test("self-hosted environment", () => {
    const elmOutput = `${tmpDir}/guida-test-elm-self-hosted-${process.pid}.js`;
    const guidaOutput = `${tmpDir}/guida-test-guida-self-hosted-${process.pid}.js`;

    try {
      childProcess.execSync(
        `elm make src/Terminal/Main.elm --output ${elmOutput}`,
        { cwd: path.join(__dirname, "..") }
      );
    } catch (e) {
      console.error(e);
    }

    try {
      childProcess.execSync(
        `./bin/index.js make src/Terminal/Main.elm --output ${guidaOutput}`,
        { cwd: path.join(__dirname, ".."), env: { ...process.env, GUIDA_REGISTRY: "https://package.elm-lang.org" } }
      );
    } catch (e) {
      console.error(e);
    }

    expect(replaceKnownDifferencesOutput(guidaOutput)).toBe(readFileContent(elmOutput));
  });

  test("json report", () => {
    const elmOutput = `${tmpDir}/guida-test-elm-json-report-${process.pid}.json`;
    const guidaOutput = `${tmpDir}/guida-test-guida-json-report-${process.pid}.json`;

    try {
      childProcess.execSync(
        `elm make src/Invalid.elm --report=json &> ${elmOutput}`,
        { cwd: path.join(__dirname, "..", "assets", "some-application") }
      );
    } catch (_) { }

    try {
      childProcess.execSync(
        `../../bin/index.js make src/Invalid.elm --report=json &> ${guidaOutput}`,
        { cwd: path.join(__dirname, "..", "assets", "some-application"), env: { ...process.env, GUIDA_REGISTRY: "https://package.elm-lang.org" } }
      );
    } catch (_) { }

    expect(replaceKnownDifferencesReport(guidaOutput)).toBe(readFileContent(elmOutput));
  });

  test("docs", () => {
    const elmOutput = `${tmpDir}/guida-test-elm-docs-${process.pid}.json`;
    const guidaOutput = `${tmpDir}/guida-test-guida-docs-${process.pid}.json`;

    try {
      childProcess.execSync(
        `elm make --docs=${elmOutput}`,
        { cwd: path.join(__dirname, "..", "assets", "some-package") }
      );
    } catch (e) {
      console.error(e);
    }

    try {
      childProcess.execSync(
        `../../bin/index.js make --docs=${guidaOutput}`,
        { cwd: path.join(__dirname, "..", "assets", "some-package"), env: { ...process.env, GUIDA_REGISTRY: "https://package.elm-lang.org" } }
      );
    } catch (e) {
      console.error(e);
    }

    expect(replaceKnownDifferencesDocs(guidaOutput)).toBe(readFileContent(elmOutput));
  });

  describe("tuples", () => {
    test("fails on 3+ tuples on elm files", () => {
      const elmOutput = `${tmpDir}/guida-test-elm-tuple-n-${process.pid}.json`;
      const guidaOutput = `${tmpDir}/guida-test-guida-tuple-n-${process.pid}.json`;

      try {
        childProcess.execSync(
          `elm make src/ElmTupleN.elm --report=json &> ${elmOutput}`,
          { cwd: path.join(__dirname, "..", "assets", "some-application") }
        );
      } catch (_) { }

      try {
        childProcess.execSync(
          `../../bin/index.js make src/ElmTupleN.elm --report=json &> ${guidaOutput}`,
          { cwd: path.join(__dirname, "..", "assets", "some-application"), env: { ...process.env, GUIDA_REGISTRY: "https://package.elm-lang.org" } }
        );
      } catch (_) { }

      expect(replaceKnownDifferencesReport(guidaOutput)).toBe(readFileContent(elmOutput));
    });
  });
});
