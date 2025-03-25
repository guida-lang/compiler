const fs = require("fs");
const path = require("path");
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

const escapedNewCodeRegex = function (guidaOutput) {
  return fs.readFileSync(guidaOutput).toString().replace("__END__\n", "__END__").replace(/\/\/__START__$(?:(?!__START__)[\s\S])*?\/\/__END__/gm, "");
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
            `elm make ./src/${example}.elm ${commandFlag} --output ${elmOutput}`,
            { cwd: path.join(__dirname, "..", "examples") }
          );
        } catch (e) {
          console.error(e);
        }

        try {
          childProcess.execSync(
            `../bin/index.js make ./src/${example}.elm ${commandFlag} --output ${guidaOutput}`,
            { cwd: path.join(__dirname, "..", "examples") }
          );
        } catch (e) {
          console.error(e);
        }

        expect(fs.readFileSync(elmOutput).toString()).toBe(escapedNewCodeRegex(guidaOutput));
      });
    }
  );

  test("self-hosted environment", () => {
    const elmOutput = `${tmpDir}/guida-test-elm-self-hosted-${process.pid}.js`;
    const guidaOutput = `${tmpDir}/guida-test-guida-self-hosted-${process.pid}.js`;

    try {
      childProcess.execSync(
        `elm make ./src/Terminal/Main.elm --output ${elmOutput}`,
        { cwd: path.join(__dirname, "..") }
      );
    } catch (e) {
      console.error(e);
    }

    try {
      childProcess.execSync(
        `./bin/index.js make ./src/Terminal/Main.elm --output ${guidaOutput}`,
        { cwd: path.join(__dirname, "..") }
      );
    } catch (e) {
      console.error(e);
    }

    expect(fs.readFileSync(elmOutput).toString()).toBe(escapedNewCodeRegex(guidaOutput));
  });
});
