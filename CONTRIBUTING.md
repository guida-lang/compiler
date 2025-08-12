# Contributing to Guida

:+1::tada: First off, thank you for your interest in contributing to Guida! :tada::+1:

This document outlines guidelines and best practices for contributing code, ideas, or feedback to the project.

---

## 📋 Table of Contents

- [Getting Started](#getting-started)
- [Ways to Contribute](#ways-to-contribute)
- [Expectations](#expectations)
- [Development Setup](#development-setup)
- [Testing Your Changes](#testing-your-changes)
- [Submitting a Pull Request](#submitting-a-pull-request)
- [Style Guide](#style-guide)
- [Reporting Issues](#reporting-issues)
- [Questions?](#questions)

---

## Getting Started

Guida is a functional programming language that builds upon the solid foundation of Elm, offering
backward compatibility with all existing Elm 0.19.1 projects.

Find out more about our [Vision](README.md#vision) on the project [README](README.md).

We welcome contributions of all kinds, code, documentation, bug reports, feedback, and ideas!

If you're unsure about where to start or how to approach an issue, feel free to open a [discussion](https://github.com/guida-lang/compiler/discussions)
or reach out directly on Elm Slack (@deciojf). We’re happy to help point you in the right direction!

---

## Ways to Contribute

- File a bug or feature request [here](https://github.com/guida-lang/compiler/issues)
- Help triage existing issues
- Submit improvements to the [compiler](https://github.com/guida-lang/compiler), [registry](https://github.com/guida-lang/package-registry), or [tooling](https://github.com/guida-lang)
- Improve documentation or examples
- Try out Guida and give us feedback
- Look for [good first issues](https://github.com/guida-lang/compiler/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22) if you're just getting started
- Port known issues or improvements from the Elm ecosystem

  Guida builds on projects like [elm/compiler](https://github.com/elm/compiler), [elm-format](https://github.com/avh4/elm-format), [elm-test](https://github.com/elm-explorations/test), and [elm-json](https://github.com/zwilias/elm-json). If you've encountered issues or ideas in those tools that feel worth bringing into Guida, feel free to reference them in a new issue or PR

---

## Expectations

- We aim to respond to contributions within a few days.
- All changes should align with the project's vision: stability, compatibility with Elm, and community evolution.
- PRs should be focused and include a clear description.
- Don’t worry if your PR needs changes — we’ll help you get it over the finish line!

---

## Development Setup

For detailed instructions on setting up your environment for contributing to Guida, see the [Development](README.md#development) section of the README.

## Testing Your Changes

We aim for stability and consistency. If you’re adding features or fixing bugs, please:

- Write tests if applicable.
- Consider all 3 outputs of the project: bin (command line), browser and node (API).
- Make sure to test all three output targets of the project:
  - CLI (bin) — the command-line interface
  - Browser — compiled for browser usage
  - Node — used as a Node.js API
- Make sure existing tests pass: see the [Run tests](README.md#run-tests) section of the README.

---

## Submitting a Pull Request

1. Fork the repo and create a new branch:
   ```sh
   git checkout -b my-feature
   ```

2. Make your changes.
3. Test locally.
4. Push and open a Pull Request (PR) to the `master` branch.

Please describe:
- What the change does
- Why it’s needed
- Any related issues or discussion

---

## Style Guide

- Follow the existing code style
- Keep Elm code idiomatic and readable
- Use descriptive names and add comments where helpful

For formatting Elm:

```sh
npm run elm-format
```

---

## Reporting Issues

If you encounter a bug or unexpected behavior:
- Search [existing issues](https://github.com/guida-lang/compiler/issues)
- If not found, open a new one with:
  - Steps to reproduce
  - Expected and actual behavior
  - Environment details

Some issues might relate to other repositories under the [guida-lang](https://github.com/guida-lang) (such as the package registry).
If you're unsure where the issue belongs, feel free to post anyway — we’ll help direct it to the right place.

---

## Questions?

You can usually find me on the [Elm Slack](https://elmlang.slack.com/). Feel free to reach out directly to @deciojf with any questions or feedback.

Please note that responses may come with a bit of delay, as availability can be limited during the day.

Thank you again for helping improve Guida!