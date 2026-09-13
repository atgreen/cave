const fs = require("node:fs");
const path = require("node:path");
const { test, expect } = require("@playwright/test");

const root = path.resolve(__dirname, "..");
const favicon = fs.readFileSync(path.join(root, "static/img/favicon.svg"), "utf8");
const styles = fs.readFileSync(path.join(root, "static/css/cave.css"), "utf8");
const pageView = fs.readFileSync(path.join(root, "src/views-base.lisp"), "utf8");

test("the favicon and navigation share the Basalt Signal mark", () => {
  expect(favicon).toContain('<title>Cave</title>');
  expect(favicon).toContain('viewBox="0 0 64 64"');
  expect(favicon).not.toContain("<circle");

  expect(styles).toMatch(/\.nav-logo\s*\{[^}]*favicon\.svg[^}]*mask/s);
  expect(pageView).toContain("(:span.nav-logo :aria-hidden \"true\")");
  expect(pageView).not.toContain('<svg class=\\"nav-logo\\"');

});
