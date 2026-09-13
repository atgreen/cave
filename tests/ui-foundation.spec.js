const fs = require("node:fs");
const path = require("node:path");
const { test, expect } = require("@playwright/test");

const root = path.resolve(__dirname, "..");
const styles = fs.readFileSync(path.join(root, "static/css/cave.css"), "utf8");
const repoViews = fs.readFileSync(path.join(root, "src/views-repos.lisp"), "utf8");
const settingsViews = fs.readFileSync(path.join(root, "src/views-settings.lisp"), "utf8");
const baseViews = fs.readFileSync(path.join(root, "src/views-base.lisp"), "utf8");
const runsViews = fs.readFileSync(path.join(root, "src/views-runs.lisp"), "utf8");
const accountRoutes = fs.readFileSync(path.join(root, "src/server-accounts.lisp"), "utf8");

function hexToRgb(hex) {
  return hex.match(/[a-f\d]{2}/gi).map((channel) => parseInt(channel, 16) / 255);
}

function luminance(hex) {
  const channels = hexToRgb(hex).map((channel) =>
    channel <= 0.04045
      ? channel / 12.92
      : ((channel + 0.055) / 1.055) ** 2.4,
  );
  return 0.2126 * channels[0] + 0.7152 * channels[1] + 0.0722 * channels[2];
}

function contrast(foreground, background) {
  const lighter = Math.max(luminance(foreground), luminance(background));
  const darker = Math.min(luminance(foreground), luminance(background));
  return (lighter + 0.05) / (darker + 0.05);
}

function themeTokens(selector) {
  const escaped = selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const block = styles.match(new RegExp(`${escaped}\\s*\\{([^}]+)\\}`))[1];
  return Object.fromEntries(
    [...block.matchAll(/--([\w-]+):\s*(#[\da-f]{6})/gi)].map((match) => [
      match[1],
      match[2],
    ]),
  );
}

test("muted text meets WCAG AA in every built-in theme", () => {
  for (const selector of [
    ":root",
    'html[data-theme="solarized-dark"]',
    'html[data-theme="nord"]',
    'html[data-theme="light"]',
    'html[data-theme="dracula"]',
  ]) {
    const tokens = themeTokens(selector);
    expect(
      contrast(tokens["text-muted"], tokens.bg),
      `${selector} muted text contrast`,
    ).toBeGreaterThanOrEqual(4.5);
  }
});

test("anonymous visitors default to Terminal Warmth", () => {
  expect(baseViews).toContain('th "terminal-warmth"');
});

test("the shared visual system defines a generous responsive canvas", () => {
  expect(styles).toMatch(/--content-width:\s*1200px/);
  expect(styles).toMatch(/--control-height:\s*36px/);
  expect(styles).toMatch(/html\s*\{[^}]*font-size:\s*16px/s);
  expect(styles).toMatch(/\.container\s*\{[^}]*max-width:\s*var\(--content-width\)/s);
  expect(styles).toMatch(/h1, h2, h3, h4, h5, h6\s*\{[^}]*var\(--font-body\)/s);
});

test("repository pages have a clear identity and responsive metadata rail", () => {
  expect(repoViews).toContain("(:header.repo-header");
  expect(repoViews).toContain("(:h1.repo-title");
  expect(repoViews).toContain("(:div.repo-layout");
  expect(repoViews).toContain("(:aside.repo-sidebar");
  expect(styles).toMatch(/\.repo-layout\s*\{[^}]*grid-template-columns:\s*minmax\(0, 1fr\) var\(--sidebar-width\)/s);
  expect(styles).toMatch(/@media\s*\(max-width:\s*900px\)[\s\S]*\.repo-layout\s*\{[^}]*grid-template-columns:\s*1fr/);
  expect(styles).toMatch(/\.repo-sidebar \.clone-widget\s*\{[^}]*display:\s*grid/s);
  expect(styles).toContain(".repo-create-tabs .repo-tab");
  expect(styles).not.toMatch(/^\.repo-tab, \.repo-tab-active/m);
  expect(repoViews).not.toMatch(/\.ref-switcher[^\n]*background:\s*#(?:fff|f6f6f6)/i);
});

test("settings exposes scannable, deep-linkable categories", () => {
  expect(settingsViews).toContain("(:nav.settings-nav :aria-label \"Settings sections\"");
  for (const id of [
    "appearance",
    "security",
    "cli",
    "ssh-keys",
    "gpg-keys",
    "runners",
    "api-tokens",
  ]) {
    expect(settingsViews).toContain(`:id "${id}"`);
    expect(settingsViews).toContain(`:href "#${id}"`);
  }
  expect(styles).toMatch(/\.settings-layout\s*\{[^}]*grid-template-columns/s);
});

test("admin account dates are human and machine readable", () => {
  expect(baseViews).toContain("(defun format-datetime-utc");
  expect(baseViews).toContain("(defun datetime-iso8601");
  expect(settingsViews).toContain("(:table.data-table.admin-table");
  expect(styles).toMatch(/\.data-table\s*\{[^}]*overflow-x:\s*auto/s);
  expect(settingsViews).toContain("(:time :datetime (datetime-iso8601");
  expect(settingsViews).not.toContain("(:td (princ-to-string (getf u :created-at)))");
});

test("runner and workflow timestamps are human and machine readable", () => {
  expect(baseViews).toContain("(defun render-relative-time");
  expect(runsViews).toContain("(render-relative-time (getf wr :created-at))");
  expect(runsViews).toContain("(render-relative-time (getf r :created-at))");
  expect(runsViews).toContain('(render-relative-time ls :fallback "never")');
  expect(runsViews).not.toMatch(/princ-to-string \(getf (?:wr|r) :created-at\)/);
  expect(runsViews).not.toContain("(princ-to-string ls)");
});

test("run links use the shared link colour token", () => {
  expect(runsViews).not.toContain("var(--primary)");
});

test("activity feeds omit Cave's internal Dolt synchronization refs", () => {
  expect(baseViews).toContain("(defun internal-feed-ref-p");
  expect(baseViews).toContain('"refs/dolt/"');
  expect(baseViews).toContain('"__dolt_remote_info__"');
  expect(baseViews).toContain("(defun visible-feed-events");
  expect(accountRoutes.match(/:events \(visible-feed-events/g)).toHaveLength(2);
});

test("shared chrome exposes compact mobile navigation markup", () => {
  expect(baseViews).toContain("(:details.nav-menu");
  expect(baseViews).toContain("(:summary.nav-menu-toggle");
  expect(baseViews).toContain("(:div.nav-menu-content");
  expect(baseViews).toContain('(:script :src "/static/js/navigation.js" :defer t)');
});

test("repository and organization creation pages use focused form layouts", () => {
  expect(baseViews.match(/\(:div\.form-page/g) || []).toHaveLength(2);
  expect(repoViews).toContain("(:div.form-page");
});
