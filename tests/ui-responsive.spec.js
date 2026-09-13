const fs = require("node:fs");
const path = require("node:path");
const { test, expect } = require("@playwright/test");

const styles = fs.readFileSync(
  path.resolve(__dirname, "../static/css/cave.css"),
  "utf8",
);
const navigationScript = fs.readFileSync(
  path.resolve(__dirname, "../static/js/navigation.js"),
  "utf8",
);
const baseViews = fs.readFileSync(
  path.resolve(__dirname, "../src/views-base.lisp"),
  "utf8",
);

async function renderAtMobileWidth(page, markup) {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.setContent(`
    <!doctype html>
    <html data-theme="terminal-warmth">
      <head><style>${styles}</style></head>
      <body><main class="container">${markup}</main></body>
    </html>
  `);
}

async function renderAtDesktopWidth(page, markup) {
  await page.setViewportSize({ width: 1440, height: 1000 });
  await page.setContent(`
    <!doctype html>
    <html data-theme="terminal-warmth">
      <head><style>${styles}</style></head>
      <body>${markup}</body>
    </html>
  `);
}

test("public landing stacks repositories and activity without mobile overflow", async ({ page }) => {
  expect(baseViews).toContain("(:div.landing-grid");
  await renderAtMobileWidth(page, `
    <div class="landing-grid">
      <section>
        <h2>Repositories</h2>
        <ul class="repo-list"><li><a>atgreen/secscan-skill</a><span class="desc">Token-efficient in-session LLM security-triage skill</span></li></ul>
      </section>
      <section>
        <h2>Recent activity</h2>
        <ul class="issue-list"><li><span>atgreen pushed a commit that refreshes embedded version metadata</span></li></ul>
      </section>
    </div>
  `);
  const layout = await page.locator(".landing-grid").evaluate((element) => ({
    columns: getComputedStyle(element).gridTemplateColumns,
    scrollWidth: document.documentElement.scrollWidth,
    viewportWidth: document.documentElement.clientWidth,
  }));
  expect(layout.columns.split(" ")).toHaveLength(1);
  expect(layout.scrollWidth).toBeLessThanOrEqual(layout.viewportWidth);
});

test("mobile Settings contains long credentials within the viewport", async ({ page }) => {
  await renderAtMobileWidth(page, `
    <h1>Settings</h1>
    <div class="settings-layout">
      <nav class="settings-nav">
        <a>Appearance</a><a>Security</a><a>CLI</a><a>SSH keys</a>
        <a>GPG keys</a><a>Runners</a><a>API tokens</a>
      </nav>
      <div class="settings-content">
        <section class="settings-section">
          <h2>CLI</h2>
          <pre>export CAVE_BASE_URL=https://cave.moxielogic.com</pre>
        </section>
        <section class="settings-section">
          <h2>SSH keys</h2>
          <ul class="data-list">
            <li><strong>signing</strong><code>SHA256:6Qw0g0InfAb0bVyW090fMY0ScPaGyMKQ2p0qId7Z6pbw=</code><button class="btn">Remove</button></li>
          </ul>
        </section>
      </div>
    </div>
  `);

  expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBe(390);
  const categoryRailMask = await page.locator(".settings-nav").evaluate(
    (element) => getComputedStyle(element).maskImage,
  );
  expect(categoryRailMask).toContain("linear-gradient");
});

test("mobile data tables scroll inside the viewport", async ({ page }) => {
  await renderAtMobileWidth(page, `
    <h1>Instance administration</h1>
    <section>
      <h2>Runners</h2>
      <table class="data-table">
        <thead><tr><th>Name</th><th>Scope</th><th>Labels</th><th>Status</th><th>Last seen</th><th></th></tr></thead>
        <tbody><tr><td>h3-runner-7</td><td>instance</td><td></td><td>online</td><td>just now</td><td><button class="btn">Delete</button></td></tr></tbody>
      </table>
    </section>
  `);

  const dimensions = await page.locator("table").evaluate((table) => ({
    clientWidth: table.clientWidth,
    scrollWidth: table.scrollWidth,
    pageWidth: document.documentElement.scrollWidth,
  }));
  expect(dimensions.pageWidth).toBe(390);
  expect(dimensions.scrollWidth).toBeGreaterThan(dimensions.clientWidth);
  const tableMask = await page.locator("table").evaluate(
    (element) => getComputedStyle(element).maskImage,
  );
  expect(tableMask).toContain("linear-gradient");
});

test("mobile repository tabs signal that more tabs are available", async ({ page }) => {
  await renderAtMobileWidth(page, `
    <nav class="repo-tabs">
      ${["Overview", "Code", "Issues (4)", "Pull requests", "Beads", "Runs", "Releases", "Security", "Pulse", "Settings"]
        .map((label) => `<a class="repo-tab">${label}</a>`)
        .join("")}
    </nav>
  `);

  const rail = await page.locator(".repo-tabs").evaluate((element) => ({
    clientWidth: element.clientWidth,
    scrollWidth: element.scrollWidth,
    maskImage: getComputedStyle(element).maskImage,
  }));
  expect(rail.scrollWidth).toBeGreaterThan(rail.clientWidth);
  expect(rail.maskImage).toContain("linear-gradient");
});

test("authenticated mobile navigation collapses into a two-row header", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.setContent(`
    <!doctype html>
    <html data-theme="terminal-warmth">
      <head><style>${styles}</style></head>
      <body>
        <nav class="nav"><div class="nav-inner">
          <a class="nav-brand"><span class="nav-logo"></span><span>Cave</span></a>
          <div class="nav-right nav-right-auth">
            <form class="nav-search"><input class="nav-search-input" placeholder="Search code..."></form>
            <details class="nav-menu" open>
              <summary class="nav-menu-toggle">Menu</summary>
              <div class="nav-menu-content">
                <a class="btn">Notifications</a><a class="btn">Explore</a><a class="btn">New org</a>
                <a class="btn">Settings</a><a class="btn">Admin</a><span class="nav-user">atgreen</span>
                <button class="btn">Sign out</button>
              </div>
            </details>
          </div>
        </div></nav>
        <script>${navigationScript}</script>
      </body>
    </html>
  `);

  const closed = await page.locator(".nav").evaluate((nav) => ({
    height: nav.getBoundingClientRect().height,
    toggleDisplay: getComputedStyle(nav.querySelector(".nav-menu-toggle")).display,
    menuDisplay: getComputedStyle(nav.querySelector(".nav-menu-content")).display,
  }));
  expect(closed.height).toBeLessThanOrEqual(110);
  expect(closed.toggleDisplay).toBe("inline-flex");
  expect(closed.menuDisplay).toBe("none");

  await page.locator(".nav-menu-toggle").click();
  await expect(page.locator(".nav-menu-content")).toBeVisible();
});

test("authenticated desktop navigation keeps its actions visible", async ({ page }) => {
  await renderAtDesktopWidth(page, `
    <nav class="nav"><div class="nav-inner">
      <a class="nav-brand"><span class="nav-logo"></span><span>Cave</span></a>
      <div class="nav-right nav-right-auth">
        <form class="nav-search"><input class="nav-search-input" placeholder="Search code..."></form>
        <details class="nav-menu" open>
          <summary class="nav-menu-toggle">Menu</summary>
          <div class="nav-menu-content">
            <a class="btn">Notifications</a><a class="btn">Explore</a><a class="btn">New org</a>
            <a class="btn">Settings</a><a class="btn">Admin</a><span class="nav-user">atgreen</span>
            <button class="btn">Sign out</button>
          </div>
        </details>
      </div>
    </div></nav><script>${navigationScript}</script>
  `);

  await expect(page.locator(".nav-menu-toggle")).toBeHidden();
  await expect(page.locator(".nav-menu-content")).toBeVisible();
  const height = await page.locator(".nav").evaluate((element) => element.getBoundingClientRect().height);
  expect(height).toBeLessThanOrEqual(60);
});

test("single-purpose desktop forms use a focused reading measure", async ({ page }) => {
  await renderAtDesktopWidth(page, `
    <main class="container"><div class="form-page">
      <h1>New repository</h1>
      <form><div class="field"><label>Name</label><input></div></form>
    </div></main>
  `);

  const width = await page.locator(".form-page").evaluate((element) => element.getBoundingClientRect().width);
  expect(width).toBeLessThanOrEqual(768);
});

test("mobile dashboard repository rows read as three calm lines", async ({ page }) => {
  await renderAtMobileWidth(page, `
    <ul class="repo-list dashboard-repo-list">
      <li>
        <div class="dashboard-repo-heading"><a>cave</a><span class="badge">private</span></div>
        <span class="desc">A deliberately long repository description that should not compete with metadata.</span>
        <span class="repo-meta">Updated 2 hours ago</span>
      </li>
    </ul>
  `);

  const positions = await page.locator(".dashboard-repo-list li").evaluate((row) => {
    const children = [...row.children].map((child) => child.getBoundingClientRect());
    return {
      display: getComputedStyle(row).display,
      y: children.map((child) => Math.round(child.y)),
      lineClamp: getComputedStyle(row.querySelector(".desc")).webkitLineClamp,
    };
  });
  expect(positions.display).toBe("grid");
  expect(positions.y[1]).toBeGreaterThan(positions.y[0]);
  expect(positions.y[2]).toBeGreaterThan(positions.y[1]);
  expect(positions.lineClamp).toBe("2");
});
