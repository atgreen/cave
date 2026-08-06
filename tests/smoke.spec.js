// Smoke tests: verify core pages load and capture screenshots
const { test, expect } = require("@playwright/test");
const { login, screenshot } = require("./helpers");

test.describe("public pages", () => {
  test("login redirects to the embedded Usher sign-in page", async ({ page }) => {
    await page.goto("/-/auth/login");
    // Should be on Usher's /authorize sign-in page (Cave's own origin)
    await expect(page).toHaveURL(/\/authorize\?/);
    await expect(page.locator('input[name="username"]')).toBeVisible();
    await screenshot(page, "usher-login");
  });
});

test.describe("authenticated pages", () => {
  test.beforeEach(async ({ page }) => {
    await login(page);
  });

  test("dashboard loads", async ({ page }) => {
    await expect(page.locator("h1")).toHaveText("Dashboard");
    await screenshot(page, "dashboard");
  });

  test("settings page loads", async ({ page }) => {
    await page.goto("/-/settings");
    await expect(page.locator("h1")).toHaveText("Settings");
    await screenshot(page, "settings");
  });

  test("admin page loads", async ({ page }) => {
    await page.goto("/-/admin");
    await expect(page.locator("h1")).toHaveText("Instance administration");
    await screenshot(page, "admin");
  });

  test("new org form loads", async ({ page }) => {
    await page.goto("/-/new-org");
    await expect(page.locator("h1")).toHaveText("Create organization");
    await screenshot(page, "new-org");
  });

  test("new repo form loads", async ({ page }) => {
    await page.goto("/-/new-repo");
    await expect(page.locator("h1")).toHaveText("New repository");
    await screenshot(page, "new-repo");
  });
});
