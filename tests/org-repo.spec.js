// Org and repo workflow tests with screenshots
const { test, expect } = require("@playwright/test");
const { login, screenshot, submitForm } = require("./helpers");

const suffix = Date.now().toString(36);

test.describe("org and repo workflows", () => {
  test.beforeEach(async ({ page }) => {
    await login(page);
  });

  test("create org and view it", async ({ page }) => {
    await page.goto("/-/new-org");
    await page.fill("#name", `org${suffix}`);
    await page.fill("#display_name", "Test Organization");
    await page.fill("#description", "An org for testing");
    await submitForm(page);

    await expect(page).toHaveURL(`/o/org${suffix}`);
    await expect(page.locator("h1")).toHaveText("Test Organization");
    await screenshot(page, "org-page");
  });

  test("create personal repo and view it", async ({ page }) => {
    await page.goto("/-/new-repo");
    await page.fill("#name", `repo-${suffix}`);
    await page.fill("#description", "A test repository");
    await submitForm(page);

    await expect(page).toHaveURL(`/admin/repo-${suffix}`);
    await screenshot(page, "repo-page");
  });

  test("user profile page", async ({ page }) => {
    await page.goto("/u/admin");
    await expect(page.locator("h1")).toHaveText("admin");
    await screenshot(page, "user-profile");
  });
});
