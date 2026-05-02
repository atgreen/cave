// Test file browser, tree, and blob views
const { test, expect } = require("@playwright/test");
const { login, screenshot } = require("./helpers");

test.describe("file browser", () => {
  test.beforeEach(async ({ page }) => {
    await login(page);
  });

  test("repo page shows file tree and README", async ({ page }) => {
    await page.goto("/admin/test-browse");
    await expect(page.locator("h2", { hasText: "Files" })).toBeVisible();
    await screenshot(page, "repo-with-files");
  });

  test("tree page loads for subdirectory", async ({ page }) => {
    await page.goto("/admin/test-browse");
    // Click on the "src" directory
    await page.click('.file-tree .file-dir a');
    await expect(page.locator("table.file-tree")).toBeVisible();
    await screenshot(page, "tree-subdir");
  });

  test("blob page loads with Monaco editor", async ({ page }) => {
    await page.goto("/admin/test-browse/blob/main?path=src/hello.lisp");
    await page.waitForSelector("#editor-container", { timeout: 10000 });
    // Wait for Monaco to initialize
    await page.waitForTimeout(2000);
    await screenshot(page, "blob-monaco");
  });

  test("blob page for README renders markdown", async ({ page }) => {
    await page.goto("/admin/test-browse/blob/main?path=README.md");
    await page.waitForSelector("#editor-container", { timeout: 10000 });
    await page.waitForTimeout(2000);
    await screenshot(page, "blob-readme");
  });

  test("raw endpoint returns file content", async ({ page }) => {
    const response = await page.goto("/admin/test-browse/raw/main?path=README.md");
    expect(response.status()).toBe(200);
    const text = await response.text();
    expect(text).toContain("# Test Repository");
  });
});
