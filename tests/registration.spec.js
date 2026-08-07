// Self-registration via Cave's native /-/register (embedded Usher).
// The account is created in Usher and awaits admin approval before first sign-in.
// (Unlike the old Keycloak flow, Cave's register does not send a verification
// email — email verification is a separate Usher concern, not exercised here.)
const { test, expect } = require("@playwright/test");
const { screenshot } = require("./helpers");

test("self-registration creates an account and rejects duplicates", async ({ page }) => {
  const suffix = Date.now().toString(36);
  const username = `testuser-${suffix}`;
  const email = `${username}@example.com`;

  // Registration form loads.
  await page.goto("/-/register");
  await expect(page.locator("h1")).toHaveText("Create an account");
  await screenshot(page, "registration-form");

  // A valid new registration succeeds and redirects to sign-in. Cave's
  // /-/register redirects to /-/auth/login, which (being logged-out) immediately
  // redirects on to the embedded Usher /authorize sign-in page, so that is the
  // URL we land on.
  await page.fill('input[name="username"]', username);
  await page.fill('input[name="email"]', email);
  await page.fill('input[name="password"]', "testpass123");
  await Promise.all([
    page.waitForURL("**/authorize**"),
    page.click('button[type="submit"]'),
  ]);

  // Re-registering the same username is rejected.
  await page.goto("/-/register");
  await page.fill('input[name="username"]', username);
  await page.fill('input[name="email"]', email);
  await page.fill('input[name="password"]', "testpass123");
  await page.click('button[type="submit"]');
  await expect(page.locator("body")).toContainText("already taken");
  await screenshot(page, "registration-duplicate");
});
