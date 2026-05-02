// Test self-registration with email verification via Keycloak + Mailpit
const { test, expect } = require("@playwright/test");
const { screenshot } = require("./helpers");

const MAILPIT = process.env.MAILPIT_URL || "http://localhost:8025";

test("new user registration sends verification email", async ({ page }) => {
  // Navigate to login, which redirects to Keycloak
  await page.goto("/-/auth/login");
  await page.waitForSelector("#kc-login");

  // Click the Register link
  await page.click('a:has-text("Register")');
  await page.waitForSelector("#firstName");

  // Fill registration form
  const suffix = Date.now().toString(36);
  const email = `testuser-${suffix}@example.com`;
  await page.fill("#firstName", "Test");
  await page.fill("#lastName", "User");
  await page.fill("#email", email);
  await page.fill("#username", `testuser-${suffix}`);
  await page.fill("#password", "testpass123");
  await page.fill("#password-confirm", "testpass123");

  await screenshot(page, "registration-form");

  // Submit registration
  await Promise.all([
    page.waitForNavigation(),
    page.click('input[type="submit"], button[type="submit"]'),
  ]);

  await screenshot(page, "verify-email-page");

  // Check that the verify-email page is shown
  const content = await page.textContent("body");
  expect(content).toContain("verify your email");

  // Check Mailpit received the verification email
  const response = await fetch(`${MAILPIT}/api/v1/messages`);
  const data = await response.json();
  expect(data.messages.length).toBeGreaterThan(0);

  const lastMessage = data.messages[0];
  expect(lastMessage.To[0].Address).toBe(email);
  expect(lastMessage.Subject).toContain("Verify email");
});
