// Shared test helpers for Cave Playwright tests

const ADMIN_USER = process.env.CAVE_ADMIN_USER || "admin";
const ADMIN_PASS = process.env.CAVE_ADMIN_PASSWORD || "admin";

/**
 * Log in as the admin user and return the authenticated page.
 */
async function login(page) {
  await page.goto("/login");
  await page.fill("#username", ADMIN_USER);
  await page.fill("#password", ADMIN_PASS);
  await Promise.all([
    page.waitForNavigation(),
    page.click('.auth-form button[type="submit"]'),
  ]);
  return page;
}

/**
 * Submit the main form on the page (not the nav logout form).
 * Waits for navigation to complete.
 */
async function submitForm(page) {
  await Promise.all([
    page.waitForNavigation(),
    page.click('main button[type="submit"]'),
  ]);
}

/**
 * Take a named screenshot and save to screenshots/ directory.
 */
async function screenshot(page, name) {
  await page.screenshot({
    path: `screenshots/${name}.png`,
    fullPage: true,
  });
}

module.exports = { login, screenshot, submitForm, ADMIN_USER, ADMIN_PASS };
