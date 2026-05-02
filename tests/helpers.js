// Shared test helpers for Cave Playwright tests

const ADMIN_USER = process.env.CAVE_ADMIN_USER || "admin";
const ADMIN_PASS = process.env.CAVE_ADMIN_PASSWORD || "admin";

/**
 * Log in via Keycloak OIDC flow.
 * Navigates to Cave's login, fills Keycloak's form, waits for redirect back.
 */
async function login(page) {
  await page.goto("/-/auth/login");
  // Now on Keycloak login page
  await page.waitForSelector("#username");
  await page.fill("#username", ADMIN_USER);
  await page.fill("#password", ADMIN_PASS);
  await Promise.all([
    page.waitForURL("**/"),
    page.click("#kc-login"),
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
