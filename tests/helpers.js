// Shared test helpers for Cave Playwright tests

const ADMIN_USER = process.env.CAVE_ADMIN_USER || "admin";
const ADMIN_PASS = process.env.CAVE_ADMIN_PASSWORD || "admin";

/**
 * Log in via the embedded Usher OIDC flow.
 * Navigates to Cave's login, which redirects to Usher's /authorize sign-in page
 * (served on Cave's own origin), fills the form, waits for the redirect back.
 * Usher's form has no element ids — select by name / class.
 */
async function login(page) {
  await page.goto("/-/auth/login");
  // Now on the embedded Usher sign-in page
  await page.waitForSelector('input[name="username"]');
  await page.fill('input[name="username"]', ADMIN_USER);
  await page.fill('input[name="password"]', ADMIN_PASS);
  await Promise.all([
    page.waitForURL("**/"),
    page.click("button.usher-submit"),
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
