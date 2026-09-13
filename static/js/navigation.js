(function () {
  "use strict";

  const mobileNavigation = window.matchMedia("(max-width: 720px)");

  function syncNavigationMenu(menu) {
    if (mobileNavigation.matches) {
      menu.removeAttribute("open");
    } else {
      menu.setAttribute("open", "");
    }
  }

  function initializeNavigation() {
    const menus = document.querySelectorAll(".nav-menu");
    menus.forEach(syncNavigationMenu);
    mobileNavigation.addEventListener("change", function () {
      menus.forEach(syncNavigationMenu);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeNavigation);
  } else {
    initializeNavigation();
  }
})();
