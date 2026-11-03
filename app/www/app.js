/* Keep the workspace navigation in step with Shiny's tab selection. */
$(function () {
  const links = { data: "nav_data", groups: "nav_groups", stats: "nav_stats" };
  function markActive(value) {
    Object.entries(links).forEach(([view, id]) => {
      const link = document.getElementById(id);
      const active = view === value;
      link.classList.toggle("is-active", active);
      if (active) link.setAttribute("aria-current", "page");
      else link.removeAttribute("aria-current");
    });
    const current = document.querySelector(".breadcrumb strong");
    if (links[value] && current) {
      const titles = { data: "Data explorer", groups: "Nested groups", stats: "Descriptive statistics" };
      current.textContent = titles[value];
    }
  }
  markActive("data");
  $(document).on("shiny:inputchanged", function (event) {
    if (event.name === "view") markActive(event.value);
  });
});
