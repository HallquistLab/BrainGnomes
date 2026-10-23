// Shared exact identity filters for offline Reactable tables. Widgets register
// after mounting; late-mounted tabs inherit the current selection.
(function () {
  "use strict";
  var tables = {};
  function apply(id, fields) {
    if (!window.Reactable) return;
    fields.forEach(function (field) {
      var control = document.getElementById("qc-filter-" + field);
      var value = control ? control.value : "";
      window.Reactable.setFilter(id, field,
        value ? { qcExact: value === "__qc_missing__" ? null : value } : undefined);
    });
  }
  function update() {
    Object.keys(tables).forEach(function (id) { apply(id, tables[id]); });
    var selected = Array.from(document.querySelectorAll("[data-qc-filter]"))
      .filter(function (control) { return control.value; })
      .map(function (control) {
        return control.previousElementSibling.textContent + ": " + control.options[control.selectedIndex].textContent;
      });
    var note = document.getElementById("qc-filter-note");
    if (note) note.textContent = (selected.length ? selected.join("; ") : "All records shown") +
      ". Shared controls filter tables with matching identity columns; snapshot totals and plots retain their own scope.";
  }
  window.bgQC = { register: function (id, fields) {
    // R can serialize one field as a string. Normalize older widget payloads
    // too, so one table cannot interrupt initialization of subsequent widgets.
    fields = Array.isArray(fields) ? fields : (typeof fields === "string" ? [fields] : []);
    tables[id] = fields;
    apply(id, fields);
  } };
  document.addEventListener("change", function (event) {
    if (event.target.matches("[data-qc-filter]")) update();
  });
  document.addEventListener("click", function (event) {
    var reset = event.target.closest("[data-qc-reset]");
    var participant = event.target.closest("[data-qc-participant]");
    if (reset) {
      document.querySelectorAll("[data-qc-filter]").forEach(function (control) { control.value = ""; });
      update();
    } else if (participant) {
      var control = document.getElementById("qc-filter-sub_id");
      if (control) { control.value = participant.getAttribute("data-qc-participant"); update(); }
    }
  });
}());
