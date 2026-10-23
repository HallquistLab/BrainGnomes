// Exercise the shipped browser script with a minimal DOM and Reactable API.
// Inputs include legacy scalar fields; output is successful initialization,
// shared filtering, and registration of widgets mounted after a selection.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const calls = [];
const events = {};
const controls = {
  "qc-filter-sub_id": { value: "001", previousElementSibling: { textContent: "Participant" },
    options: [{ textContent: "001" }], selectedIndex: 0 },
  "qc-filter-ses_id": { value: "__qc_missing__", previousElementSibling: { textContent: "Session" },
    options: [{ textContent: "No session label" }], selectedIndex: 0 }
};
const note = { textContent: "" };
const context = {
  window: { Reactable: { setFilter: (id, field, filter) => calls.push({ id, field, filter }) } },
  document: {
    getElementById: id => id === "qc-filter-note" ? note : controls[id],
    addEventListener: (name, handler) => { events[name] = handler; },
    querySelectorAll: () => Object.values(controls)
  }
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(process.argv[2], "utf8"), context);
const register = context.window.bgQC.register;
register("empty", []);
register("participant", "sub_id"); // Legacy singleton payload that broke rendering.
register("later-metrics", ["sub_id", "ses_id"]);
register("unset", null);
assert.equal(calls.length, 3);
assert.equal(calls[0].filter.qcExact, "001");
assert.equal(calls[2].filter.qcExact, null);
events.change({ target: { matches: () => true } });
assert.match(note.textContent, /Participant: 001/);
assert.equal(calls.length, 6);
controls["qc-filter-sub_id"].value = "002";
register("late-widget", ["sub_id"]);
assert.equal(calls.at(-1).filter.qcExact, "002");
events.click({ target: { closest: selector => selector === "[data-qc-reset]" ? {} : null } });
assert.equal(controls["qc-filter-sub_id"].value, "");
assert.equal(controls["qc-filter-ses_id"].value, "");
assert.equal(calls.at(-1).filter, undefined);
assert.match(note.textContent, /All records shown/);
console.log("Shared filter regression passed");
