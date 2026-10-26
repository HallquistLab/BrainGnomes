// Execute offline browser logic without R, a scheduler, or network access.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const calls = [], events = {};
let payload = null;
const controls = ["sub_id", "ses_id", "sub_id"].map(field => ({
  value: "", getAttribute: () => field
}));
const notes = [{textContent: ""}, {textContent: ""}];
const document = {
  getElementById: id => id === "bg-qc-data" && payload ? {textContent: JSON.stringify(payload)} : null,
  addEventListener: (name, handler) => { events[name] = handler; },
  querySelectorAll: selector => selector === "[data-qc-filter]" ? controls :
    selector === "[data-qc-scope-note]" ? notes : []
};
let exported = "";
const context = {
  window: {Reactable: {
    setFilter: (id, field, filter) => calls.push({id, field, filter}),
    getState: () => ({sortedData: [{sub_id: "001", value: 2}, {sub_id: "002", value: 'a"b\tc'}], pageRows: []})
  }},
  document, setTimeout: fn => fn(), Map, Set,
  URL: {createObjectURL: blob => { exported = blob.text; return "blob:test"; }, revokeObjectURL: () => {}},
  Blob: class { constructor(parts) { this.text = parts.join(""); } }
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(process.argv[2], "utf8"), context);
const api = context.window.bgQC;
api.register("empty", []);
api.register("participant", "sub_id");
api.register("later-metrics", ["sub_id", "ses_id"]);
api.register("unset", null);
assert.equal(calls.length, 3);
function change(field, value) {
  events.change({target: {value, matches: () => true, getAttribute: () => field}});
}
change("sub_id", "001");
change("ses_id", "__qc_missing__");
assert.equal(calls.at(-1).filter.qcExact, null);
assert.match(notes[0].textContent, /sub_id: 001/);
assert.equal(controls[2].value, "001"); // Scope controls on every page stay synchronized.
assert.equal(api.scoped([{sub_id: "001", ses_id: null}, {sub_id: "001", ses_id: "A"},
  {sub_id: "002", ses_id: null}]).length, 1);
assert.equal(api.scoped([{stage: "setup"}]).length, 1); // No invented acquisition grain.
api.register("late-widget", ["sub_id"]);
assert.equal(calls.at(-1).filter.qcExact, "001");
const before = calls.length;
api.register("bg-qc-issues", ["sub_id"]);
assert.equal(calls.length, before); // Whole-snapshot context is explicitly unfiltered.
const observations = [
  {source_file: "/a", group_id: "fd", record_id: "a"},
  {source_file: "/a", group_id: "fd", record_id: "b"},
  {source_file: "/a", group_id: "other", record_id: "a"},
  {source_file: null, group_id: "fd", record_id: "c"},
  {source_file: null, group_id: "fd", record_id: "d"}
];
assert.equal(api.sources(observations).length, 4);
const flagged = ["a", "b"].map(id => ({record_id: id, sub_id: "001", stage: "extract_rois",
  attention: true, attention_reasons: "Saved audit failed; Diagnostics missing; Recorded ROI loss"}));
const review = api.attention(flagged);
assert.equal(review.length, 3);
assert.equal(review[0].products, 2);
assert.deepEqual(Array.from(review, row => row.category),
  ["Computational/workflow failure", "Missing/invalid evidence", "Descriptive review"]);
assert.equal(api.attention([{...flagged[0], attention_reasons: "Diagnostics invalid"}])[0].category,
  "Missing/invalid evidence");
document.createElement = () => ({click() {}});
api.exportTable("metrics");
assert.match(exported, /001/);
assert.match(exported, /002/); // All matching rows, not pageRows.
assert.match(exported, /"a""b\tc"/);
events.click({target: {closest: selector => selector === "[data-qc-reset]" ? {} : null}});
assert.equal(controls[0].value, "");
assert.equal(calls.at(-1).filter, undefined);
assert.match(notes[1].textContent, /All records shown/);
api.register("zero", [], ["metric", "value"]);
context.window.Reactable.getState = () => ({sortedData: []});
api.exportTable("zero");
assert.equal(exported, '"metric"\t"value"\n');
payload = {
  metadata: {}, coverage: [],
  inventory: [
    {record_id: "a", product: "<script>unsafe</script>", derivative_file: "/a", source_file: "/preproc",
      sub_id: "001", task: "rest", stage: "postprocess", stream: "clean", space: "MNI"},
    {record_id: "fd", derivative_file: "/preproc", sub_id: "001", task: "rest", stage: "fmriprep", stream: null, space: "MNI"},
    {record_id: "other", derivative_file: "/different", sub_id: "002", task: "rest", stage: "fmriprep", stream: null, space: "MNI"}
  ],
  acquisitions: [], workflow: [], metrics: [], evidence: [], manifests: [], jobs: [], job_links: [],
  checks: [{record_id: "a", step: "mask", status: "failed", message: '<img src=x onerror="unsafe()">',
    warnings: "<unsafe warning>", details_json: '{"scalar":3,"array":[1,2]}'}]
};
const detail = api.detail({record_id: "a"}, "record");
assert.match(detail, /&lt;script&gt;unsafe/);
assert.match(detail, /Availability \(at collection\)/);
assert.match(detail, /not a scientific QC pass/);
assert.match(detail, /&lt;img src=x onerror=/);
assert.match(detail, /&lt;unsafe warning&gt;/);
assert.doesNotMatch(detail, /<script>|<img/);
assert.match(detail, /<dt>scalar<\/dt><dd>3<\/dd>/);
assert.doesNotMatch(detail, /<dt>array/);
const fdRows = payload.inventory.filter(row => row.stage === "fmriprep").map(row =>
  ({...row, metric: "mean_fd", source_file: "/" + row.record_id + ".tsv", value: .2}));
assert.equal(api.metricScope(fdRows, {stage: "postprocess"}).length, 1);
assert.equal(api.metricScope(fdRows, {stage: "postprocess", stream: "clean"})[0].sub_id, "001");
assert.equal(api.metricScope(fdRows, {stage: "postprocess", sub_id: "002"}).length, 0);
assert.equal(api.metricScope(fdRows, {space: "unknown-space"}).length, 0);
assert.equal(api.metricScope(fdRows, {stage: "postprocess", stream: "missing"}).length, 0);
const gaps = [{sub_id: "missing", stage: "fmriprep", attention: true, resolved_outputs: 0,
  record_ids: "", reason: "Raw BOLD present but no derivative paths resolved"}];
const queue = api.reviewQueue([], gaps, {scope: "run", submission: {state: "submission_started", acknowledged_jobs: 0}});
assert.equal(queue.length, 2);
assert.match(queue[0].reason, /no derivative/);
assert.equal(queue[1].category, "Submission unconfirmed");
assert.match(api.detail(queue[0], "attention"), /No resolved derivative association/);
console.log("Shared filter regression passed");
