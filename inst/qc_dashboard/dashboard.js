// Offline scope, source distributions, and indexed lazy evidence panels.
(function () {
  "use strict";
  var tables = {}, tableColumns = {}, plots = {}, selection = {}, snapshot, indexes;
  var group = "", view = "histogram", selectedRecord = null;
  var wholeSnapshot = ["bg-qc-active", "bg-qc-reconciliation", "bg-qc-issues",
    "bg-qc-manifests", "bg-qc-manifest-exceptions"];
  var dynamic = ["bg-qc-participants", "bg-qc-outputs", "bg-qc-attention", "bg-qc-metric-review"];

  // Escape a saved scalar for HTML; output is text, never executable markup.
  function escape(value) {
    return String(value == null ? "Unavailable" : value).replace(/[&<>"']/g, function (character) {
      return {"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;"}[character];
    });
  }
  // Index row arrays by a join key; output preserves all associations.
  function indexRows(rows, key) {
    var result = new Map();
    rows.forEach(function (row) {
      if (!result.has(row[key])) result.set(row[key], []);
      result.get(row[key]).push(row);
    });
    return result;
  }
  // Read and index the embedded snapshot once, without filesystem or network I/O.
  function data() {
    if (snapshot) return snapshot;
    var element = document.getElementById("bg-qc-data");
    if (!element) return null;
    snapshot = JSON.parse(element.textContent);
    indexes = {
      records: indexRows(snapshot.inventory, "record_id"),
      metrics: indexRows(snapshot.metrics, "record_id"),
      evidence: indexRows(snapshot.evidence, "record_id"),
      participantEvidence: indexRows(snapshot.evidence, "sub_id"),
      checks: indexRows(snapshot.checks, "record_id"),
      manifests: indexRows(snapshot.manifests, "path"),
      jobs: indexRows(snapshot.jobs, "job_id"),
      logs: indexRows(snapshot.job_links, "path")
    };
    return snapshot;
  }
  // Select exact recorded identities; absent fields retain their own table grain.
  function scoped(rows, filters) {
    var active = filters || selection;
    return rows.filter(function (row) {
      return Object.keys(active).every(function (field) {
        if (!active[field] || !Object.prototype.hasOwnProperty.call(row, field)) return true;
        return active[field] === "__qc_missing__" ? row[field] == null : row[field] === active[field];
      });
    });
  }
  // Deduplicate scoped source/group pairs, preserving records with unlocated sources.
  function sources(rows) {
    var seen = new Set();
    return rows.filter(function (row) {
      var key = JSON.stringify([row.source_file || row.record_id, row.group_id]);
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }
  // Retain upstream FD for the selected outputs through their saved input chain.
  // Acquisition coverage also supports stages (such as AROMA) without output rows.
  function metricScope(rows, filters) {
    var active = filters || selection;
    var saved = data();
    if (!active.stage && !active.stream && !active.space) return scoped(rows, active);
    var selected = scoped(saved.inventory, active), paths = indexRows(saved.inventory, "derivative_file"), ancestors = new Set();
    selected.forEach(function (row) {
      var visited = new Set(), pending = [row];
      while (pending.length) {
        var current = pending.pop();
        if (visited.has(current.record_id)) continue;
        visited.add(current.record_id); ancestors.add(current.record_id);
        pending.push.apply(pending, paths.get(current.source_file) || []);
      }
    });
    var identities = ["sub_id", "ses_id", "task", "run", "acquisition", "reconstruction", "direction", "echo", "part"];
    var key = function (row) { return JSON.stringify(identities.map(function (field) { return row[field] ?? null; })); };
    var acquisitions = new Set((active.space ? [] : scoped(saved.coverage || [], active)).filter(function (row) {
      return row.source_file != null;
    }).map(key));
    return rows.filter(function (row) {
      if (row.metric !== "mean_fd") return scoped([row], active).length > 0;
      var identityScope = Object.assign({}, row);
      delete identityScope.stage; delete identityScope.stream; delete identityScope.space;
      return scoped([identityScope], active).length > 0 && (ancestors.has(row.record_id) || acquisitions.has(key(row)));
    });
  }
  // Group scoped products by acquisition/context/reason, retaining drill-down IDs.
  function attention(rows) {
    var result = new Map();
    rows.filter(function (row) { return row.attention; }).forEach(function (row) {
      (row.attention_reasons || "Recorded attention flag").split(/;\s*/).forEach(function (reason) {
        var fields = ["sub_id", "ses_id", "task", "run", "acquisition", "reconstruction", "direction", "echo", "part", "stage", "stream"];
        var key = JSON.stringify(fields.map(function (field) { return row[field]; }).concat(reason));
        if (!result.has(key)) {
          var item = {reason: reason, products: 0, record_ids: ""};
          fields.forEach(function (field) { item[field] = row[field]; });
          item.category = /missing|not run|unavailable|invalid|ambiguous/i.test(reason) ? "Missing/invalid evidence" :
            /failed|error|blocked|cancelled/i.test(reason) ? "Computational/workflow failure" : "Descriptive review";
          result.set(key, item);
        }
        var item = result.get(key), ids = item.record_ids ? item.record_ids.split("\n") : [];
        if (!ids.includes(row.record_id)) ids.push(row.record_id);
        item.record_ids = ids.join("\n");
        item.products = ids.length;
      });
    });
    return Array.from(result.values());
  }
  // Include unresolved acquisitions/units and unacknowledged saved submissions.
  // These review items can legitimately have no derivative record to expand.
  function reviewQueue(inventory, coverage, metadata) {
    var result = attention(inventory);
    (coverage || []).filter(function (row) { return row.attention; }).forEach(function (row) {
      result.push(Object.assign({}, row, {category: "Coverage/workflow unresolved", reason: row.reason,
        products: row.resolved_outputs, record_ids: row.record_ids || ""}));
    });
    var submission = metadata.submission || {};
    if (metadata.scope === "run" && (submission.state !== "submitted" || !(submission.acknowledged_jobs > 0))) {
      result.push({category: "Submission unconfirmed", products: 0, record_ids: "",
        reason: "Saved request has incomplete or unavailable submission acknowledgements; existing files do not confirm execution. Inspect run provenance and job recovery before resubmitting."});
    }
    return result;
  }
  // Apply shared selections to registered static tables at their recorded grain.
  function apply(id, fields) {
    if (!window.Reactable || wholeSnapshot.includes(id)) return;
    fields.forEach(function (field) {
      var value = selection[field];
      window.Reactable.setFilter(id, field,
        value ? {qcExact: value === "__qc_missing__" ? null : value} : undefined);
    });
  }
  // Replace a registered derived table's rows and clear stale expanded panels.
  function setRows(id, rows) {
    if (tables[id] && window.Reactable) window.Reactable.setData(id, rows, {resetExpanded: true});
  }
  // Recompute scoped counters and review summaries from canonical saved rows.
  function summarize(inventory, acquisitions, workflow, coverage) {
    var outputs = inventory.filter(function (row) { return row.record_role === "output"; });
    var review = reviewQueue(inventory, coverage, data().metadata);
    var required = outputs.filter(function (row) { return row.validation_required === true; });
    var validationSettings = data().validation_settings || [];
    var participants = new Set();
    inventory.concat(acquisitions, workflow).forEach(function (row) { if (row.sub_id != null) participants.add(row.sub_id); });
    var totals = {
      participants: participants.size, raw_bold_files: acquisitions.length,
      available_outputs: outputs.filter(function (row) { return row.output_status === "available"; }).length + " of " + outputs.length,
      passed_required_audits: required.length ? required.filter(function (row) { return row.validation_status === "passed"; }).length +
        " of " + required.length + " passed" : validationSettings.length && validationSettings.every(function (value) { return value === "FALSE"; }) ?
        "Validation not enabled" : "No required audits resolved",
      attention_records: inventory.filter(function (row) { return row.attention; }).length,
      review_items: review.length
    };
    document.querySelectorAll("[data-qc-total]").forEach(function (element) {
      element.textContent = totals[element.getAttribute("data-qc-total")];
    });
    var participantRows = Array.from(participants).sort().map(function (subject) {
      var rows = outputs.filter(function (row) { return row.sub_id === subject; });
      var units = workflow.filter(function (row) { return row.sub_id === subject; });
      return {
        sub_id: subject, raw_bold_files: acquisitions.filter(function (row) { return row.sub_id === subject; }).length,
        available_outputs: rows.filter(function (row) { return row.output_status === "available"; }).length,
        output_records: rows.length,
        passed_required_audits: rows.filter(function (row) { return row.validation_required === true && row.validation_status === "passed"; }).length,
        required_audits: rows.filter(function (row) { return row.validation_required === true; }).length,
        attention_records: inventory.filter(function (row) { return row.sub_id === subject && row.attention; }).length,
        coverage_gaps: coverage.filter(function (row) { return row.sub_id === subject && row.attention; }).length,
        review_items: review.filter(function (row) { return row.sub_id === subject; }).length,
        completed_units: units.filter(function (row) { return row.status === "COMPLETED"; }).length, workflow_units: units.length
      };
    });
    setRows("bg-qc-participants", participantRows);
    var groups = new Map();
    inventory.forEach(function (row) {
      var key = JSON.stringify([row.record_role, row.stage, row.stream]);
      if (!groups.has(key)) groups.set(key, {record_role: row.record_role, stage: row.stage, stream: row.stream,
        available: 0, records: 0, missing_or_empty: 0, attention_records: 0});
      var item = groups.get(key);
      item.records++;
      item.available += row.output_status === "available" ? 1 : 0;
      item.missing_or_empty += row.output_status !== "available" ? 1 : 0;
      item.attention_records += row.attention ? 1 : 0;
    });
    setRows("bg-qc-outputs", Array.from(groups.values()));
    setRows("bg-qc-attention", review);
  }
  // Return the selected comparison group's sources after scope filtering.
  function metricRows() {
    return sources(metricScope(data().metrics)).filter(function (row) { return row.group_id === group; });
  }
  // Update metric controls, source denominators, review rows, and distribution.
  function updateMetrics() {
    var all = data().metrics;
    var selector = document.getElementById("qc-metric-group");
    if (selector && !selector.options.length) {
      var groups = new Map();
      all.forEach(function (row) { groups.set(row.group_id, row.group_label); });
      groups.forEach(function (label, id) {
        var option = document.createElement("option");
        option.value = id; option.textContent = label; selector.appendChild(option);
      });
    }
    if (!group && all.length) group = all.find(function (row) { return row.metric === "mean_fd"; })?.group_id || all[0].group_id;
    if (selector) selector.value = group;
    var rows = metricRows(), finite = rows.filter(function (row) { return typeof row.value === "number" && Number.isFinite(row.value); });
    setRows("bg-qc-metric-review", rows);
    var counts = document.getElementById("qc-metric-counts");
    if (counts) counts.textContent = finite.length + " observed sources / " + new Set(finite.map(function (row) { return row.sub_id; })).size +
      " participants; " + (rows.length - finite.length) + " unavailable values among " + rows.length +
      " recorded sources. FD follows selected acquisitions through upstream evidence, not its producing stage. Absent diagnostic sources are not counted; consult Acquisition coverage and Needs attention.";
    var mode = document.getElementById("qc-metric-view");
    if (view === "scans" && (!selection.sub_id || selection.sub_id === "__qc_missing__")) view = "histogram";
    if (mode) {
      mode.value = view;
      mode.querySelector('option[value="scans"]').disabled = !selection.sub_id || selection.sub_id === "__qc_missing__";
    }
    if (!plots.metrics || !window.Plotly) return;
    var values = finite.map(function (row) { return row.value; });
    var unit = rows.length ? rows[0].unit || "Metric value" : "Metric value";
    var trace, layout = {showlegend: false, margin: {t: 20, b: 65, l: 65, r: 20},
      paper_bgcolor: "transparent", plot_bgcolor: "transparent",
      xaxis: {title: unit, type: "linear", autorange: true}, yaxis: {title: "Sources", autorange: true},
      annotations: finite.length ? [] : [{text: rows.length ? "All recorded values are unavailable" : "No recorded sources match this scope and comparison group",
        showarrow: false, x: 0.5, y: 0.5, xref: "paper", yref: "paper"}]};
    if (view === "ecdf") {
      var sorted = values.slice().sort(function (left, right) { return left - right; });
      trace = {type: "scatter", mode: "lines", line: {shape: "hv", color: "#168b80"},
        x: sorted, y: sorted.map(function (_, index) { return (index + 1) / sorted.length; }),
        hovertemplate: "%{x}<br>Cumulative fraction: %{y:.3f}<extra></extra>"};
      layout.yaxis = {title: "Cumulative fraction", range: [0, 1]};
    } else if (view === "scans") {
      trace = {type: "scatter", mode: "markers", x: finite.map(function (row) {
        return [row.ses_id || "no session", row.task || "no task", row.run || "no run"].concat(
          ["acquisition", "reconstruction", "direction", "echo", "part"].filter(function (field) { return row[field] != null; })
            .map(function (field) { return field + "-" + row[field]; })).join(" / ");
      }), y: values, customdata: finite.map(function (row) { return row.record_id; }),
        text: finite.map(function (row) { return escape(row.value) + " " + escape(row.unit) +
          "<br>Observed frames/samples: " + escape(row.n_observations) + "; missing: " + escape(row.n_missing); }),
        hovertemplate: "%{x}<br>%{text}<extra></extra>", marker: {color: "#168b80", size: 11}};
      layout.xaxis = {title: "Session / task / run", type: "category", automargin: true, autorange: true,
        categoryorder: "array", categoryarray: trace.x};
      layout.yaxis = {title: unit, autorange: true};
    } else {
      trace = {type: "histogram", x: values, marker: {color: "#168b80"},
        hovertemplate: "%{x}<br>Sources: %{y}<extra></extra>"};
    }
    window.Plotly.react(plots.metrics, [trace], layout, {responsive: true, displaylogo: false});
  }
  // Rebuild the scoped workflow status plot without inventing scan identities.
  function updateWorkflow(rows) {
    if (!plots.workflow || !window.Plotly) return;
    var colors = {COMPLETED: "#168b80", RUNNING: "#468acf", QUEUED: "#93bfe8", FAILED: "#cf5360",
      BLOCKED: "#b36e35", CANCELLED: "#9b779d", NOT_TRACKED: "#c8d1d9", UNKNOWN: "#8a98a5"};
    var statuses = new Map(), stages = new Set();
    rows.forEach(function (row) {
      var label = row.stage + (row.stream ? " / " + row.stream : "");
      stages.add(label);
      if (!statuses.has(row.status)) statuses.set(row.status, new Map());
      var counts = statuses.get(row.status);
      counts.set(label, (counts.get(label) || 0) + 1);
    });
    var traces = Array.from(statuses, function (entry) {
      return {type: "bar", orientation: "h", name: entry[0], y: Array.from(stages),
        marker: {color: colors[entry[0]] || "#8a98a5"},
        x: Array.from(stages, function (stage) { return entry[1].get(stage) || 0; })};
    });
    window.Plotly.react(plots.workflow, traces, {barmode: "stack", margin: {l: 110, r: 15, t: 15, b: 50},
      xaxis: {title: "Workflow units (recorded identity fields)"}, yaxis: {automargin: true},
      legend: {orientation: "h"}, paper_bgcolor: "transparent", plot_bgcolor: "transparent"}, {responsive: true, displaylogo: false});
  }
  // Synchronize page controls and rebuild applicable views from shared scope.
  function update() {
    document.querySelectorAll("[data-qc-filter]").forEach(function (control) {
      control.value = selection[control.getAttribute("data-qc-filter")] || "";
    });
    var labels = Object.keys(selection).filter(function (field) { return selection[field]; }).map(function (field) {
      return field + ": " + (selection[field] === "__qc_missing__" ? "No label" : selection[field]);
    });
    document.querySelectorAll("[data-qc-scope-note]").forEach(function (note) {
      note.textContent = (labels.length ? labels.join("; ") : "All records shown") +
        ". Counts, charts and tables use recorded identity fields. Raw acquisitions lack stage/stream/space; workflow units lack task/space. Table searches affect only that table.";
    });
    Object.keys(tables).forEach(function (id) { if (!dynamic.includes(id)) apply(id, tables[id]); });
    if (!data()) return;
    var status = document.getElementById("qc-current-scope");
    if (!status) {
      status = document.createElement("div"); status.id = "qc-current-scope"; status.className = "qc-current-scope";
      status.innerHTML = '<span aria-live="polite"></span><button type="button" data-qc-edit-scope>Edit scope</button><button type="button" data-qc-reset>Reset</button>';
      document.body.appendChild(status);
    }
    var text = "Scope: " + (labels.length ? labels.join("; ") : "All records") +
      (snapshot.metadata.scope === "run" ? " | Run: " + snapshot.metadata.run_id : " | Project-wide") + " | Snapshot: " + snapshot.metadata.created_at +
      (snapshot.metadata.scheduler_refreshed ? " | Scheduler compared at collection" : " | Scheduler not queried");
    status.querySelector("span").textContent = text; status.querySelector("span").title = text;
    var inventory = scoped(snapshot.inventory), acquisitions = scoped(snapshot.acquisitions), workflow = scoped(snapshot.workflow);
    summarize(inventory, acquisitions, workflow, scoped(snapshot.coverage || []));
    updateMetrics();
    updateWorkflow(workflow);
    updateParticipant();
  }
  // Render small saved-detail rows as escaped, horizontally scrollable HTML.
  function simpleTable(rows, fields) {
    if (!rows.length) return "<p>No saved records for this output.</p>";
    return '<div class="qc-detail-table"><table><thead><tr>' + fields.map(function (field) {
      return "<th>" + escape(field.replace(/_/g, " ")) + "</th>";
    }).join("") + "</tr></thead><tbody>" + rows.map(function (row) {
      return "<tr>" + fields.map(function (field) { return "<td>" + escape(row[field]) + "</td>"; }).join("") + "</tr>";
    }).join("") + "</tbody></table></div>";
  }
  // Render precomputed evidence links; missing targets remain explicit plain text.
  function links(rows) {
    if (!rows.length) return "<p>No linked evidence was recorded.</p>";
    var seen = new Set();
    return '<ul class="qc-evidence">' + rows.filter(function (row) {
      var key = JSON.stringify([row.kind, row.path]);
      if (seen.has(key)) return false; seen.add(key); return true;
    }).map(function (row) {
      var label = escape((row.kind || "Evidence").replace(/_/g, " ")) + " · " + escape(row.path?.split("/").pop());
      return "<li>" + (row.exists && row.href ? '<a href="' + escape(row.href) +
        '" target="_blank" rel="noopener">' + label + "</a>" : "Missing: " + label) +
        '<small class="qc-path">' + escape(row.path) + "</small></li>";
    }).join("") + "</ul>";
  }
  // Render saved check messages and scalar measurements only on demand.
  function checksHTML(rows) {
    if (!rows.length) return "<p>No saved computational checks for this output. Absence is not a pass.</p>";
    return rows.map(function (row) {
      var details;
      try { details = JSON.parse(row.details_json); } catch (_) { details = null; }
      var scalars = details && typeof details === "object" ? Object.keys(details).filter(function (field) {
        return details[field] == null || typeof details[field] !== "object";
      }) : [];
      return '<section class="qc-check"><h4>' + escape(row.step) + " · " + escape(row.status) +
        '</h4><p class="qc-message">' + escape(row.message) + "</p>" +
        (row.warnings ? '<p class="qc-warning">Warnings: ' + escape(row.warnings) + "</p>" : "") +
        "<p>Audit recorded: " + escape(row.audited_at) + " | Started: " + escape(row.started_at) +
        " | Elapsed seconds: " + escape(row.elapsed_seconds) + "</p><dl>" +
        scalars.map(function (field) { return "<dt>" + escape(field) + "</dt><dd>" + escape(details[field]) + "</dd>"; }).join("") + "</dl></section>";
    }).join("");
  }
  // Look up one output's indexed evidence, checks, metrics, and manifests.
  function recordHTML(id) {
    var row = (indexes.records.get(id) || [])[0];
    if (!row) return "<p>No derivative association is recorded.</p>";
    return "<h4>" + escape(row.product) + "</h4>" +
      "<p><strong>Availability (at collection):</strong> " + escape(row.output_status) +
      " | Saved audit: " + escape(row.validation_status) + " | Diagnostics: " + escape(row.diagnostics_status) + "</p>" +
      "<p><strong>Recorded attention reasons:</strong> " + escape(row.attention_reasons || "No recorded flags; this is not a scientific QC pass.") + "</p>" +
      links(indexes.evidence.get(id) || []) +
      simpleTable([row], ["sub_id", "ses_id", "task", "run", "acquisition", "reconstruction", "direction", "echo", "part", "stage", "stream", "space", "atlas"]) +
      "<p>Source: " + escape(row.source_file) + " | Modified: " + escape(row.modified_at) + "</p>" +
      "<p>Input stream: " + escape(row.input_stream) + " | Association: " + escape(row.association_status) + "</p>" +
      "<p>Workflow context: " + escape(row.workflow_status) + " | Scope: " + escape(row.workflow_scope) +
      " | Pipeline run: " + escape(row.workflow_run_id) + ". This does not identify the attempt that produced the file.</p>" +
      "<h4>Saved measurements</h4>" + simpleTable(indexes.metrics.get(id) || [],
        ["metric", "value", "unit", "definition", "n_observations", "n_missing"]) +
      "<h4>Computational checks</h4>" + checksHTML(indexes.checks.get(id) || []) +
      "<h4>Manifest associations</h4>" + simpleTable(indexes.manifests.get(row.derivative_file) || [],
        ["job_id", "run_id", "attempt", "is_current_attempt", "status"]);
  }
  // Resolve an expanded table row to its indexed, escaped detail markup.
  function detail(row, kind) {
    if (!data()) return "Snapshot details are not available.";
    if (kind === "attention" && !row.record_ids) return "<p>" + escape(row.reason) +
      "</p><p>No resolved derivative association. Consult Acquisition coverage, workflow inventory, and saved run provenance; absence is not a pass.</p>";
    if (kind === "attention") return row.record_ids.split("\n").map(function (id) {
      return '<details><summary>' + escape((indexes.records.get(id) || [{}])[0].product) +
        "</summary>" + recordHTML(id) + "</details>";
    }).join("");
    if (kind === "check") return checksHTML((indexes.checks.get(row.record_id) || []).filter(function (check) {
      return check.step === row.step;
    }));
    if (kind === "job") {
      var job = (indexes.jobs.get(row.job_id) || []).find(function (item) {
        return item.sequence_id === row.sequence_id && item.attempt === row.attempt;
      }) || {};
      return links(["stdout_log", "stderr_log", "job_manifest_path", "runtime_receipt_path", "provenance_path"].map(function (field) {
        var path = job[field], saved = (indexes.logs.get(path) || [])[0];
        return saved ? Object.assign({kind: field}, saved) : null;
      }).filter(Boolean));
    }
    return recordHTML(row.record_id);
  }
  // Render selected-participant context and one chosen acquisition/output.
  function updateParticipant() {
    var pane = document.getElementById("qc-participant-detail");
    if (!pane) return;
    var participant = selection.sub_id;
    if (!participant || participant === "__qc_missing__") {
      pane.innerHTML = "<p>Select a participant in the review list to inspect individual scans, computational checks, and original reports.</p>";
      return;
    }
    var rows = scoped(data().inventory);
    var selected = (indexes.records.get(selectedRecord) || [])[0];
    var record = rows.find(function (row) { return row.record_id === selectedRecord; }) ||
      (selected && selected.sub_id === participant ? selected : rows[0]);
    var supporting = record && !rows.some(function (row) { return row.record_id === record.record_id; });
    var choices = supporting ? [record].concat(rows) : rows;
    var reports = (indexes.participantEvidence.get(participant) || []).filter(function (row) {
      return /\.html?$/i.test(row.path || "");
    });
    pane.innerHTML = "<h3>sub-" + escape(participant) + '</h3><button type="button" data-qc-cohort>Return to cohort</button>' +
      '<details><summary>Original MRIQC / fMRIPrep reports (participant-wide)</summary>' +
      (reports.length ? links(reports) : "<p>No linked HTML reports were recorded.</p>") + "</details>" +
      (supporting ? '<p class="qc-note">Selected upstream measurement supports the filtered acquisitions. Its original source is shown without changing the output-stage selection.</p>' : "") +
      "<label for='qc-detail-output'>Output / acquisition</label><select id='qc-detail-output'>" +
      choices.map(function (row) { return '<option value="' + escape(row.record_id) + '"' +
        (row.record_id === record?.record_id ? " selected" : "") + ">" + escape([
          row.task && "task-" + row.task, row.run && "run-" + row.run,
          row.echo && "echo-" + row.echo, row.stage, row.stream, row.space, row.atlas && row.atlas.split("/").pop()
        ].filter(Boolean).join(" / ")) + " — " + escape(row.product) + "</option>"; }).join("") +
      "</select>" + (record ? recordHTML(record.record_id) : "<p>No derivative records match this scope.</p>");
  }
  // Select a review identity and navigate to its participant scan/detail view.
  function inspect(id, participant) {
    if (!data()) return;
    var row = (indexes.records.get(id) || [])[0];
    selection.sub_id = row ? row.sub_id : participant;
    selectedRecord = row ? id : null; view = "scans"; update();
    var tab = document.querySelector('a[data-bs-toggle="tab"][href="#qc-metrics"]');
    if (tab) tab.click();
    scrollMetrics();
  }
  // Restore the chart context after changing participant/cohort view or tabs.
  function scrollMetrics() {
    requestAnimationFrame(function () {
      var pane = document.getElementById("qc-metric-controls");
      if (pane) { pane.scrollIntoView({block: "start"}); pane.focus({preventScroll: true}); }
    });
  }
  // Download every matching table row, including off-page rows and empty headers.
  function exportTable(id) {
    var state = window.Reactable.getState(id);
    var rows = state.sortedData || [];
    var fields = (tableColumns[id] || Object.keys(rows[0] || {})).filter(function (field) {
      return !["href", "record_ids"].includes(field);
    });
    function quote(value) {
      return value == null ? "NA" : '"' + String(value).replace(/"/g, '""') + '"';
    }
    var text = fields.map(quote).join("\t") + "\n" +
      rows.map(function (row) { return fields.map(function (field) { return quote(row[field]); }).join("\t"); }).join("\n");
    var url = URL.createObjectURL(new Blob([text], {type: "text/tab-separated-values;charset=utf-8"}));
    var link = document.createElement("a"); link.href = url; link.download = id + "-filtered.tsv";
    link.click(); setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  }
  window.bgQC = {
    register: function (id, fields, columns) {
      fields = Array.isArray(fields) ? fields : (typeof fields === "string" ? [fields] : []);
      tables[id] = fields;
      if (Array.isArray(columns)) tableColumns[id] = columns;
      var element = document.getElementById(id);
      if (element && !element.previousElementSibling?.matches("[data-qc-table-tools]")) {
        var tools = document.createElement("div"); tools.className = "qc-table-tools"; tools.setAttribute("data-qc-table-tools", "");
        var button = document.createElement("button"); button.type = "button"; button.textContent = "Export matching rows (all pages)";
        button.setAttribute("data-qc-export", id); tools.appendChild(button);
        var note = document.createElement("small");
        note.textContent = id === "bg-qc-manifest-exceptions" ? "Preview only; full history is in manifest_files.tsv. Shared scope does not apply." : wholeSnapshot.includes(id) ? "Whole snapshot context — shared scope does not apply." :
          "Scope fields: " + (fields.join(", ") || "none; whole snapshot") + ". Search and column filters refine this table only.";
        tools.appendChild(note); element.before(tools);
      }
      if (!dynamic.includes(id)) apply(id, fields);
      if (data() && dynamic.includes(id)) update();
    },
    plot: function (element, kind) {
      plots[kind] = element;
      if (kind === "metrics") element.on("plotly_click", function (event) {
        var id = event.points[0]?.customdata;
        if (id) inspect(id);
      });
      update();
    },
    detail: detail,
    // Pure helpers are shared by deterministic offline browser regressions.
    scoped: scoped, sources: sources, metricScope: metricScope, attention: attention,
    reviewQueue: reviewQueue, exportTable: exportTable
  };
  document.addEventListener("change", function (event) {
    var target = event.target;
    if (target.matches("[data-qc-filter]")) {
      selection[target.getAttribute("data-qc-filter")] = target.value;
      selectedRecord = null; update();
    } else if (target.id === "qc-metric-group") { group = target.value; updateMetrics(); }
    else if (target.id === "qc-metric-view") { view = target.value; updateMetrics(); }
    else if (target.id === "qc-detail-output") { selectedRecord = target.value; updateParticipant(); }
  });
  document.addEventListener("click", function (event) {
    var reset = event.target.closest("[data-qc-reset]"), participant = event.target.closest("[data-qc-participant]");
    var record = event.target.closest("[data-qc-record]"), download = event.target.closest("[data-qc-export]");
    if (reset) { selection = {}; selectedRecord = null; view = "histogram"; update(); }
    else if (record) inspect(record.getAttribute("data-qc-record"));
    else if (participant) inspect(null, participant.getAttribute("data-qc-participant"));
    else if (download) exportTable(download.getAttribute("data-qc-export"));
    else if (event.target.closest("[data-qc-edit-scope]")) {
      var control = Array.from(document.querySelectorAll("[data-qc-filter]")).find(function (item) { return item.getClientRects().length; });
      if (control) { control.scrollIntoView({block: "center"}); control.focus({preventScroll: true}); }
    }
    else if (event.target.closest("[data-qc-cohort]")) {
      selection.sub_id = ""; selectedRecord = null; view = "histogram"; update();
      scrollMetrics();
    }
  });
  document.addEventListener("shown.bs.tab", function () {
    Object.values(plots).forEach(function (plot) { if (window.Plotly) window.Plotly.Plots.resize(plot); });
  });
}());
