// Real Chromium walkthrough of an exported, self-contained synthetic report.
// Inputs: HTML path; optional screenshot directory; explicit local Playwright/runtime.
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const {pathToFileURL} = require("node:url");
const {chromium} = require(process.env.BRAINGNOMES_PLAYWRIGHT_MODULE || "playwright");

// Parse quoted TSV, including escaped quotes and newlines in saved messages.
function parseTSV(text) {
  const rows = [], row = [];
  let value = "", quoted = false;
  for (let index = 0; index < text.length; index++) {
    const character = text[index];
    if (character === '"') {
      if (quoted && text[index + 1] === '"') { value += '"'; index++; }
      else quoted = !quoted;
    } else if (!quoted && (character === "\t" || character === "\n")) {
      row.push(value); value = "";
      if (character === "\n") { rows.push(row.splice(0)); }
    } else value += character;
  }
  if (value || row.length) { row.push(value); rows.push(row); }
  return rows;
}

// Exercise a real file:// report without HTTP, R, external requests, or scheduler calls.
async function walkthrough() {
  const browser = await chromium.launch({headless: true,
    executablePath: process.env.BRAINGNOMES_CHROMIUM || undefined,
    args: ["--no-sandbox", "--disable-dev-shm-usage"]});
  try {
    const context = await browser.newContext({viewport: {width: 1440, height: 1000}, acceptDownloads: true});
    const page = await context.newPage(), errors = [], requests = [];
    page.setDefaultTimeout(15000);
    page.on("pageerror", error => errors.push(error.message));
    page.on("request", request => { if (/^https?:/.test(request.url())) requests.push(request.url()); });
    const started = Date.now();
    let evidenceLinkChecked = false;
    await page.goto(pathToFileURL(path.resolve(process.argv[2])).href, {waitUntil: "load"});
    await page.waitForFunction(() => window.Reactable && document.querySelector("#qc-current-scope"));
    const expected = await page.evaluate(() => {
      const snapshot = JSON.parse(document.querySelector("#bg-qc-data").textContent);
      const fd = window.bgQC.sources(snapshot.metrics).filter(row => row.metric === "mean_fd");
      return {sources: fd.length, finite: fd.filter(row => typeof row.value === "number" && Number.isFinite(row.value)).length,
        participants: new Set(snapshot.inventory.concat(snapshot.acquisitions, snapshot.workflow)
          .map(row => row.sub_id).filter(value => value != null)).size,
        firstParticipant: snapshot.inventory[0].sub_id,
        firstSources: fd.filter(row => row.sub_id === snapshot.inventory[0].sub_id).length};
    });
    await page.waitForFunction(count => Reactable.getState("bg-qc-metric-review").sortedData.length === count, expected.sources);
    assert.equal(await page.locator('[data-qc-total="participants"]').textContent(), String(expected.participants));
    const resetBox = await page.locator('#overview [data-qc-reset]').boundingBox();
    assert.ok(resetBox.x >= 0 && resetBox.x + resetBox.width <= 1440);
    assert.equal(await page.evaluate(() => {
      const ids = [...document.querySelectorAll('[id^="bg-qc-"], [id^="qc-filter-"]')].map(element => element.id);
      return new Set(ids).size === ids.length;
    }), true);
    await page.getByRole("tab", {name: "QC metrics", exact: true}).click();
    const plot = page.locator(".js-plotly-plot:visible");
    await page.waitForFunction(count => document.querySelector("#qc-metrics .js-plotly-plot").data[0].x.length === count, expected.finite);
    assert.equal(await plot.evaluate(element => element.data[0].type), "histogram");
    assert.equal(await plot.evaluate(element => element.layout.xaxis.type), "linear");
    assert.ok((await plot.boundingBox()).height >= 350);
    const headers = await page.locator('#bg-qc-metric-review .rt-thead [role="columnheader"]').allTextContents();
    assert.ok(headers.findIndex(text => text === "Participant") < headers.findIndex(text => text === "Value"));
    await page.locator("#qc-metric-view").selectOption("ecdf");
    await page.waitForFunction(() => document.querySelector("#qc-metrics .js-plotly-plot").data[0].mode === "lines");
    assert.equal(await plot.evaluate(element => element.data[0].y.at(-1)), 1);

    const download = page.waitForEvent("download");
    await page.locator('[data-qc-export="bg-qc-metric-review"]').click();
    const exported = parseTSV(await fs.readFile(await (await download).path(), "utf8"));
    assert.equal(exported.length - 1, expected.sources); // Includes off-page and unavailable rows.
    const search = page.locator('#bg-qc-metric-review input[placeholder="Search this table..."]');
    await search.fill("__no_such_metric__");
    await page.waitForFunction(() => Reactable.getState("bg-qc-metric-review").sortedData.length === 0);
    assert.equal(await plot.evaluate(element => element.data[0].x.length), expected.finite);
    const emptyDownload = page.waitForEvent("download");
    await page.locator('[data-qc-export="bg-qc-metric-review"]').click();
    const empty = parseTSV(await fs.readFile(await (await emptyDownload).path(), "utf8"));
    assert.equal(empty.length, 1); assert.ok(empty[0].includes("value"));
    await search.fill("");
    await page.locator("#qc-filter-metrics-sub_id").selectOption(expected.firstParticipant);
    await page.waitForFunction(count => Reactable.getState("bg-qc-metric-review").sortedData.length === count, expected.firstSources);
    assert.equal(await page.locator('[data-qc-total="participants"]').textContent(), "1");
    assert.deepEqual(await page.locator('[data-qc-filter="sub_id"]').evaluateAll(elements => elements.map(element => element.value)),
      Array(4).fill(expected.firstParticipant));
    await page.locator('#bg-qc-metric-review [data-qc-record]').first().click();
    await page.waitForFunction(() => document.querySelector("#qc-metric-view").value === "scans");
    assert.match(await page.locator("#qc-participant-detail").innerText(), /Computational checks/);
    const evidence = page.locator('#qc-participant-detail a[href*="fmriprep"][href$=".html"]').first();
    if (await evidence.count() && process.env.BRAINGNOMES_QC_SKIP_EVIDENCE_LINK !== "true") {
      await page.locator('#qc-participant-detail details > summary').first().click();
      const popup = context.waitForEvent("page"); await evidence.click();
      const report = await popup; await report.waitForLoadState();
      assert.match(await report.locator("body").innerText(), /Evidence report/); await report.close();
      evidenceLinkChecked = true;
    }
    await page.locator('#bg-qc-metric-review').getByRole("button", {name: "Toggle details", exact: true}).first().click();
    assert.equal(await page.locator('#bg-qc-metric-review .qc-detail').count(), 1);
    assert.equal(await page.locator('#bg-qc-metric-review .qc-detail .reactable').count(), 0); // No eager subwidgets.
    await page.locator('[data-qc-cohort]').click();
    await page.waitForFunction(count => Reactable.getState("bg-qc-metric-review").sortedData.length === count, expected.sources);
    assert.equal(await page.locator("#qc-metric-view").inputValue(), "histogram");
    await page.waitForFunction(() => {
      const rect = document.querySelector("#qc-metric-controls").getBoundingClientRect();
      return rect.top >= 0 && rect.top < innerHeight;
    });
    await page.locator("#qc-filter-metrics-stage").selectOption("postprocess");
    await page.waitForFunction(count => Reactable.getState("bg-qc-metric-review").sortedData.length === count, expected.sources);
    assert.match(await page.locator("#qc-metric-counts").textContent(), /upstream evidence/);
    await page.locator('#qc-metrics [data-qc-reset]').click();

    // Comparison changes reset axes and keep their own units/source denominators.
    const fdGroup = await page.locator("#qc-metric-group").inputValue();
    const alternative = await page.evaluate(current => {
      const snapshot = JSON.parse(document.querySelector("#bg-qc-data").textContent);
      const other = snapshot.metrics.find(row => row.group_id !== current);
      if (!other) return null;
      const rows = window.bgQC.sources(snapshot.metrics).filter(row => row.group_id === other.group_id);
      return {id: other.group_id, unit: other.unit || "Metric value", count: rows.length,
        finite: rows.filter(row => typeof row.value === "number" && Number.isFinite(row.value)).length};
    }, fdGroup);
    if (alternative) {
      await page.locator("#qc-metric-group").selectOption(alternative.id);
      await page.waitForFunction(count => Reactable.getState("bg-qc-metric-review").sortedData.length === count, alternative.count);
      assert.equal(await plot.evaluate(element => element.data[0].x.length), alternative.finite);
      assert.equal(await plot.evaluate(element => element.layout.xaxis.title.text || element.layout.xaxis.title), alternative.unit);
      await page.locator("#qc-metric-group").selectOption(fdGroup);
    }
    // Exercise every shared acquisition/processing selector, including missing identity labels.
    for (const field of ["ses_id", "task", "stage", "stream", "space"]) {
      const value = await page.evaluate(field => {
        const snapshot = JSON.parse(document.querySelector("#bg-qc-data").textContent);
        return snapshot.metrics.find(row => row.metric === "mean_fd")[field] ?? "__qc_missing__";
      }, field);
      const control = page.locator("#qc-filter-metrics-" + field);
      if (!await control.locator("option").evaluateAll((options, value) => options.some(option => option.value === value), value)) continue;
      await control.selectOption(value);
      const counts = await page.evaluate(group => {
        const snapshot = JSON.parse(document.querySelector("#bg-qc-data").textContent);
        const rows = window.bgQC.sources(window.bgQC.metricScope(snapshot.metrics)).filter(row => row.group_id === group);
        return {review: rows.length, raw: window.bgQC.scoped(snapshot.acquisitions).length,
          finite: rows.filter(row => typeof row.value === "number" && Number.isFinite(row.value)).length};
      }, fdGroup);
      await page.waitForFunction(count => Reactable.getState("bg-qc-metric-review").sortedData.length === count, counts.review);
      assert.equal(await plot.evaluate(element => element.data[0].x.length), counts.finite);
      assert.equal(await page.locator('[data-qc-total="raw_bold_files"]').textContent(), String(counts.raw));
      await page.locator('#qc-metrics [data-qc-reset]').click();
    }

    await page.getByRole("tab", {name: "Derivatives", exact: true}).click();
    await page.locator('#bg-qc-inventory').getByRole("button", {name: "Toggle details", exact: true}).first().click();
    assert.equal(await page.locator('#bg-qc-inventory .qc-detail').count(), 1);
    await page.getByRole("tab", {name: "Workflow & evidence", exact: true}).click();
    await page.getByRole("tab", {name: "Active jobs — whole snapshot", exact: true}).click();
    assert.match(await page.locator("#qc-current-scope").innerText(), /Scheduler not queried/);
    if (await page.evaluate(() => Reactable.getState("bg-qc-active").sortedData.length === 0)) {
      assert.match(await page.locator("body").innerText(), /not a live scheduler query/);
    }
    await page.getByRole("tab", {name: "Scheduler comparison — whole snapshot", exact: true}).click();
    assert.match(await page.locator("body").innerText(), /Not queried: the scheduler/);
    for (const name of ["Output manifest checks — whole snapshot", "Evidence library", "Collection issues — whole snapshot", "Downloads & interpretation"]) {
      await page.getByRole("tab", {name, exact: true}).click();
    }
    assert.match(await page.locator("body").innerText(), /complete, unfiltered snapshot/);

    const mobileContext = await browser.newContext({viewport: {width: 390, height: 844}, isMobile: true, hasTouch: true});
    const mobile = await mobileContext.newPage();
    mobile.on("pageerror", error => errors.push(error.message));
    await mobile.goto(pathToFileURL(path.resolve(process.argv[2])).href);
    await mobile.waitForSelector("#qc-current-scope");
    assert.equal(await mobile.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
    for (const box of await mobile.locator('#overview [data-qc-filter], #overview [data-qc-reset]').evaluateAll(elements =>
      elements.map(element => ({left: element.getBoundingClientRect().left, right: element.getBoundingClientRect().right})))) {
      assert.ok(box.left >= 0 && box.right <= 390);
    }
    await mobile.getByRole("button", {name: "Toggle navigation", exact: true}).click();
    await mobile.getByRole("tab", {name: "QC metrics", exact: true}).click();
    await mobile.getByRole("button", {name: "Toggle navigation", exact: true}).click();
    await mobile.locator("#qc-filter-metrics-sub_id").selectOption(expected.firstParticipant);
    await mobile.waitForFunction(() => document.querySelector('[data-qc-total="participants"]').textContent === "1");
    if (process.argv[3]) {
      await fs.mkdir(process.argv[3], {recursive: true});
      await mobile.screenshot({path: path.join(process.argv[3], "qc-mobile.png")});
      await page.getByRole("tab", {name: "QC metrics", exact: true}).click();
      await page.locator("#qc-metric-group").scrollIntoViewIfNeeded();
      await page.screenshot({path: path.join(process.argv[3], "qc-cohort.png")});
    }
    assert.deepEqual(errors, []); assert.deepEqual(requests, []);
    console.log(JSON.stringify({result: "QC browser walkthrough passed", expected, evidenceLinkChecked,
      elapsedSeconds: (Date.now() - started) / 1000}));
  } finally { await browser.close(); }
}
walkthrough().catch(error => { console.error(error); process.exitCode = 1; });
