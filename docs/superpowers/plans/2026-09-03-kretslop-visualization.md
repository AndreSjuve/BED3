# Kapitalens kretsløp — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An interactive «Slik henger kurset sammen» section on forsiden that renders the velstandshjul capital-flow story with explorable topic nodes.

**Architecture:** The diagram is HTML/CSS (a grid of station panels and topic chips) with small inline SVGs for the animated flow connectors, not one monolithic hand-drawn SVG. Without JS it is a complete static diagram whose chips are plain links. A small vanilla JS file adds hover/tap highlighting and a fixed explanation-card panel; card prose lives in one JS data object.

**Tech Stack:** Quarto website, SCSS theme (`site/theme/bed3.scss`), vanilla JS, no libraries.

**Spec:** `docs/superpowers/specs/2026-09-03-kretslop-visualization-design.md`

## Global Constraints

- No JS frameworks or libraries; no d3. The one JS file is vanilla, wrapped in an IIFE.
- Use only existing design tokens: `$bed3-paper #F5EDE3`, `$bed3-ink #2A1414`, `$bed3-sepia #4A342F`, `$bed3-muted #6B5750`, `$bed3-oxblood #6E1E1E`, `$bed3-rule #E1CEC5`, `$bed3-rule-strong #C9A9A1`, `$bed3-wash #EFE3DB`; faces `$bed3-display` / `$bed3-text`.
- No all-caps anywhere, including SVG labels. Mixed case only.
- Site prose rules: Norwegian (bokmål), no em dashes, no decorative bold, teaching voice.
- Never edit files with PowerShell redirection (`Set-Content`/`Out-File`); Norwegian characters corrupt under PS 5.1. Use the Write/Edit tools.
- `prefers-reduced-motion: reduce` must stop all ambient animation.
- Without JS the section must render legibly and every chip must be a working link.
- Build with `quarto render` from `site/`; verify in a browser via Playwright MCP tools against a local server of `site/_site`.
- The repo has no test framework; each task's test cycle is: render, drive the built page in the browser, assert with `browser_evaluate` / snapshots, screenshot review.

---

### Task 1: Static diagram markup and layout styles

The complete no-JS deliverable: the section on forsiden with three stations, topic-chip links, connector arrows, risk strip, and return loop, laid out and styled. No animation, no interactivity yet.

**Files:**
- Create: `site/_includes/kretslop.html`
- Modify: `site/index.qmd` (insert before line 53 `## Det du finner her`)
- Modify: `site/theme/bed3.scss` (append to end of file, after the `@media print` block)
- Modify: `site/theme/bed3.scss` `@media print` block (hide the section in print)

**Interfaces:**
- Produces: DOM contract used by Tasks 2–3: section `#kretslop.kretslop`, chips `a.kretslop__topic[data-topic]`, stations `[data-station="husholdningene"|"markedet"|"bedriftene"]`, connectors `.kretslop__link[data-link="til-markedet"|"til-bedriftene"]`, risk strip `.kretslop__risk`, return loop `.kretslop__return`, SVG lines `line.kretslop__flow-line` (left-to-right) and `line.kretslop__flow-line--return` (right-to-left). The `data-topic` ids: `portefoljeteori`, `baerekraft`, `kvm`, `rentemarkedet`, `valuta`, `investeringsprosjekter`, `skatt-laan`, `inflasjon`, `risikovurderinger`, `laan-betaling`, `kapitalstruktur`, `opsjoner`, `terminer`.

- [ ] **Step 1: Write `site/_includes/kretslop.html`**

```html
<div class="kretslop" id="kretslop">
  <div class="kretslop__diagram">

    <div class="kretslop__station" data-station="husholdningene">
      <h3 class="kretslop__station-name">Husholdningene</h3>
      <p class="kretslop__station-role">eier ressursene og sparer</p>
      <ul class="kretslop__topics">
        <li><a class="kretslop__topic" data-topic="portefoljeteori" href="timeplan.html">Porteføljeteori</a></li>
        <li><a class="kretslop__topic" data-topic="baerekraft" href="timeplan.html">Bærekraftige investeringer</a></li>
      </ul>
    </div>

    <div class="kretslop__link" data-link="til-markedet" aria-hidden="true">
      <svg viewBox="0 0 100 24" preserveAspectRatio="none">
        <line class="kretslop__flow-line" x1="0" y1="12" x2="90" y2="12" />
        <path class="kretslop__flow-head" d="M90 7 L100 12 L90 17 Z" />
      </svg>
      <span class="kretslop__link-label">sparing</span>
    </div>

    <div class="kretslop__station" data-station="markedet">
      <h3 class="kretslop__station-name">Kapitalmarkedet</h3>
      <p class="kretslop__station-role">setter prisen på kapital</p>
      <ul class="kretslop__topics">
        <li><a class="kretslop__topic" data-topic="kvm" href="timeplan.html">Kapitalverdimodellen</a></li>
        <li><a class="kretslop__topic" data-topic="rentemarkedet" href="kursmateriale/02-markeder/02-rentemarkedet/index.html">Rentemarkedet</a></li>
        <li><a class="kretslop__topic" data-topic="valuta" href="kursmateriale/02-markeder/05-valuta/index.html">Internasjonal finans</a></li>
      </ul>
    </div>

    <div class="kretslop__link" data-link="til-bedriftene" aria-hidden="true">
      <svg viewBox="0 0 100 24" preserveAspectRatio="none">
        <line class="kretslop__flow-line" x1="0" y1="12" x2="90" y2="12" />
        <path class="kretslop__flow-head" d="M90 7 L100 12 L90 17 Z" />
      </svg>
      <span class="kretslop__link-label">kapital</span>
    </div>

    <div class="kretslop__station" data-station="bedriftene">
      <h3 class="kretslop__station-name">Bedriftene</h3>
      <p class="kretslop__station-role">investerer i prosjekter</p>
      <ul class="kretslop__topics">
        <li><a class="kretslop__topic" data-topic="investeringsprosjekter" href="kursmateriale/01-investeringsanalyse/02-investeringsprosjekter/index.html">Investeringsprosjekter</a></li>
        <li><a class="kretslop__topic" data-topic="skatt-laan" href="kursmateriale/01-investeringsanalyse/03-skatt-og-laan/index.html">Effekter av skatt og lån</a></li>
        <li><a class="kretslop__topic" data-topic="inflasjon" href="kursmateriale/01-investeringsanalyse/04-inflasjon/index.html">Effekter av inflasjon</a></li>
        <li><a class="kretslop__topic" data-topic="risikovurderinger" href="kursmateriale/01-investeringsanalyse/05-risikovurderinger/index.html">Risikovurderinger</a></li>
        <li><a class="kretslop__topic" data-topic="laan-betaling" href="kursmateriale/01-investeringsanalyse/06-laane-og-betalingsalternativer/index.html">Låne- og betalingsalternativer</a></li>
        <li><a class="kretslop__topic" data-topic="kapitalstruktur" href="timeplan.html">Kapitalstruktur</a></li>
      </ul>
    </div>

    <div class="kretslop__risk">
      <span class="kretslop__risk-label">flytter risiko mellom aktørene</span>
      <a class="kretslop__topic" data-topic="opsjoner" href="kursmateriale/02-markeder/03-opsjoner/index.html">Opsjonskontrakter</a>
      <a class="kretslop__topic" data-topic="terminer" href="kursmateriale/02-markeder/04-terminer/index.html">Terminkontrakter</a>
    </div>

    <div class="kretslop__return" aria-hidden="true">
      <svg viewBox="0 0 100 24" preserveAspectRatio="none">
        <path class="kretslop__flow-head kretslop__flow-head--return" d="M10 7 L0 12 L10 17 Z" />
        <line class="kretslop__flow-line kretslop__flow-line--return" x1="10" y1="12" x2="100" y2="12" />
      </svg>
      <span class="kretslop__link-label">avkastningen vender tilbake</span>
    </div>

  </div>

  <div class="kretslop__card" id="kretslop-card" aria-live="polite">
    <p class="kretslop__card-hint">Pek på et tema for å se hvor det hører hjemme i kretsløpet, eller les
      <a href="kursmateriale/00-bakteppe.html">hele historien i bakteppet</a>.</p>
  </div>
</div>
```

- [ ] **Step 2: Insert the section into `site/index.qmd`**

Immediately before `## Det du finner her` (line 53), insert:

```markdown
## Slik henger kurset sammen

Kapitalen renner fra husholdningene som sparer, gjennom kapitalmarkedet som
setter en pris på den, til bedriftene som investerer den i prosjekter. Hvert
tema i kurset hører hjemme et sted i det kretsløpet.

{{< include _includes/kretslop.html >}}

<p class="note">Hele resonnementet er skrevet ut i <a href="kursmateriale/00-bakteppe.html">bakteppet</a>.</p>

```

- [ ] **Step 3: Append layout styles to `site/theme/bed3.scss`**

Append after the `@media print` block:

```scss
// =============================================================================
// Kapitalens kretsløp (forsiden)
// =============================================================================

.kretslop {
  margin-top: 2rem;
}

.kretslop__diagram {
  display: grid;
  grid-template-columns: 1fr 3.5rem 1fr 3.5rem 1fr;
  grid-template-areas:
    "hush  linka marked linkb bedrift"
    "risk  risk  risk   risk  risk"
    "retur retur retur  retur retur";
  align-items: start;
  column-gap: 0;
  row-gap: 1.25rem;
}

.kretslop__station[data-station="husholdningene"] { grid-area: hush; }
.kretslop__link[data-link="til-markedet"]         { grid-area: linka; }
.kretslop__station[data-station="markedet"]       { grid-area: marked; }
.kretslop__link[data-link="til-bedriftene"]       { grid-area: linkb; }
.kretslop__station[data-station="bedriftene"]     { grid-area: bedrift; }
.kretslop__risk                                   { grid-area: risk; }
.kretslop__return                                 { grid-area: retur; }

.kretslop__station {
  border: 1px solid $bed3-rule-strong;
  background: $bed3-wash;
  padding: 1rem 1.1rem 1.1rem;
}

.kretslop__station-name {
  font-family: $bed3-display;
  font-size: 1.35rem;
  margin: 0;
}

.kretslop__station-role {
  color: $bed3-muted;
  font-size: 0.9375rem;
  font-style: italic;
  margin: 0 0 0.75rem;
}

.kretslop__topics {
  list-style: none;
  margin: 0;
  padding: 0;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

.kretslop__topic {
  display: inline-block;
  border: 1px solid $bed3-rule-strong;
  background: $bed3-paper;
  color: $bed3-oxblood;
  padding: 0.2rem 0.65rem;
  font-size: 0.9375rem;
  text-decoration: none;
  border-radius: 999px;
}

.kretslop__topic:hover,
.kretslop__topic:focus-visible,
.kretslop__topic.is-active {
  background: $bed3-oxblood;
  color: $bed3-paper;
  border-color: $bed3-oxblood;
}

.kretslop__link {
  align-self: center;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.15rem;
  padding-top: 1.5rem;
}

.kretslop__link svg {
  width: 100%;
  height: 1.1rem;
  display: block;
  overflow: visible;
}

.kretslop__flow-line {
  stroke: $bed3-oxblood;
  stroke-width: 1.5;
  stroke-dasharray: 4 7;
}

.kretslop__flow-head { fill: $bed3-oxblood; }

.kretslop__link-label {
  color: $bed3-muted;
  font-size: 0.8125rem;
  font-style: italic;
}

.kretslop__risk {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  justify-content: center;
  gap: 0.5rem 0.75rem;
  border-top: 1px solid $bed3-rule;
  padding-top: 1rem;
}

.kretslop__risk-label {
  color: $bed3-muted;
  font-size: 0.8125rem;
  font-style: italic;
  width: 100%;
  text-align: center;
}

.kretslop__return {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.15rem;
}

.kretslop__return svg {
  width: 100%;
  height: 1.1rem;
  display: block;
  overflow: visible;
}

.kretslop__card {
  margin-top: 1.5rem;
  border-top: 1px solid $bed3-rule;
  padding-top: 1rem;
  min-height: 7.5rem;
}

.kretslop__card-hint {
  color: $bed3-muted;
  font-size: 0.9375rem;
}
```

Use only variables that exist in the `scss:defaults` block of `site/theme/bed3.scss`; check before using one.

Also add `.kretslop` to the print hide list:

```scss
@media print {
  .navbar, .nav-footer, .sidebar, .nav-page, .btn, .kretslop { display: none; }
  ...
}
```

- [ ] **Step 4: Render and verify statically**

```bash
cd site && quarto render
```
Expected: clean render, no include errors.

Start a server (background): `python -m http.server 8765 -d site/_site`

With Playwright MCP: `browser_navigate` to `http://localhost:8765/`, take `browser_snapshot`. Verify:
- The heading «Slik henger kurset sammen» is present.
- All 13 chips exist: `browser_evaluate` → `document.querySelectorAll('.kretslop__topic').length === 13`.
- Every chip href resolves: `browser_evaluate` collecting `[...document.querySelectorAll('.kretslop__topic')].map(a => a.getAttribute('href'))`, then `browser_network_request` (or fetch in evaluate) each URL expecting 200.
- `browser_take_screenshot` at default width; the three stations sit side by side with arrows between them.

- [ ] **Step 5: Commit**

```bash
git add site/_includes/kretslop.html site/index.qmd site/theme/bed3.scss
git commit -m "Add static kapitalens kretsløp diagram to forsiden"
```

---

### Task 2: Motion, responsive reflow, reduced motion

**Files:**
- Modify: `site/theme/bed3.scss` (extend the kretsløp section from Task 1)

**Interfaces:**
- Consumes: Task 1 DOM classes.
- Produces: CSS state classes Task 3 toggles: `.kretslop.is-exploring` (dims everything), `.is-lit` (re-lights a station/link/risk element), `.kretslop__topic.is-active`.

- [ ] **Step 1: Add flow animation and state classes to the kretsløp SCSS**

```scss
@keyframes kretslop-flow {
  to { stroke-dashoffset: -22; }
}

@keyframes kretslop-flow-return {
  to { stroke-dashoffset: 22; }
}

.kretslop__flow-line { animation: kretslop-flow 2.4s linear infinite; }
.kretslop__flow-line--return { animation: kretslop-flow-return 2.4s linear infinite; }

@media (prefers-reduced-motion: reduce) {
  .kretslop__flow-line,
  .kretslop__flow-line--return { animation: none; }
}

// Exploring state: JS sets .is-exploring on .kretslop, .is-lit on the
// active topic's station (or the risk strip) and its adjacent links.
.kretslop.is-exploring .kretslop__station,
.kretslop.is-exploring .kretslop__link,
.kretslop.is-exploring .kretslop__risk,
.kretslop.is-exploring .kretslop__return {
  opacity: 0.35;
  transition: opacity 160ms ease;
}

.kretslop.is-exploring .is-lit { opacity: 1; }

.kretslop__station,
.kretslop__link,
.kretslop__risk,
.kretslop__return {
  transition: opacity 160ms ease;
}
```

- [ ] **Step 2: Add the narrow-screen reflow**

```scss
@media (max-width: 47.9rem) {
  .kretslop__diagram {
    grid-template-columns: 1fr;
    grid-template-areas: "hush" "linka" "marked" "linkb" "bedrift" "risk" "retur";
  }

  .kretslop__link {
    padding-top: 0;
    flex-direction: row;
    justify-content: center;
    gap: 0.5rem;
  }

  .kretslop__link svg {
    width: 1.1rem;
    height: 2.2rem;
    transform: rotate(90deg);
    transform-origin: center;
  }
}
```

- [ ] **Step 3: Render and verify**

`cd site && quarto render`, reload the page in Playwright.

- Desktop: `browser_evaluate` → `getComputedStyle(document.querySelector('.kretslop__flow-line')).animationName` is `kretslop-flow`.
- Reduced motion: re-run with CSS emulation if available; otherwise assert the media query exists in the built CSS: `grep -c 'prefers-reduced-motion' site/_site/theme/bed3*.css` ≥ 1 and that it sets `animation: none` for the flow lines (inspect the built CSS text around the match).
- Mobile: `browser_resize` to 390×844, `browser_take_screenshot`; stations stack vertically, arrows point downward, nothing overflows horizontally (`document.documentElement.scrollWidth <= window.innerWidth`).

- [ ] **Step 4: Commit**

```bash
git add site/theme/bed3.scss
git commit -m "Animate kretsløp flow, reflow on narrow screens, honor reduced motion"
```

---

### Task 3: Interaction JS, explanation cards, content

**Files:**
- Create: `site/assets/kretslop.js`
- Modify: `site/_includes/kretslop.html` (append script tag)
- Modify: `site/_quarto.yml` (add `assets/kretslop.js` to `project.resources`)
- Modify: `site/theme/bed3.scss` (card content styles)

**Interfaces:**
- Consumes: Task 1 DOM contract and Task 2 state classes.
- Produces: on hover/focus a chip previews its card; on click, `preventDefault` and pin/unpin the card; the card holds the real navigation link.

- [ ] **Step 1: Write `site/assets/kretslop.js`**

The card body texts below are final copy; do not paraphrase them. `meta` is the small line under the title; `linkLabel` is the card's link text; the link URL is read from the chip's `href` so it lives in one place.

```js
(function () {
  "use strict";

  var TOPICS = {
    portefoljeteori: {
      title: "Porteføljeteori",
      meta: "Undervises i plenum",
      linkLabel: "Se timeplanen",
      body: "Husholdningene sparer ikke i ett enkelt prosjekt, men i porteføljer av mange. Porteføljeteorien viser hvordan risiko kan spres mellom aktiva, og hvorfor det bare er risikoen som ikke lar seg diversifisere bort som gir betalt i markedet."
    },
    baerekraft: {
      title: "Bærekraftige investeringer",
      meta: "Undervises i plenum",
      linkLabel: "Se timeplanen",
      body: "Sparingen skal ikke bare kaste av seg, den skal også forvaltes i tråd med det eierne står for. Vi ser på hvordan bærekraft og etiske hensyn påvirker hvilke investeringer kapitalen går til, og hva slike hensyn gjør med avkastningskravet."
    },
    kvm: {
      title: "Kapitalverdimodellen",
      meta: "Undervises i plenum",
      linkLabel: "Se timeplanen",
      body: "Kapitalverdimodellen er markedets prislapp på risiko: forventet avkastning bestemmes av hvor mye markedsrisiko en investering bærer. Prisen markedet setter her, blir avkastningskravet bedriftene må regne med i sine analyser."
    },
    rentemarkedet: {
      title: "Rentemarkedet",
      meta: "Modul 13 · 2 t 40 min",
      linkLabel: "Gå til modulen",
      body: "Renten er prisen på å flytte penger i tid, og rentemarkedet er der den prisen dannes. Vi priser obligasjoner og sertifikater, leser terminrenter og måler renterisiko med durasjon."
    },
    valuta: {
      title: "Internasjonal finans",
      meta: "Modul 16 · 41 min",
      linkLabel: "Gå til modulen",
      body: "Kapital krysser landegrenser, og da må den veksles. Vi ser hvordan valutakurser noteres, hva paritetsbetingelsene sier om sammenhengen mellom renter og kurser, og hva det betyr for investeringer og lån i utenlandsk valuta."
    },
    investeringsprosjekter: {
      title: "Investeringsprosjekter",
      meta: "Modul 1 · 71 min",
      linkLabel: "Gå til modulen",
      body: "Her møter kapitalen prosjektene. Netto nåverdi og internrente er beslutningsreglene som avgjør hvilke prosjekter som skaper verdi, og hvilke som bør vrakes når kapitalen ikke strekker til alt."
    },
    "skatt-laan": {
      title: "Effekter av skatt og lån",
      meta: "Modul 2 · 81 min",
      linkLabel: "Gå til modulen",
      body: "Et prosjekt må vurderes etter kontantstrømmene det faktisk gir eierne, og både skatt og lån endrer dem. Vi ser hvordan avskrivninger, skattesatser og lånefinansiering flettes inn i analysen."
    },
    inflasjon: {
      title: "Effekter av inflasjon",
      meta: "Modul 3 · 40 min",
      linkLabel: "Gå til modulen",
      body: "Når prisnivået stiger, er ikke en krone i dag og en krone om ti år samme størrelse. Vi skiller nominelle og reelle størrelser og sørger for at kontantstrømmer og avkastningskrav måles i samme enhet."
    },
    risikovurderinger: {
      title: "Risikovurderinger",
      meta: "Modul 4 · 74 min",
      linkLabel: "Gå til modulen",
      body: "Kontantstrømmene i en analyse er anslag, ikke fasit. Stjernediagram, scenarioanalyse, simulering og beslutningstrær stresstester forutsetningene og viser hvor følsom lønnsomheten er for at de ryker."
    },
    "laan-betaling": {
      title: "Låne- og betalingsalternativer",
      meta: "Modul 5 · 46 min",
      linkLabel: "Gå til modulen",
      body: "Samme investering kan finansieres og betales på mange måter: serielån, annuitetslån eller leasing. Effektiv rente gir en felles målestokk som gjør alternativene sammenlignbare."
    },
    kapitalstruktur: {
      title: "Kapitalstruktur",
      meta: "Undervises i plenum",
      linkLabel: "Se timeplanen",
      body: "Bedriften kan hente kapitalen som gjeld eller egenkapital, og blandingen kalles kapitalstrukturen. Vi ser hva miksen gjør med risikoen, avkastningskravet og verdien av selskapet."
    },
    opsjoner: {
      title: "Opsjonskontrakter",
      meta: "Modul 14 · 1 t 58 min",
      linkLabel: "Gå til modulen",
      body: "En opsjon gir retten, men ikke plikten, til å kjøpe eller selge til en avtalt pris. Slik kan risiko flyttes fra den som ikke vil bære den, til den som tar betalt for å gjøre det. Vi ser også hvordan slike kontrakter prises."
    },
    terminer: {
      title: "Terminkontrakter",
      meta: "Modul 15 · 1 t 42 min",
      linkLabel: "Gå til modulen",
      body: "En terminkontrakt låser prisen på en fremtidig handel allerede i dag. Det gjør fremtiden mer forutsigbar for begge parter, og vi ser hvordan terminpriser henger sammen med spotpriser og forventninger."
    }
  };

  var root = document.getElementById("kretslop");
  var card = document.getElementById("kretslop-card");
  if (!root || !card) return;

  var defaultCard = card.innerHTML;
  var pinned = null; // data-topic id of a clicked (pinned) chip, or null

  function neighborhood(chip) {
    // Elements to re-light for a given chip: its station (or the risk
    // strip) plus the flow links that touch it.
    var els = [];
    var station = chip.closest(".kretslop__station");
    if (station) {
      els.push(station);
      var s = station.getAttribute("data-station");
      if (s === "husholdningene" || s === "markedet") {
        els.push(root.querySelector('[data-link="til-markedet"]'));
      }
      if (s === "markedet" || s === "bedriftene") {
        els.push(root.querySelector('[data-link="til-bedriftene"]'));
      }
    } else {
      els.push(root.querySelector(".kretslop__risk"));
      els.push(root.querySelector('[data-link="til-markedet"]'));
      els.push(root.querySelector('[data-link="til-bedriftene"]'));
    }
    return els.filter(Boolean);
  }

  function clearState() {
    root.classList.remove("is-exploring");
    root.querySelectorAll(".is-lit").forEach(function (el) {
      el.classList.remove("is-lit");
    });
    root.querySelectorAll(".kretslop__topic.is-active").forEach(function (el) {
      el.classList.remove("is-active");
    });
  }

  function show(chip) {
    var id = chip.getAttribute("data-topic");
    var topic = TOPICS[id];
    clearState();
    root.classList.add("is-exploring");
    chip.classList.add("is-active");
    neighborhood(chip).forEach(function (el) { el.classList.add("is-lit"); });
    if (!topic) return; // unknown id: keep highlight, leave the card alone
    card.innerHTML =
      '<h4 class="kretslop__card-title"></h4>' +
      '<p class="kretslop__card-meta"></p>' +
      '<p class="kretslop__card-body"></p>' +
      '<p><a class="kretslop__card-link"></a></p>';
    card.querySelector(".kretslop__card-title").textContent = topic.title;
    card.querySelector(".kretslop__card-meta").textContent = topic.meta;
    card.querySelector(".kretslop__card-body").textContent = topic.body;
    var link = card.querySelector(".kretslop__card-link");
    link.textContent = topic.linkLabel;
    link.setAttribute("href", chip.getAttribute("href"));
  }

  function reset() {
    clearState();
    card.innerHTML = defaultCard;
    pinned = null;
  }

  root.querySelectorAll(".kretslop__topic").forEach(function (chip) {
    chip.addEventListener("mouseenter", function () {
      if (!pinned) show(chip);
    });
    chip.addEventListener("focus", function () {
      if (!pinned) show(chip);
    });
    chip.addEventListener("click", function (e) {
      e.preventDefault();
      var id = chip.getAttribute("data-topic");
      if (pinned === id) {
        reset();
      } else {
        pinned = id;
        show(chip);
      }
    });
  });

  root.addEventListener("mouseleave", function () {
    if (!pinned) reset();
  });

  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape" && pinned) reset();
  });
})();
```

- [ ] **Step 2: Wire the script and card styles**

Append to `site/_includes/kretslop.html` (last line, after the closing `</div>` of `.kretslop`):

```html
<script src="assets/kretslop.js" defer></script>
```

In `site/_quarto.yml`, add to `project.resources`:

```yaml
    - assets/kretslop.js
```

Append to the kretsløp SCSS section:

```scss
.kretslop__card-title {
  font-family: $bed3-display;
  font-size: 1.25rem;
  margin: 0;
}

.kretslop__card-meta {
  color: $bed3-muted;
  font-size: 0.8125rem;
  margin: 0 0 0.4rem;
}

.kretslop__card-body {
  max-width: 68ch;
  margin: 0 0 0.4rem;
}
```

- [ ] **Step 3: Render and verify interactions**

`cd site && quarto render`, reload in Playwright.

- Hover `Rentemarkedet` chip (`browser_hover` on the chip): `#kretslop-card` shows title «Rentemarkedet», meta «Modul 13 · 2 t 40 min», and a link to `kursmateriale/02-markeder/02-rentemarkedet/index.html`. The markedet station and both connectors have `is-lit`; `#kretslop` has `is-exploring`.
- Move the mouse out of the diagram: the card returns to the hint text.
- Click `Porteføljeteori`: page does NOT navigate; card pins with «Se timeplanen» link to `timeplan.html`. Click it again: card resets.
- Click `Opsjonskontrakter`: risk strip and both connectors are lit.
- Keyboard: `browser_press_key` Tab until a chip focuses; the card follows focus. Escape unpins.
- Card link navigates: click the card's «Gå til modulen» after pinning `Investeringsprosjekter`; lands on the module page (then navigate back).

- [ ] **Step 4: Commit**

```bash
git add site/assets/kretslop.js site/_includes/kretslop.html site/_quarto.yml site/theme/bed3.scss
git commit -m "Make kretsløpet explorable: topic cards, highlighting, pinning"
```

---

### Task 4: Design polish and full verification

**Files:**
- Modify: `site/theme/bed3.scss`, `site/_includes/kretslop.html` (only as polish requires)

**Interfaces:**
- Consumes: everything above. Produces: the shipped section.

- [ ] **Step 1: Polish pass with fresh eyes**

Invoke the `impeccable:impeccable` skill on the built page (desktop and 390px screenshots) and apply its material fixes within the design system: spacing rhythm against `--section-gap`, chip sizing and tap targets (min 44px on touch), connector alignment with station midlines, card min-height so the layout never jumps, hover transition timing. Do not introduce new colors, shadows, or faces; the system is `design/v2/styles.css` and its SCSS port.

- [ ] **Step 2: Full verification sweep**

- `cd site && quarto render` clean.
- Playwright: repeat Task 3 assertions once end-to-end on the built site.
- All 13 chip hrefs return 200 on the local server.
- Mobile 390×844: no horizontal scroll, chips tappable, tap pins the card.
- `grep -n 'prefers-reduced-motion' site/theme/bed3.scss` present; built CSS contains it.
- Copy check on all card texts and labels: no em dashes (`grep -c '—' site/assets/kretslop.js` must be 0 outside of nothing — the file must contain no em dash at all), no all-caps labels, bokmål.
- Screenshots (desktop, mobile, one with a pinned card) reviewed against the design system.

- [ ] **Step 3: Commit**

```bash
git add -A
git commit -m "Polish kretsløp spacing, tap targets, and card rhythm"
```
