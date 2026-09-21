(function () {
  "use strict";

  var TP = "timeplan.html";
  var TOPICS = {
    portefoljeteori: { title: "Porteføljeteori", meta: "Undervises i plenum", station: "hush", paths: ["p1"],
      links: [{ label: "Se timeplanen", href: TP }],
      body: "Husholdningene sparer ikke i ett enkelt prosjekt, men i porteføljer av mange. Porteføljeteorien viser hvordan risiko kan spres mellom aktiva, og hvorfor det bare er risikoen som ikke lar seg diversifisere bort som gir betalt i markedet." },
    baerekraft: { title: "Bærekraftige investeringer", meta: "Undervises i plenum", station: "hush", paths: ["p1"],
      links: [{ label: "Se timeplanen", href: TP }],
      body: "Sparingen skal ikke bare kaste av seg, den skal også forvaltes i tråd med det eierne står for. Vi ser på hvordan bærekraft og etiske hensyn påvirker hvilke investeringer kapitalen går til, og hva slike hensyn gjør med avkastningskravet." },
    kvm: { title: "Kapitalverdimodellen", meta: "Undervises i plenum", station: "kapmarked", paths: ["p1", "p2"],
      links: [{ label: "Se timeplanen", href: TP }],
      body: "Kapitalverdimodellen er markedets prislapp på risiko: forventet avkastning bestemmes av hvor mye markedsrisiko en investering bærer. Prisen markedet setter her, blir avkastningskravet bedriftene må regne med i sine analyser." },
    aksjeprising: { title: "Aksjeprising og markedseffisiens", meta: "Undervises i plenum", station: "kapmarked", paths: ["p1", "p2"],
      links: [{ label: "Se timeplanen", href: TP }],
      body: "En aksje er en eierandel i bedriftens fremtidige kontantstrømmer, og prisen dannes i markedet. Vi ser hvordan aksjer verdsettes, og hva det betyr at markedet er effisient: at prisene allerede reflekterer tilgjengelig informasjon." },
    rentemarkedet: { title: "Rentemarkedet", meta: "Modul 13 · 2 t 40 min", station: "kapmarked", paths: ["p1", "p2"],
      links: [{ label: "Gå til modulen", href: "kursmateriale/02-markeder/02-rentemarkedet/index.html" }],
      body: "Renten er prisen på å flytte penger i tid, og rentemarkedet er der den prisen dannes. Vi priser obligasjoner og sertifikater, leser terminrenter og måler renterisiko med durasjon." },
    valuta: { title: "Internasjonal finans", meta: "Modul 16 · 41 min", station: "kapmarked", paths: ["p1", "p2"],
      links: [{ label: "Gå til modulen", href: "kursmateriale/02-markeder/05-valuta/index.html" }],
      body: "Kapital krysser landegrenser, og da må den veksles. Vi ser hvordan valutakurser noteres, hva paritetsbetingelsene sier om sammenhengen mellom renter og kurser, og hva det betyr for investeringer og lån i utenlandsk valuta." },
    opsjoner: { title: "Opsjonskontrakter", meta: "Modul 14 · 1 t 58 min", station: "derivat", paths: ["r1", "r2"],
      links: [{ label: "Gå til modulen", href: "kursmateriale/02-markeder/03-opsjoner/index.html" }],
      body: "En opsjon gir retten, men ikke plikten, til å kjøpe eller selge til en avtalt pris. Slik kan risiko flyttes fra den som ikke vil bære den, til den som tar betalt for å gjøre det. Vi ser også hvordan slike kontrakter prises." },
    terminer: { title: "Terminkontrakter", meta: "Modul 15 · 1 t 42 min", station: "derivat", paths: ["r1", "r2"],
      links: [{ label: "Gå til modulen", href: "kursmateriale/02-markeder/04-terminer/index.html" }],
      body: "En terminkontrakt låser prisen på en fremtidig handel allerede i dag. Det gjør fremtiden mer forutsigbar for begge parter, og vi ser hvordan terminpriser henger sammen med spotpriser og forventninger." },
    investeringsbeslutningen: { title: "Investeringsbeslutningen", meta: "Fire moduler", station: "bedrift", paths: ["p2"],
      links: [
        { label: "Investeringsprosjekter", note: "modul 1 · 71 min", href: "kursmateriale/01-investeringsanalyse/02-investeringsprosjekter/index.html" },
        { label: "Effekter av skatt og lån", note: "modul 2 · 81 min", href: "kursmateriale/01-investeringsanalyse/03-skatt-og-laan/index.html" },
        { label: "Effekter av inflasjon", note: "modul 3 · 40 min", href: "kursmateriale/01-investeringsanalyse/04-inflasjon/index.html" },
        { label: "Risikovurderinger", note: "modul 4 · 74 min", href: "kursmateriale/01-investeringsanalyse/05-risikovurderinger/index.html" }
      ],
      body: "Hvilke prosjekter skal gjennomføres? Netto nåverdi og internrente rangerer prosjektene, mens skatt, lån, inflasjon og risiko kompliserer kontantstrømmene som ligger til grunn." },
    finansieringsbeslutningen: { title: "Finansieringsbeslutningen", meta: "Én modul og ett plenumstema", station: "bedrift", paths: ["p2"],
      links: [
        { label: "Låne- og betalingsalternativer", note: "modul 5 · 46 min", href: "kursmateriale/01-investeringsanalyse/06-laane-og-betalingsalternativer/index.html" },
        { label: "Kapitalstruktur", note: "plenum · se timeplanen", href: TP }
      ],
      body: "Hvordan skal prosjektene betales, med gjeld eller egenkapital? Kapitalstrukturen bestemmer miksen, og effektiv rente gjør låne- og betalingsalternativene sammenlignbare." },
    utbyttebeslutningen: { title: "Utbyttebeslutningen", meta: "Undervises i plenum", station: "bedrift", paths: ["p3"],
      links: [{ label: "Utbyttepolitikk, se timeplanen", href: TP }],
      body: "Når prosjektene kaster av seg, må bedriften bestemme hva overskuddet skal brukes til: betales ut til eierne som utbytte, eller holdes tilbake og reinvesteres. Utbyttepolitikken avgjør hvordan verdiene finner veien tilbake til husholdningene." }
  };

  var board = document.getElementById("kl-board");
  var flows = document.getElementById("kl-flows");
  var canvas = document.getElementById("kl-sparks");
  var card = document.getElementById("kl-card");
  if (!board || !flows || !canvas || !card) return;

  var defaultCard = card.innerHTML;
  var pinned = null;
  var pathEls = {};

  function anchor(sel, side) {
    var b = board.getBoundingClientRect();
    var r = board.querySelector(sel).getBoundingClientRect();
    return {
      x: (side === "left" ? r.left : side === "right" ? r.right : r.left + r.width / 2) - b.left,
      y: (side === "bottom" ? r.bottom : r.top + r.height / 2) - b.top
    };
  }

  function layoutPaths() {
    var b = board.getBoundingClientRect();
    flows.setAttribute("viewBox", "0 0 " + b.width + " " + b.height);
    var hushR = anchor('[data-station="hush"]', "right");
    var bedL = anchor('[data-station="bedrift"]', "left");
    var kapL = anchor('[data-station="kapmarked"]', "left");
    var kapR = anchor('[data-station="kapmarked"]', "right");
    var derL = anchor('[data-station="derivat"]', "left");
    var derR = anchor('[data-station="derivat"]', "right");
    var e1 = anchor('[data-station="bedrift"]', "bottom");
    var e2 = anchor('[data-station="hush"]', "bottom");
    var retY = b.height - 40;
    function curve(id, cls, x1, y1, x2, y2) {
      var dx = (x2 - x1) * 0.45;
      return '<path id="' + id + '" class="' + cls + '" d="M' + x1 + " " + y1 +
        " C " + (x1 + dx) + " " + y1 + ", " + (x2 - dx) + " " + y2 + ", " + x2 + " " + y2 + '"/>';
    }
    flows.innerHTML =
      curve("p1", "", hushR.x, hushR.y - 24, kapL.x, kapL.y) +
      curve("p2", "", kapR.x, kapR.y, bedL.x, bedL.y - 24) +
      curve("r1", "risk", hushR.x, hushR.y + 24, derL.x, derL.y) +
      curve("r2", "risk", derR.x, derR.y, bedL.x, bedL.y + 24) +
      '<path id="p3" class="ret" d="M' + e1.x + " " + (e1.y + 8) + " C " + e1.x + " " + retY + ", " + e2.x + " " + retY + ", " + e2.x + " " + (e2.y + 8) + '"/>';
    pathEls = { p1: flows.querySelector("#p1"), p2: flows.querySelector("#p2"),
                r1: flows.querySelector("#r1"), r2: flows.querySelector("#r2"),
                p3: flows.querySelector("#p3") };
  }

  /* particles: capital ember one-way, risk parchment two-way, return gold */
  var ctx = canvas.getContext("2d");
  var parts = [];
  var reduced = matchMedia("(prefers-reduced-motion: reduce)").matches;
  function seed() {
    parts = [];
    [["p1", 8, 1], ["p2", 8, 1], ["p3", 16, 1], ["r1", 5, -1], ["r1", 5, 1], ["r2", 5, -1], ["r2", 5, 1]].forEach(function (cfg) {
      for (var i = 0; i < cfg[1]; i++) {
        parts.push({ id: cfg[0], t: Math.random(), v: cfg[2] * (0.0011 + Math.random() * 0.0011) });
      }
    });
  }
  function tick() {
    var b = board.getBoundingClientRect();
    if (canvas.width !== b.width * devicePixelRatio) {
      canvas.width = b.width * devicePixelRatio;
      canvas.height = b.height * devicePixelRatio;
    }
    ctx.setTransform(devicePixelRatio, 0, 0, devicePixelRatio, 0, 0);
    ctx.clearRect(0, 0, b.width, b.height);
    parts.forEach(function (p) {
      var el = pathEls[p.id];
      if (!el) return;
      p.t += p.v;
      if (p.t > 1) p.t -= 1;
      if (p.t < 0) p.t += 1;
      var len = el.getTotalLength();
      var pt = el.getPointAtLength(p.t * len);
      var fill, glow, r;
      if (p.id === "p3") { fill = "rgba(211,162,74,0.9)"; glow = "rgba(211,162,74,0.9)"; r = 1.8; }
      else if (p.id === "r1" || p.id === "r2") { fill = "rgba(194,168,157,0.85)"; glow = "rgba(194,168,157,0.7)"; r = 1.5; }
      else { fill = "rgba(224,91,65,0.95)"; glow = "rgba(224,91,65,0.9)"; r = 2.2; }
      ctx.beginPath();
      ctx.arc(pt.x, pt.y, r, 0, 7);
      ctx.fillStyle = fill;
      ctx.shadowColor = glow;
      ctx.shadowBlur = 8;
      ctx.fill();
      ctx.shadowBlur = 0;
    });
    requestAnimationFrame(tick);
  }

  /* interaction */
  function clearState() {
    board.classList.remove("exploring");
    board.querySelectorAll(".lit, .has-lit").forEach(function (el) { el.classList.remove("lit", "has-lit"); });
    board.querySelectorAll(".kl-chip.on").forEach(function (el) { el.classList.remove("on"); });
  }
  function show(chip) {
    var id = chip.getAttribute("data-topic");
    var t = TOPICS[id];
    clearState();
    board.classList.add("exploring");
    chip.classList.add("on");
    if (!t) return;
    var stEl = board.querySelector('[data-station="' + t.station + '"]');
    if (stEl) {
      stEl.classList.add("lit");
      var parent = stEl.closest(".kl-station");
      if (parent && parent !== stEl) parent.classList.add("has-lit");
    }
    (t.paths || []).forEach(function (pid) { if (pathEls[pid]) pathEls[pid].classList.add("lit"); });
    card.innerHTML = "<h3 class='kl-card-title'></h3><span class='kl-meta'></span><p class='kl-body'></p><div class='kl-links'></div>";
    card.querySelector(".kl-card-title").textContent = t.title;
    card.querySelector(".kl-meta").textContent = t.meta;
    card.querySelector(".kl-body").textContent = t.body;
    var wrap = card.querySelector(".kl-links");
    (t.links || []).forEach(function (l) {
      var row = document.createElement("div");
      row.className = "kl-row";
      var a = document.createElement("a");
      a.textContent = l.label;
      a.href = l.href;
      row.appendChild(a);
      if (l.note) {
        var n = document.createElement("span");
        n.className = "kl-note";
        n.textContent = l.note;
        row.appendChild(n);
      }
      wrap.appendChild(row);
    });
    card.classList.remove("pop"); void card.offsetWidth; card.classList.add("pop");
  }
  function reset() { clearState(); card.innerHTML = defaultCard; pinned = null; }

  board.querySelectorAll(".kl-chip").forEach(function (chip) {
    chip.addEventListener("mouseenter", function () { if (!pinned) show(chip); });
    chip.addEventListener("focus", function () { if (!pinned) show(chip); });
    chip.addEventListener("click", function (e) {
      e.preventDefault();
      var id = chip.getAttribute("data-topic");
      if (pinned === id) { reset(); } else { pinned = id; show(chip); }
    });
  });
  board.addEventListener("mouseleave", function () { if (!pinned) reset(); });
  document.addEventListener("keydown", function (e) { if (e.key === "Escape" && pinned) reset(); });
  document.addEventListener("click", function (e) {
    if (pinned && !e.target.closest(".kl-chip") && !e.target.closest(".kl-card")) reset();
  });

  layoutPaths();
  addEventListener("resize", layoutPaths);
  if (!reduced) { seed(); requestAnimationFrame(tick); }
})();
