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
    if (!topic) return; // unknown id: keep the highlight, leave the card alone
    card.innerHTML =
      '<h4 class="kretslop__card-title"></h4>' +
      '<p class="kretslop__card-meta"></p>' +
      '<p class="kretslop__card-body"></p>' +
      '<p class="kretslop__card-action"><a class="kretslop__card-link"></a></p>';
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
