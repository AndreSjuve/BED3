# Konvensjoner for videosider (K01-modellen)

Dette dokumentet er kontrakten for alle som lager kapittelsider på BED3-siden.
K01 (`site/kursmateriale/01-investeringsanalyse/02-investeringsprosjekter/`) er
referanseimplementasjonen — les den før du skriver noe.

## Kilder og autoritet

- **Transkriptene er autoritative.** Sidene skrives fra det foreleseren faktisk
  sier, i hans rekkefølge og med hans vektlegging — ikke fra læreboken og ikke
  fra gjetning. Transkripter ligger i `transcripts/KXX/` (gitignorert).
- Whisper hører feil på tall og fagord. Hvert transkript skal ha en
  «Transcription notes»-seksjon som flagger mistenkte feilhøringer, med
  korrigerte tall **verifisert aritmetisk** mot alle øvrige tall i videoen
  (se K01/V06 og V07 for mønsteret). Tall som ikke lar seg verifisere brukes
  ikke i prosa eller quiz.
- Usikre referanser i lyden (kurskoder o.l.) utelates fra prosaen.
- Læreboken (BDH: Berk, DeMarzo & Harford, *Fundamentals of Corporate
  Finance*, Global Edition, 6e, Pearson 2024 — fra og med 2026/2027 kurset;
  tidligere pensum var Brealey/Myers/Marcus (BMM) 11e) er **inspirasjon**
  for oppgaver og distraktorer — aldri kilde for påstander om hva videoen
  sier.

## Sideanatomi (i denne rekkefølgen)

```
--- YAML: title + subtitle ---
## Før du ser videoen        <- 2 avsnitt, 85–115 ord
::::: {.video} + iframe
::::: {.video-meta}          <- «M:SS · Video N av X · [Kapittel …](index.qmd)»
## Sjekk forståelsen         <- preambel + quiz
```

## Introteksten («Før du ser videoen»)

- **Orientering, ikke referat.** Still spørsmålet videoen besvarer, knytt til
  forrige video, pek på det strukturelt viktige å se etter. Videoen underviser;
  prosaen posisjonerer.
- **Ingen** formler, tabeller, utregnede eksempler eller tall fra videoen.
- **Still spørsmål, ikke gi svar:** introen skal aldri inneholde formuleringer
  som besvarer quizspørsmålene. `tools/check_overlap.py` skal gi 0 treff.
- Bokmål. Aldri versaler/sperret skrift i overskrifter.

## Videoembed

- Panopto-iframe med samme form som K01-sidene (autoplay=false, lazy, egen
  `title` «Video N · Tittel»).
- **Per-video-GUID-er finnes foreløpig bare for K01.** For øvrige kapitler:
  bruk `.video__placeholder`-mønsteret (definert i `bed3.scss`, brukt i
  `01-introduksjon.qmd`) med lenke til kapittelets Panopto-mappe
  (`docs/panopto_folder_links.md`). **Aldri** finn på en GUID.
- Varighet i `.video-meta` hentes fra videofilens faktiske lengde
  (transkriptrapportens header). Den brukes senere til å matche side ↔
  Panopto-økt, så den må være eksakt.

## Rettelser (valgfri blokk)

Sider der opptaket inneholder en **bekreftet** feil — foreleseren sier feil
tall/begrep, eller plansjen viser en gal verdi — får en `::::: {.corrections}`
-blokk rett under `.video-meta`: tidsstempel i fet, hva som sies/vises, og
riktig verdi. Synlig, ikke sammenleggbar: studenten som trenger den står fast
midt i videoen. Terskelen er streng: bare feil som er verifisert mot kilder
eller intern konsistens. Whisper-feilhøringer og uverifiserte tall hører
hjemme i transkriptnotatene, aldri her.

## Quizen («Sjekk forståelsen»)

- 3 spørsmål; **4 på regnetunge sider**, der siste spørsmål krever kalkulator
  og preambelen sier det: «Fire spørsmål. … Det siste spørsmålet krever
  kalkulator.»
- Markup nøyaktig som K01: `.quiz` > `.quiz__q[data-answer]` > tre
  `.choice`-knapper (a/b/c) > `.quiz__feedback hidden`, og til slutt
  `.quiz__score`. Ingen id-er trengs (JS setter roller selv).
- **Distraktorer er navngitte misforståelser** (rente feil vei, enkel rente,
  glemt diskontering, sunk cost «føles reell», forveksling av k og y, …).
  Aldri utfyllingsalternativer ingen tror på, aldri alternativer med
  selvdiskrediterende begrunnelse, aldri alternativer som viser til
  informasjon som ikke står i stammen.
- **Feedback forklarer hvorfor distraktorene er gale**, ikke bare hvorfor
  fasit er riktig. Dette er sidens viktigste pedagogiske element.
- Spørsmål tester **videoen**, ikke introteksten. Eksempler hentet fra
  foreleserens egne tall skal **omparametriseres** (samme skjema, nye tall) så
  episodisk hukommelse ikke er en snarvei.
- Fasitbokstaven varieres (ikke samme bokstav tre ganger).
- **All aritmetikk verifiseres ved faktisk utregning** før innsjekk — også
  distraktorenes tall (de skal være de gale svarene en ekte feil gir).

## Kapittelets index.qmd

- Intro (hva kapittelet handler om, antall videoer, samlet tid — verifisert
  mot filene), `.index-list` med rad per video (tittel + undertittel **må**
  holdes identisk med sidenes YAML), lenke til Panopto-mappen, og ~3
  `Oppgave`-blokker med `<details class="solution">`-løsninger.
- Oppgavene er regneoppgaver som dekker kapittelets viktigste ferdigheter og
  overlapper minst mulig med quizene. Løsninger: verifisert aritmetikk, og
  ikke overpresis avrunding (skriv «≈» der faktorer er avrundet).

## Notasjon

NNV, $k$ (avkastningskrav), $y$ (internrente), $I_0$, $CF_t$, NVI
(= NNV/$I_0$, grense > 0). Følg foreleserens muntlige bruk; avviker den i et
senere kapittel, følg foreleseren og flagg avviket til orkestratoren.
Formelarket kan bruke NPV/PVI — det er kjent og håndteres sentralt.

## Tekniske husregler

- **Aldri** skriv/rediger disse UTF-8-filene via PowerShell 5.1 — norske tegn
  dobbeltkodes. Bruk Write/Edit-verktøyene, eller python med
  `encoding='utf-8'` (`C:\Python314\python.exe`).
- Rendering av nettstedet gjøres av orkestratoren (unngå samtidige
  `quarto render` mot `_site/`).
- Nye filnavn → oppdater `site/_quarto.yml` (eneste sted utenfor mappen som
  refererer enkeltsider).

## Sjekkliste før levering

1. `python tools/check_overlap.py <kapittelmappe>` → 0 funn.
2. Aritmetikken i quiz + oppgaver verifisert med et kjørt regnestykke
   (legg beregningen ved i leveranserapporten).
3. Tittel/undertittel identisk i side-YAML og index-rad.
4. Ingen versaler-overskrifter, ingen PS-mojibake (`grep "Ã"` → tomt).
5. Rapportér: hva som er skrevet, tall som ble omparametrisert, avvik fra
   transkriptet, og alt du var usikker på.
