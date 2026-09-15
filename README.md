# Arcade Fantasy Stats Lab

Shiny-App für Fantasy-Football-Statistiken und Wins Above Replacement (WAR), live unter
<https://arcadefantasy.shinyapps.io/Arcade_Basic_Stats/>.

## Aufbau

```
pipeline/            baut die Datensätze aus nflverse (läuft in GitHub Actions)
R/                   App-Code, wird von Shiny automatisch geladen
  data_store.R       lädt die Daten aus dem Release "data", prüft stündlich auf neue
  war.R              WAR-Berechnung
  league.R           Sleeper- und MFL-Anbindung
  mod_*.R            ein Modul pro Tab
app.R                UI und Server
tests/testthat/      Unit-, Pipeline- und App-Tests
deploy.R             Deploy nach shinyapps.io
```

## Wie die Daten aktuell bleiben

Der Workflow `.github/workflows/data.yml` läuft während der Saison (September bis Februar)
täglich, sonst wöchentlich. Er

1. lädt Player Stats, Snap Counts, Expected Points und Player IDs von nflverse für alle
   Saisons ab 1999 und baut daraus `offense.parquet` und `defense.parquet` komplett neu,
2. prüft die Daten (Spalten, Duplikate, Zeilen pro Saison, aktuelle Woche vorhanden,
   Abdeckung der Expected Points) und bricht bei Problemen ab,
3. lädt die Dateien und `manifest.json` in das GitHub-Release `data` hoch.

Die App holt sich die Daten beim Start aus dem Release und schaut stündlich, ob sich das
Manifest geändert hat. Neue Daten brauchen also keinen Deploy. Oben rechts in der App steht
der Datenstand.

Saison und Woche kommen aus `nflreadr::get_current_season()` / `get_current_week()`, es
gibt keine Jahreszahlen im Code und keine Übergabe von Saison zu Saison.

Schlägt ein Lauf fehl, verschickt GitHub eine Mail. Manuell anstoßen:

```bash
gh workflow run data.yml
```

## Deploy

Der Workflow `ci.yml` testet jeden Pull Request und jeden Push auf `main`; nach grünen Tests
auf `main` deployt er nach shinyapps.io. Dafür braucht das Repository die Secrets
`SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN` und `SHINYAPPS_SECRET` (Token unter
shinyapps.io → Account → Tokens).

`dependencies.yml` aktualisiert einmal im Monat `renv.lock` und öffnet dafür einen PR.
Damit das klappt, muss unter Settings → Actions → General "Allow GitHub Actions to create
and approve pull requests" aktiv sein.

## Lokal entwickeln

```r
renv::restore()
```

Daten lokal bauen und die App damit starten:

```bash
Rscript pipeline/build.R build
ARCADE_DATA_DIR=build Rscript -e 'shiny::runApp()'
```

Ohne `ARCADE_DATA_DIR` lädt die App die veröffentlichten Daten aus dem Release.

Tests:

```bash
NOT_CRAN=true Rscript -e 'testthat::test_dir("tests/testthat")'
```

Der App-Test braucht Chrome; ohne Chrome wird er übersprungen.

## Ligen

Im WAR-Tab wird die Liga als Text angegeben:

| Eingabe       | Punkte                                                   |
|---------------|----------------------------------------------------------|
| `PPR`         | nflverse PPR, IDP mit Arcade-Scoring aus der Pipeline    |
| `mPPR`        | Scores der Arcade-MFL-Liga 60206                         |
| `mfl22686`    | Scores einer beliebigen MFL-Liga, inkl. Franchises       |
| Sleeper-ID    | Scores und Kader einer Sleeper-Liga                      |

MFL wird direkt über die MFL-API abgefragt (gedrosselt und 15 Minuten gecacht), Sleeper
über ffscrapr.
