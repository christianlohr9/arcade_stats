# Arcade Fantasy Stats

🎮 **[Live App verfügbar auf shinyapps.io](https://arcadefantasy.shinyapps.io/Arcade_Basic_Stats/)** 🎮

Eine umfassende R Shiny-Anwendung für Fantasy Football Statistik-Analysen und Visualisierungen. Die App bietet tiefgreifende Einblicke in NFL-Spielerleistungen, WAR-Berechnungen (Wins Above Replacement) und Team-Bewertungen für Fantasy Football Ligen.

## 🏈 Features

- **nflfastR Stats**: Umfassende offensive und defensive Spielerstatistiken aus nflfastR-Daten
- **Player Stats Offense**: Detaillierte Analysen von Fantasy-relevanten Metriken (WOPR, RACR, PACR, etc.)
- **WAR Analysis**: Vollständige Wins Above Replacement Berechnungen für alle Positionen (Offense + IDP)
- **Values**: Team-Bewertungen basierend auf WAR-Werten aller Roster-Positionen
- **League Tools**: Sleeper League ID Helper für einfache Liga-Integration
- **Multi-League Support**: Unterstützung für PPR, mPPR, MFL und Sleeper Ligen
- **🆕 Automatische Daten-Updates**: Vollautomatische NFL-Daten-Aktualisierung während der Saison

## 🚀 Quick Start

### Voraussetzungen

- R (>= 4.5.0)
- **ffscrapr Development Version** (erforderlich für ESPN API Kompatibilität):
  ```r
  install.packages("ffscrapr", repos = c("https://ffverse.r-universe.dev", getOption("repos")))
  ```
- Docker (optional, für containerisierten Deployment)

### Lokale Entwicklung

1. **Repository klonen**
   ```bash
   git clone <your-repo-url>
   cd arcade_stats
   ```

2. **Dependencies installieren** (mit renv)
   ```r
   # R wird automatisch renv installieren und Pakete wiederherstellen
   # beim ersten Öffnen des Projekts
   renv::restore()
   
   # WICHTIG: ffscrapr Development Version installieren
   install.packages("ffscrapr", repos = c("https://ffverse.r-universe.dev", getOption("repos")))
   ```

3. **Einmalige Datenstruktur-Migration** (nur beim ersten Setup)
   ```bash
   # Konsolidiert 26+ Einzeldateien zu 6 optimierten Dateien
   Rscript migrate_data_structure.R
   ```

4. **Automatische Daten-Updates einrichten** (optional)
   ```bash
   # Installiert Cron Jobs für automatische Updates
   ./setup_cron.sh
   ```

5. **Daten manuell aktualisieren**
   ```bash
   # Aktuelle Saison aktualisieren
   Rscript update_nfl_data.R --force
   
   # Oder mit Test-Script
   Rscript test_update.R
   ```

6. **Anwendung starten**
   ```r
   shiny::runApp("app.R")
   ```

Die App ist dann verfügbar unter `http://localhost:3838`

### Docker Deployment

1. **Mit Docker Compose erstellen und starten**
   ```bash
   docker-compose up --build
   ```

2. **Produktions-Deployment mit nginx**
   ```bash
   docker-compose --profile production up --build
   ```

## 🤖 Automatische Daten-Updates

### Neue Optimierte Datenstruktur

Das System verwendet eine hochoptimierte, jahresunabhängige Datenstruktur:

```
data/
├── historical_weekly_stats.rds     # Alle Jahre 1999-2024 (vereint)
├── historical_snaps_stats.rds      # Alle Jahre 2012-2024 (vereint)
├── ep_historical.rds               # Historical Expected Points Daten
├── weekly_act.rds                  # Aktuelle Saison (jahresagnostisch)
├── all_snaps_act.rds              # Aktuelle Saison Snaps
├── ep_act.rds                     # Aktuelle Saison Expected Points
└── ep_past.rds                    # Backup EP Daten
```

### Automatische Updates während der NFL-Saison

**Cron Schedule (MESZ/MEZ 09:00 Uhr):**
- **Freitag 09:00**: Nach Thursday Night Game
- **Montag 09:00**: Nach Sunday Games
- **Dienstag 09:00**: Nach Monday Night Game

**Setup:**
```bash
# Cron Jobs installieren (automatische Zeitumstellung MESZ/MEZ)
./setup_cron.sh

# Status prüfen
crontab -l | grep update_nfl_data

# Logs überwachen
tail -f logs/nfl_data_update.log
```

### Intelligente Saison-Erkennung

- **Automatische Erkennung** der aktuellen NFL-Saison (September bis Februar)
- **Off-Season Handling**: Keine Updates zwischen März-August
- **Jahresunabhängige Dateien**: Keine manuellen Anpassungen nötig
- **Automatic Season Transition**: Migriert automatisch von 2025 → 2026

### Manuelle Steuerung

```bash
# Status-Check
Rscript test_update.R

# Manuelles Update (auch Off-Season)
Rscript update_nfl_data.R --force

# Migration am Saisonende
Rscript -e "source('R/utils_neu.R'); migrate_current_season_to_historical()"
```

## 📁 Projektstruktur

```
arcade_stats/
├── app.R                          # Haupt-Shiny-Anwendung
├── 
├── 🆕 Automatisierung
├── update_nfl_data.R              # Haupt-Update-Script (für Cron Jobs)
├── R/utils_neu.R                  # Optimiertes Daten-Management System
├── migrate_data_structure.R       # Einmalige Datenstruktur-Migration
├── setup_cron.sh                  # Cron Jobs Installation
├── test_update.R                  # Test-Script für Updates
├── 
├── 📊 Legacy System
├── R/utils.R                      # Original Utils (Referenz)
├── update_data.R                  # Legacy Update Script (deprecated)
├── 
├── 🏗️ Core System
├── DESCRIPTION                    # R Package Dependencies
├── renv.lock                      # Gesperrte Dependency-Versionen
├── R/
│   ├── global.R                   # Globale Konfigurationen
│   ├── data_generation.R          # Datenverarbeitungs-Funktionen
│   └── modules/                   # Shiny Module
├── 
├── 📁 Daten & Logs
├── data/                          # Optimierte RDS-Dateien (6 statt 26+)
├── logs/                          # Update-Logs und Monitoring
├── 
├── 🐳 Deployment
├── Dockerfile                     # Docker Container Konfiguration
├── docker-compose.yml             # Docker Compose Setup
├── nginx.conf                     # Nginx Reverse Proxy
└── www/                          # Statische Web-Assets
```

## 🔄 Automatisches App-Deployment

### Nach Daten-Updates

Das System unterstützt automatische App-Deployments nach erfolgreichen Daten-Updates:

```bash
# In update_nfl_data.R einbauen (am Ende):
if [ $? -eq 0 ]; then
    echo "Deploying updated app to shinyapps.io..."
    Rscript -e "
      library(rsconnect)
      rsconnect::deployApp(
        appDir = '.',
        appName = 'Arcade_Basic_Stats',
        account = 'arcadefantasy',
        forceUpdate = TRUE
      )
    "
fi
```

### Deployment-Strategien

1. **Entwicklung**: Manuelle Deployments nach größeren Änderungen
2. **Daten-Updates**: Automatische Deployments nach erfolgreichen Cron-Updates
3. **Produktions-Rollback**: Automatische Rollback-Mechanismen bei Fehlern

## 🛠️ Daten-Management

### Vereinfachtes Update-System

```bash
# Während der Saison (automatisch via Cron)
# Aktualisiert nur weekly_act.rds, all_snaps_act.rds, ep_act.rds

# Manuell
Rscript update_nfl_data.R --force

# Status prüfen
Rscript test_update.R
```

### Migration & Wartung

```bash
# Einmalige Migration (neue Installation)
Rscript migrate_data_structure.R

# Saisonende (einmal jährlich im März)
Rscript -e "source('R/utils_neu.R'); migrate_current_season_to_historical()"

# System-Status
Rscript -e "source('R/utils_neu.R'); print(get_data_status())"
```

### Fehlerbehandlung & Monitoring

- **Robuste API-Calls**: Automatische Fallbacks bei ESPN/API-Fehlern
- **Comprehensive Logging**: Detaillierte Logs in `logs/nfl_data_update.log`
- **Graceful Degradation**: System läuft auch bei Teilausfällen weiter
- **Data Validation**: Automatische Validierung nach Updates

## 🚢 Deployment

### Shinyapps.io (Automatisch & Deployment-Ready)

Die App ist vollständig deployment-ready mit automatischer ffscrapr-Kompatibilität:

**Live-URL**: **https://arcadefantasy.shinyapps.io/Arcade_Basic_Stats/**

#### **🔧 Deployment-Problem & Lösung**

**Problem**: ffscrapr dev version (benötigt für lokale Updates) kann auf shinyapps.io Probleme verursachen.

**✅ Lösung**: Conditional Loading + Deployment Preparation

```bash
# === Deployment-Ready Workflow ===

# 1. Prüfe Deployment-Readiness
Rscript check_deployment_readiness.R

# 2. Teste App-Kompatibilität 
Rscript test_app_simple.R

# 3. Deployment vorbereiten (backup + stable packages)
Rscript prepare_deployment.R prepare

# 4. App zu shinyapps.io deployen
Rscript prepare_deployment.R deploy

# 5. Lokale Entwicklungsumgebung wiederherstellen
Rscript prepare_deployment.R restore
```

#### **🧠 Intelligente ffscrapr-Behandlung**

Die App verwendet conditional loading für maximale Kompatibilität:

```r
# In app.R und global.R:
if (requireNamespace("ffscrapr", quietly = TRUE)) {
  library(ffscrapr)
  .ffscrapr_available <- TRUE
  message("✓ ffscrapr loaded - Liga integration available")
} else {
  .ffscrapr_available <- FALSE
  message("⚠ ffscrapr not available - Liga integration disabled")
}
```

**Vorteile:**
- ✅ **Lokale Updates**: ffscrapr dev version für robuste ESPN API calls
- ✅ **Deployment**: Graceful fallback wenn ffscrapr nicht verfügbar
- ✅ **Liga-Features**: Bleiben funktional wenn ffscrapr verfügbar ist
- ✅ **Core App**: Läuft auch ohne ffscrapr (mit vorgenerierten Daten)

#### **📦 Deployment-Scripts**

```bash
# Deployment-Readiness prüfen
Rscript check_deployment_readiness.R

# App-Kompatibilität testen (ohne dev packages)
Rscript test_app_simple.R

# Vollständiger Deployment-Workflow
Rscript prepare_deployment.R prepare  # Backup + stable packages
Rscript prepare_deployment.R deploy   # Deploy to shinyapps.io
Rscript prepare_deployment.R restore  # Restore local dev environment
```

#### **🔄 Automatisches Deployment**

```bash
# Auto-Deployment nach Daten-Updates aktivieren
./enable_auto_deploy.sh enable

# Das System deployed dann automatisch nach erfolgreichen Cron Updates
# Verwendet die deployment-ready App-Version mit conditional loading
```

#### **🛠️ Manuelles Deployment**

```r
# Direkt deployen (für Entwickler)
library(rsconnect)
rsconnect::deployApp(
  appDir = ".",
  appName = "Arcade_Basic_Stats", 
  account = "arcadefantasy",
  forceUpdate = TRUE
)
```

### Docker Produktion

```bash
# Produktions-Image mit aktuellen Daten
docker build -t arcade-stats .

# Mit automatischen Updates
docker-compose --profile production up -d

# Updates in Container
docker exec arcade-stats Rscript update_nfl_data.R --force
```

## 📊 Performance-Optimierungen

### Neue Optimierungen (2025)

- **6 statt 26+ Dateien**: Drastisch reduzierte I/O-Zeit
- **Intelligentes Caching**: Lädt nur geänderte Daten
- **Jahresagnostische Struktur**: Keine hardcodierten Jahre mehr
- **Incremental Updates**: Nur aktuelle Saison wird neu geladen
- **Smart Fallbacks**: Graceful Handling von API-Fehlern

### Legacy-Optimierungen

- **Daten-Caching**: Vorverarbeitete `.rds` Dateien für schnelles Laden
- **Reaktive Werte**: Globale reactive Values für WAR- und Liga-Daten
- **Bedingte Rendering**: UI-Updates nur bei Bedarf
- **Waiter Screens**: Loading-Indikatoren für lange Berechnungen

## 🛠️ Technische Details

### Wichtige Libraries
- **Shiny Ecosystem**: shiny, shinyWidgets, shinytoastr, waiter
- **Datenverarbeitung**: tidyverse, dplyr, DT
- **NFL Data**: nflfastR, nflreadr, ffopportunity
- **Fantasy Data**: **ffscrapr (dev version required)**
- **Automation**: R cron scripts, system logging

### Data Pipeline (Optimiert)

1. **Cron Trigger**: Automatische Updates Fr/Mo/Di 09:00 MESZ
2. **Season Detection**: Intelligente Erkennung aktuelle/off-Season
3. **Incremental Loading**: Nur aktuelle Saison wird neu geladen
4. **Smart Merging**: Historical + Current → Final Datasets
5. **App Deployment**: Automatisches Deployment nach erfolgreichem Update
6. **Monitoring**: Comprehensive Logging und Status-Checks

### WAR-Algorithmus
1. **Positionsgruppen**: Dynamische Top-N Spieler basierend auf Liga-Settings
2. **Replacement Level**: Letzter startbare Spieler pro Position
3. **Win Probability**: Normal-Verteilung basierend auf Team-Score-Erwartungen
4. **Consistency**: Standard-Abweichung der wöchentlichen WAR-Werte

## 🚨 Wichtige Hinweise

### Neue Installation
1. **ffscrapr dev version** installieren (erforderlich!)
2. **Einmalige Migration** durchführen (`migrate_data_structure.R`)
3. **Cron Jobs** einrichten (`setup_cron.sh`)

### Bestehende Installation
- Legacy `update_data.R` ist deprecated
- Neue Scripts verwenden `update_nfl_data.R`
- Datenstruktur-Migration erforderlich für Optimierungen

### Monitoring & Logging

Das System bietet umfassende Logging-Funktionen für die Überwachung automatischer Updates:

```bash
# === Haupt-Update-Logs ===
# Live-Monitoring aller Updates
tail -f logs/nfl_data_update.log

# Letzte 20 Einträge anzeigen
tail -20 logs/nfl_data_update.log

# Logs nach Datum filtern
grep "2025-09-12" logs/nfl_data_update.log

# === Cron-spezifische Logs ===
# Freitag Updates (nach Thursday Night Game)
tail -f logs/cron_friday_summer.log     # MESZ (März-Oktober)
tail -f logs/cron_friday_winter.log     # MEZ (November-Februar)

# Montag Updates (nach Sunday Games)  
tail -f logs/cron_monday_summer.log
tail -f logs/cron_monday_winter.log

# Dienstag Updates (nach Monday Night Game)
tail -f logs/cron_tuesday_summer.log
tail -f logs/cron_tuesday_winter.log

# === Deployment-Logs ===
# Auto-Deployment Status
./enable_auto_deploy.sh status

# Deployment-Konfiguration
cat logs/auto_deploy_config.log

# === System-Status ===
# Umfassender Status-Check
Rscript test_update.R

# Aktueller Daten-Status
Rscript -e "source('R/utils_neu.R'); print(get_data_status())"

# Cron Jobs Status
crontab -l | grep update_nfl_data
```

### Log-Dateien Übersicht

```
logs/
├── nfl_data_update.log           # Haupt-Update-Log (alle Ausführungen)
├── cron_friday_summer.log        # Freitag 09:00 MESZ (März-Oktober)
├── cron_monday_summer.log        # Montag 09:00 MESZ (März-Oktober)  
├── cron_tuesday_summer.log       # Dienstag 09:00 MESZ (März-Oktober)
├── cron_friday_winter.log        # Freitag 09:00 MEZ (November-Februar)
├── cron_monday_winter.log        # Montag 09:00 MEZ (November-Februar)
├── cron_tuesday_winter.log       # Dienstag 09:00 MEZ (November-Februar)
├── auto_deploy_config.log        # Deployment-Konfiguration
├── data_migration.log            # Einmalige Datenstruktur-Migration
└── cron_setup.log               # Cron-Installation
```

### Nächste geplante Updates

```bash
# Nächste automatische Updates anzeigen
echo "Nächste NFL Updates (MESZ 09:00):"
date -v+fri '+Freitag:  %d.%m.%Y'
date -v+mon '+Montag:   %d.%m.%Y' 
date -v+tue '+Dienstag: %d.%m.%Y'
```

### Troubleshooting

```bash
# System-Status prüfen
Rscript test_update.R

# Detaillierte Logs prüfen
tail -f logs/nfl_data_update.log

# Cron Jobs prüfen  
crontab -l | grep update_nfl_data

# Rscript-Pfad prüfen (häufiger Fehler)
which Rscript  # Sollte /usr/local/bin/Rscript sein

# Manuelle Migration
Rscript migrate_data_structure.R

# Cron Jobs neu installieren (bei Pfad-Problemen)
./setup_cron.sh
```

### Häufige Probleme & Lösungen

**Problem: Cron Job läuft nicht**
```bash
# 1. Rscript-Pfad prüfen
which Rscript

# 2. Cron Jobs neu installieren
./setup_cron.sh

# 3. Spezifische Cron-Logs prüfen
tail -10 logs/cron_friday_summer.log
```

**Problem: Updates schlagen fehl**
```bash
# 1. ffscrapr dev version prüfen
Rscript -e "packageVersion('ffscrapr')"

# 2. Manuelles Update testen
Rscript update_nfl_data.R --force

# 3. Haupt-Log analysieren
tail -50 logs/nfl_data_update.log | grep ERROR
```

**Problem: Deployment schlägt fehl**
```bash
# 1. rsconnect-Konfiguration prüfen
./enable_auto_deploy.sh test

# 2. Auto-Deployment temporär deaktivieren
./enable_auto_deploy.sh disable

# 3. Manuelles Deployment testen
Rscript -e "rsconnect::deployApp()"
```

## 🤝 Beitragen

1. Repository forken
2. Feature-Branch erstellen
3. Änderungen vornehmen
4. **Daten-Updates testen** mit `test_update.R`
5. Pull Request einreichen

## 📄 Lizenz

MIT License - siehe [LICENSE](LICENSE) Datei für Details.

## 🙏 Danksagungen

- **nflfastR Team** für exzellente NFL-Daten-Tools
- **ffscrapr** für Fantasy-Plattform-Integrationen  
- **ffopportunity** für Expected Points Modelle
- **R Shiny Community** für Framework und Best Practices

---

**🆕 Version 2.0 (2025)**: Komplett automatisiertes System mit jahresagnostischer Datenstruktur, intelligenten Cron Updates und automatischem App-Deployment. Von 26+ Einzeldateien auf 6 optimierte Dateien reduziert für maximale Performance und Wartbarkeit.