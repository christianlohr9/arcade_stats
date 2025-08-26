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

## 🚀 Quick Start

### Voraussetzungen

- R (>= 4.5.0)
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
   ```

3. **Datendateien generieren**
   ```bash
   Rscript update_data.R 2024
   ```

4. **Anwendung starten**
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

## 📁 Projektstruktur

```
arcade_stats/
├── app.R                    # Haupt-Shiny-Anwendung (vollständige WAR-Implementation)
├── shiny_base_stats.R      # Backup/Referenz der ursprünglichen Implementierung (gitignore)
├── DESCRIPTION              # R Package Dependencies
├── renv.lock               # Gesperrte Dependency-Versionen
├── update_data.R           # Daten-Generierungs-Script
├── 
├── R/                      # R Quellcode
│   ├── global.R           # Globale Konfigurationen und Utilities
│   ├── data_generation.R  # Datenverarbeitungs-Funktionen
│   └── utils.R            # Utility-Funktionen
├── 
├── data/                  # Datendateien (.rds)
│   ├── weekly_stats.rds   # Hauptdaten für offensive Statistiken
│   ├── weekly_stats_def.rds # Hauptdaten für defensive Statistiken (IDP)
│   └── [weitere .rds Dateien...]
├── 
├── www/                   # Statische Web-Assets
│   └── logo.png          # App-Logo
├── 
├── Dockerfile             # Docker Container Konfiguration
├── docker-compose.yml     # Docker Compose Setup
└── nginx.conf            # Nginx Reverse Proxy Konfiguration
```

## 🔧 Daten-Management

### Daten aktualisieren

Die Anwendung verwendet vorverarbeitete Datendateien, die als `.rds`-Dateien im `data/`-Verzeichnis gespeichert sind:

```bash
# Aktualisierung für das aktuelle Jahr
Rscript update_data.R

# Aktualisierung für bestimmte Jahre  
Rscript update_data.R 2023 2024

# Aktualisierung für einen Jahresbereich
Rscript update_data.R 2020 2021 2022 2023 2024
```

### Datenquellen

- **nflfastR**: Play-by-Play NFL-Daten
- **ffscrapr**: Fantasy Football Platform APIs (Sleeper, MFL)
- **nflreadr**: NFL Roster- und Team-Informationen

## 🎯 WAR-Berechnung

Die App implementiert eine vollständige Wins Above Replacement Berechnung mit:

- **Statistische Methoden**: Verwendung von pnorm() für Win-Wahrscheinlichkeiten
- **Replacement Level**: Dynamische Berechnung basierend auf Liga-Parametern
- **Alle Positionen**: QB, RB, WR, TE + IDP (DT, DE, LB, CB, S)
- **Flex-Positionen**: WR/TE, RB/WR/TE, IDP-Flex Support
- **Liga-Integration**: Direkter Import von Sleeper/MFL Liga-Daten
- **Konsistenz-Metriken**: VOR (Value over Replacement) Berechnungen

## 🚢 Deployment

### Shinyapps.io

Die App ist bereits live unter: **https://arcadefantasy.shinyapps.io/Arcade_Basic_Stats/**

Für neue Deployments:

```r
library(rsconnect)

# Deploy zu shinyapps.io
rsconnect::deployApp(
  appDir = ".",
  appName = "Arcade_Basic_Stats",
  account = "arcadefantasy"
)
```

### Docker Produktion

Für Produktions-Deployments die bereitgestellte Docker-Konfiguration verwenden:

```bash
# Produktions-Image erstellen
docker build -t arcade-stats .

# Mit docker-compose starten
docker-compose --profile production up -d
```

## 📊 Daten-Pipeline

1. **Rohdaten**: nflfastR lädt Play-by-Play Daten
2. **Verarbeitung**: `R/data_generation.R` verarbeitet und erweitert Daten
3. **Speicherung**: Verarbeitete Daten als `.rds` Dateien in `data/`
4. **App-Loading**: `app.R` lädt vorverarbeitete Daten für schnellen Zugriff

## ⚡ Performance-Optimierungen

- **Daten-Caching**: Vorverarbeitete `.rds` Dateien für schnelles Laden
- **Reaktive Werte**: Globale reactive Values für WAR- und Liga-Daten
- **Bedingte Rendering**: UI-Updates nur bei Bedarf
- **Waiter Screens**: Loading-Indikatoren für lange Berechnungen
- **Docker-Optimierung**: Multi-Stage Builds und Caching

## 🛠️ Technische Details

### Wichtige Libraries
- **Shiny Ecosystem**: shiny, shinyWidgets, shinytoastr, waiter
- **Datenverarbeitung**: tidyverse, dplyr, DT
- **NFL Data**: nflfastR, nflreadr
- **Fantasy Data**: ffscrapr
- **Statistik**: psych (für describe() in WAR-Berechnungen)

### WAR-Algorithmus
1. **Positionsgruppen**: Dynamische Top-N Spieler basierend auf Liga-Settings
2. **Replacement Level**: Letzter startbare Spieler pro Position
3. **Win Probability**: Normal-Verteilung basierend auf Team-Score-Erwartungen
4. **Consistency**: Standard-Abweichung der wöchentlichen WAR-Werte

## 🤝 Beitragen

1. Repository forken
2. Feature-Branch erstellen
3. Änderungen vornehmen
4. Gründlich testen
5. Pull Request einreichen

## 📄 Lizenz

MIT License - siehe [LICENSE](LICENSE) Datei für Details.

## 🙏 Danksagungen

- **nflfastR Team** für exzellente NFL-Daten-Tools
- **ffscrapr** für Fantasy-Plattform-Integrationen  
- **R Shiny Community** für Framework und Best Practices

---

**Hinweis**: Diese Anwendung wurde von einer monolithischen Struktur refaktored, um Wartbarkeit, Performance und Deployment-Flexibilität zu verbessern. Die vollständige WAR-Implementierung aus `shiny_base_stats.R` wurde erfolgreich in `app.R` integriert.