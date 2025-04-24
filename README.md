# 📊 Die Bewertung der Wiedervereinigung in Ostdeutschland: Eine empirische Analyse der Determinanten und zeitlichen Entwicklung auf Basis von ALLBUS-Daten

Dieses Repository enthält sämtliche R-Skripte zu meiner Publikation. Die Analyse eruiert die Determinanten und Zeittrends der Bewertung der Wiedervereinigung in Ostdeutschland mittels OLS-Regression und Längsschnittanalysen. Basis der Analysen sind die **ALLBUS-Kumulation 1980-2021 (ZA5284)** und der **ALLBUS 2023 (ZA8830)**.

---

## 📂 Projektstruktur

```
├── R-Code/         # Alle R-Skripte (für die Längsschnitt- und Querschnitts-Analysen)
├── Data/           # Enthält die beiden verwendeten ALLBUS-Datensätze (im dta-Format)
```

## Hinweis zur Datennutzung

Die Vollversion des **ALLBUS 2023 (ZA8830)** Datensatzes befindet sich nicht im `Data/`-Ordner, sondern es wurde lediglich der **ALLBUScompact 2023 (ZA8831)** beigefügt. Die Vollversion des Datensatzes darf aus datenschutzrechtlichen Gründen nicht geteilt werden.

Die **Vollversion des ALLBUS2023** und Informationen über die abgefragten Variablen können jedoch unter folgendem Link bei GESIS heruntergeladen werden: [ALLBUS 2023 (ZA8830)](https://search.gesis.org/research_data/ZA8830)

Weitere Infos zu den Variablen der **ALLBUS-Kumulation 1980-2021 (ZA5284)** sind [hier](https://search.gesis.org/research_data/ZA5284) zu finden.

---

## 📦 Verwendete R-Packages

Hier sind alle R-Packages (inkl.) Entwicklerinnen und Entwickler aufgelistet, die für die multivariaten Analysen verwendet wurden:

| R-Package             | Entwicklerinnen und Entwickler             |
|------------------------|-------------------------------------------|
| `haven`                | Wickham et al. 2023a                      |
| `tidyverse`            | Wickham et al. 2019                       |
| `gt`                   | Iannone et al. 2024                       |
| `gtExtras`             | Mock 2023                                 |
| `scales`               | Wickham et al. 2023b                      |
| `stargazer`            | Hlavac 2022                               |
| `jtools`               | Long 2022                                 |
| `performance`          | Lüdecke et al. 2021a                      |
| `lmtest`               | Hothorn & Zeileis 2002                    |
| `sandwich`             | Zeileis et al. 2020                       |
| `see`                  | Lüdecke et al. 2021b                      |
| `car`                  | Fox & Weisberg 2019                       |
| `sjPlot`               | Lüdecke 2023                              |
| `writexl`              | Ooms 2024                                 |
| `sf`                   | Pebesma & Bivand 2023                     |
| `rnaturalearth`        | Masicotte & South 2023                    |
| `rnaturalearthdata`    | South et al. 2024                         |
| `ggspatial`            | Dunnington 2023                           |
| `patchwork`            | Pedersen 2024                             |

---

## 📚 Zitation

Die vollständige Publikation ist [hier](www.google.de) zu finden.

Eine mögliche Zitationen meiner Publikation lautet:
> Schulz, Noah (2025). *Die Bewertung der Wiedervereinigung in Ostdeutschland: Eine empirische Analyse der Determinanten und zeitlichen Entwicklung auf Basis von ALLBUS-Daten*. [Link](www.google.de).

---

## 📬 Kontakt

Bei Fragen, Anregungen oder Interesse an einer Vernetzung:

**Noah Schulz**  
B.A. Politikwissenschaft | R-User | Quantitativer Sozialforscher

**LinkedIn-Profil:** [<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/LinkedIn_icon.svg/800px-LinkedIn_icon.svg.png" width="30" />](https://www.linkedin.com/in/noah-schulz-971031301/)

**E-Mail:** *noah.schulz@stud.uni-due.de* oder *Noah.Schulz@edu.ruhr-uni-bochum.de*



---
