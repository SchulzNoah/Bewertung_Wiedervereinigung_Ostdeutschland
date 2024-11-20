# Zeittrendanalyse (Bundesländervergleich) - Tabelle --------

# Setzen des Working Directories ------------------------------------------
setwd("C:/Users/Noah/Desktop/Bachelorarbeit")


# Laden relevanter Packages -----------------------------------------------
library(haven) # Einlesen des ALLBUS
library(tidyverse) # Data-Cleaning, Pipes und Visualisierung
library(gt) # Tabellenerstellung
library(gtExtras) # Verfeinerungen von Tabellen

# Einlesen der Datensätze ------------------------------------------------

allbus = read_dta("allbus2023.dta")
allbus_kumulation = read_dta("allbus_kumulation.dta")


# Funktion zur Datenmanipulation (Durchschnittliche Bewertung pro Bundesland)-------------------------------------------------------
allbus_manipulation <- function(allbus) {
  df <- allbus %>% 
    filter(dg10 %in% c(1:17)) %>% 
    mutate(Bundesland = case_when(
      dg10 == 1 ~ "Baden-Württemberg",
      dg10 == 2 ~ "Bayern",
      dg10 %in% c(3, 12) ~ "Berlin",
      dg10 == 4 ~ "Bremen",
      dg10 == 5 ~ "Hamburg",
      dg10 == 6 ~ "Hessen",
      dg10 == 7 ~ "Niedersachsen",
      dg10 == 8 ~ "Nordrhein-Westfalen",
      dg10 == 9 ~ "Rheinland-Pfalz",
      dg10 == 10 ~ "Saarland",
      dg10 == 11 ~ "Schleswig-Holstein",
      dg10 == 13 ~ "Brandenburg",
      dg10 == 14 ~ "Mecklenburg-Vorpommern",
      dg10 == 15 ~ "Sachsen",
      dg10 == 16 ~ "Sachsen-Anhalt",
      dg10 == 17 ~ "Thüringen")) %>% 
    mutate(pr05 = case_when(
      pr05 == 1 ~ 4,
      pr05 == 2 ~ 3,
      pr05 == 3 ~ 2,
      pr05 == 4 ~ 1)) %>% 
    select(Bundesland, pr05) %>% 
    group_by(Bundesland) %>%
    filter(pr05 != -11) %>%
    summarise(
      Mittelwert = mean(pr05, na.rm = T),
      n = n(),  
      sd = sd(pr05, na.rm = T),
      Standardfehler = qt(0.975, df = n - 1) * (sd / sqrt(n)),
      "Untere Grenze (C.I. 95%)" = Mittelwert - Standardfehler,
      "Obere Grenze (C.I. 95%)" = Mittelwert + Standardfehler)
  
  return(df)
}

# Anwendung der allbus_manipulation-Funktion auf die Datensätze zu den verschiedenen Jahren -----------
df_2023 = allbus_manipulation(allbus = allbus) %>% 
  mutate(Jahr = 2023)
df_2018 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 2018)) %>% 
  mutate(Jahr = 2018)
df_2010 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 2010)) %>% 
  mutate(Jahr = 2010)
df_2006 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 2006)) %>% 
  mutate(Jahr = 2006)
df_1991 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 1991)) %>% 
  mutate(Jahr = 1991)

# Zusammenfassung aller Datensätze zu einem Datensatz: df_zeittrend ------------------------------------------------------------
df_zeittrend = bind_rows(df_2023, df_2018, df_2010, df_2006, df_1991)


# Erstellen der Tabelle mit gt ----------------------------------------------------
df_zeittrend %>%
  group_by(Bundesland, Jahr) %>%
  summarise(Mittelwert,
            `Konfidenzintervall (95%)` = paste0("[", round(`Untere Grenze (C.I. 95%)`, 2), "; ", round(`Obere Grenze (C.I. 95%)`, 2), "]"),
            sd, n) %>% 
  gt() %>%
  tab_header(title = "Zeittrendanalyse: Bewertung der Wiedervereinigung aus ostdeutscher Sicht (Bundesländervergleich)") %>% 
  fmt_number(columns = c(`Mittelwert`, sd),
             decimals = 2) %>% 
  cols_align(align = "center", 
             columns = "Konfidenzintervall (95%)") %>% 
  gt_theme_espn()


# Auflistung der verwendeten Packages -------------------------------------

citation("haven")
citation("tidyverse")
citation("gt")
citation("gtExtras")
