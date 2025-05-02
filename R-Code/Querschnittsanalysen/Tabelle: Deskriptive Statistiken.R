#  Tabelle mit deskriptiven Statistiken ---------------------

# Laden der relevanten Packages -------------------------------------------

library(haven)     # Einlesen des Datensatzes
library(tidyverse) # Data Cleaning, Pipes
library(writexl)   # Speichern des Dataframes in Excel-Datei

# Einlesen des Datensatzes ------------------------------------------------

allbus = read_dta("allbus2023.dta")
 
# Datenmanipulation -------------------------------------------------------

df_ost = allbus %>%
  mutate(educ = ifelse(educ %in% c(1:5), educ, NA),
         incc = ifelse(incc > 0, incc, NA),
         pa01 = ifelse(pa01 > 0, pa01, NA),
         age = ifelse(age > 0, age, NA),
         sex = ifelse(sex %in% c(1, 2), sex, NA),
         id01 = ifelse(id01 > 0, id01, NA)) %>% 
  mutate(ep03 = case_when(
    ep03 == 1 ~ 5,
    ep03 == 2 ~ 4,
    ep03 == 3 ~ 3,
    ep03 == 4 ~ 2,
    ep03 == 5 ~ 1),
    pn12 = case_when(
      pn12 == 1 ~ 4,
      pn12 == 2 ~ 3,
      pn12 == 3 ~ 2,
      pn12 == 4 ~ 1),
    pr10 = case_when(
      pr10 == 1 ~ 4,
      pr10 == 2 ~ 3,
      pr10 == 3 ~ 2,
      pr10 == 4 ~ 1),
    pn13 = case_when(
      pn13 == 1 ~ 4,
      pn13 == 2 ~ 3,
      pn13 == 3 ~ 2,
      pn13 == 4 ~ 1),
    dw18 = case_when(
      dw18 == 1 ~ 1,
      dw18 == 2 ~ 0),
    gs01 = case_when(
      gs01 == 1 ~ 5,
      gs01 == 2 ~ 4,
      gs01 == 3 ~ 3,
      gs01 == 4 ~ 2,
      gs01 == 5 ~ 1),
    pr05 = case_when(
      pr05 == 1 ~ 4,
      pr05 == 2 ~ 3,
      pr05 == 3 ~ 2,
      pr05 == 4 ~ 1), 
    ident_bundesland =
      case_when(pn13 == 1 ~ 4,
                pn13 == 2 ~ 3,
                pn13 == 3 ~ 2,
                pn13 == 4 ~ 1)) %>% 
  mutate(linkspartei = ifelse(pv01 == 6, 1, 0)) %>% 
  mutate(afd = ifelse(pv01 == 42, 1, 0)) %>% 
  rename("Bewertung der Wiedervereinigung in Ostdeutschland" = pr05,
         Alter = age,
         Geschlecht = sex,
         "Wohnort (Stadt/Land)" = gs01,
         "Formeller Schulabschluss" = educ,
         "Sozialistische Einstellungen" = pr10,
         "Wahl der Linkspartei" = linkspartei,
         "Einkommen (kategorisiert)" = incc,
         "Eigene wirtschaftliche Lage" = ep03,
         "Arbeitslosigkeit in den letzten zehn Jahren" = dw18,
         "Verbundenheit mit Bundesland" = ident_bundesland,
         "Verbundenheit mit Gemeinde" = pn12,
         "Gefühl des gerechten Anteils" = id01,
         "Wahl der AfD" = afd) %>% 
  filter(eastwest == 2) %>% 
  select("Bewertung der Wiedervereinigung in Ostdeutschland", "Alter", 
         "Geschlecht", "Wohnort (Stadt/Land)",
         "Formeller Schulabschluss", "Sozialistische Einstellungen", 
         "Wahl der Linkspartei","Einkommen (kategorisiert)", 
         "Eigene wirtschaftliche Lage", 
         "Arbeitslosigkeit in den letzten zehn Jahren", 
         "Verbundenheit mit Bundesland", "Verbundenheit mit Gemeinde",
         "Wahl der AfD", "Gefühl des gerechten Anteils")

# Funktion der Berechnung der wichtigen deskriptiven Statistiken --------------------

desk_statistik <- function(variable) {
  n <- sum(!is.na(variable))
  conf_int <- t.test(variable, conf.level = 0.95)$conf.int
  conf_int <- paste0("[", round(conf_int[1], 2), "; ", round(conf_int[2], 2), "]")
  return(c("CI (95%)" = conf_int,
    "μ" = round(mean(variable, na.rm = T), 2),
    "σ" = round(sd(variable, na.rm = T), 2),
    "Min" = min(variable, na.rm = T),
    "Max" = max(variable, na.rm = T),
    "n" = n))
}

# Anwendung der Funktion zur Berechnung der deskriptiven Statistik auf df_ost
df <- sapply(df_ost, desk_statistik)

# Speichern als Dataframe
df <- as.data.frame(t(df))

# Hinzufügen der Variablennamen
df <- cbind(Variable = names(df_ost), df)

# Speichern des Dataframes in Excel-Datei (für die weitere Bearbeitung)
write_xlsx(df, "Tabelle - Deskriptive Statistik.xlsx")

# Auflistung der verwendeten Packages -------------------------------------

citation("haven")
citation("tidyverse")
citation("writexl")









