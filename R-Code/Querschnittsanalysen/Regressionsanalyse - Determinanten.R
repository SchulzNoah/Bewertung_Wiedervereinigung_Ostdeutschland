# Regressionsanalyse der Determinanten ------------------------------------

# Laden der relevanten Packages -------------------------------------------

library(haven)       # Einlesen des ALLBUS
library(tidyverse)   # Datacleaning, Datenvisualisierung, Pipes
library(stargazer)   # Erstellung von Regressionstabellen
library(lmtest)      # Berechnung der robusten Standardfehler
library(sandwich)    # Berechnung der robusten Standardfehler
library(performance) # Testung der Annahmen der OLS-Regression
library(jtools)      # Visualisierung der Regressionskoeffizienten
library(see)         # theme_lucid()
library(car)         # Berechnung der Varianz-Inflations-Faktoren
library(gt)          # Erstellung der Tabellen
library(gtExtras)    # Verfeinerungen der Tabellen
library(sjPlot)      # Visualisierung der Interaktion

# Einlesen des ALLBUS-Datensatzes ------------------------------------------------

allbus = read_dta("allbus2023.dta")


# Manipulation des ALLBUS für Ostdeutschland ----------------------

# (Um-)kodierung der Variablen, Filtern, Umbenennung


df_ost = allbus %>%
  mutate(educ = ifelse(educ %in% c(1:5), educ, NA),
         incc = ifelse(incc > 0, incc, NA),
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
      pr05 == 4 ~ 1)) %>% 
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
         "Verbundenheit mit Bundesland" = pn13,
         "Verbundenheit mit Gemeinde" = pn12,
         "Relative Deprivation: Gerechtigkeitsempfinden" = id01,
         "Wahl der AfD" = afd) %>% 
  filter(eastwest == 2)


# Aufstellen der Regressionsmodelle ---------------------------------------

# Socialist Legacy Approach
m1 = lm(data = df_ost,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss`+
          `Sozialistische Einstellungen` + `Wahl der Linkspartei`)

summary(m1)

# Ökonomische Situation
m2 = lm(data = df_ost,
        `Bewertung der Wiedervereinigung in Ostdeutschland`~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` + 
          `Einkommen (kategorisiert)` + `Eigene wirtschaftliche Lage`+
          `Arbeitslosigkeit in den letzten zehn Jahren`)

summary(m2)


# Soziale Identität/Lokale Verbundenheit
m3 = lm(data = df_ost,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~ 
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
        `Verbundenheit mit Bundesland` + `Verbundenheit mit Gemeinde`)

summary(m3)


# Relative Deprivation
m4 = lm(data = df_ost,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Wahl der AfD` + `Relative Deprivation: Gerechtigkeitsempfinden`)

summary(m4)


# Alle Erklärungsansätze
m5 = lm(data = df_ost,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Sozialistische Einstellungen` + `Wahl der Linkspartei`+
          `Einkommen (kategorisiert)` + `Eigene wirtschaftliche Lage`+
          `Arbeitslosigkeit in den letzten zehn Jahren`+
          `Verbundenheit mit Bundesland` + `Verbundenheit mit Gemeinde`+
          `Wahl der AfD` + `Relative Deprivation: Gerechtigkeitsempfinden`)

summary(m5)


# Alle Erklärungsansätze + Interaktion

m6 = lm(data = df_ost,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Sozialistische Einstellungen` + `Wahl der Linkspartei`+
          `Einkommen (kategorisiert)` + `Eigene wirtschaftliche Lage`+
          `Arbeitslosigkeit in den letzten zehn Jahren`* `Wahl der AfD`+
          `Verbundenheit mit Bundesland` + `Verbundenheit mit Gemeinde` +
          `Relative Deprivation: Gerechtigkeitsempfinden`)

summary(m6)





# Erstellung der Regressionstabelle ---------------------------------------

# Für Determinationskoeffizienten, F-Statistik etc.

stargazer(m1, m2, m3, m4, m5, m6,
          type = "html",
          style = "apsr",
          decimal.mark = ",",
          covariate.labels = c("Alter", "Geschlecht", "Wohnort (Stadt/Land)", 
                               "Formeller Schulabschluss", "Sozialistische Einstellungen",
                               "Wahl der Linkspartei", "Einkommen (kategorisiert)",
                               "Eigene wirtschaftliche Lage", "Arbeitslosigkeit in den letzten zehn Jahren",
                               "Verbundenheit mit Bundesland", "Verbundenheit mit Gemeinde",
                               "Wahl der AfD", "Relative Deprivation: Gerechtigkeitsempfinden",
                               "Arbeitslosigkeit * Wahl der AfD"), 
          dep.var.labels = "Bewertung der Wiedervereinigung für Ostdeutschland",
          out = "Regressionstabelle.html",
          out.header = T)


# Testung der Annahmen der OLS-Regression ---------------------------------

# Verteilung der abhängigen Variablen 

ggplot(aes(x = `Bewertung der Wiedervereinigung in Ostdeutschland`),
       data = df_ost)+
  geom_histogram(fill = "steelblue")+
  theme_minimal()+
  ylab("Häufigkeit")

# Homoskedastizität (Breusch-Pagan-Test)

bptest(m1)
bptest(m2)
bptest(m3)
bptest(m4)
bptest(m5)
bptest(m6)


# Zusammenfassung der Ergebnisse in einem Dataframe

df_bptest = data.frame(Modell = paste0("m", 1:6),
           p = c(bptest(m1)$p.value, 
             bptest(m2)$p.value, 
             bptest(m3)$p.value,
             bptest(m4)$p.value, 
             bptest(m5)$p.value, 
             bptest(m6)$p.value))

df_bptest$p <- round(df_bptest$p, 4)

# Visualisierung der Ergebnisse der Breusch-Pagan-Tests (Tabelle)

df_bptest %>%
  gt()%>% 
  tab_header(title = "Resultate der Breusch-Pagan-Tests für Regressionsmodelle ") %>% 
  gt_theme_espn()

# Visuelle Überprüfung der Homoskedastizität

plot(m1, 1, main = "Homoskedastizitätsüberprüfung (m1)", sub ="")
plot(m2, 1, main = "Homoskedastizitätsüberprüfung (m2)", sub ="")
plot(m3, 1, main = "Homoskedastizitätsüberprüfung (m3)", sub ="")
plot(m4, 1, main = "Homoskedastizitätsüberprüfung (m4)", sub ="")
plot(m5, 1, main = "Homoskedastizitätsüberprüfung (m5)", sub ="")
plot(m6, 1, main = "Homoskedastizitätsüberprüfung (m6)", sub ="")


# Normalverteilung der Residuen (Shapiro-Wilk-Test)

shapiro.test(m1$residuals)
shapiro.test(m2$residuals)
shapiro.test(m3$residuals)
shapiro.test(m4$residuals)
shapiro.test(m5$residuals)
shapiro.test(m6$residuals) 

# Zusammenfassung der Ergebnisse in einem Dataframe

df_swtest = data.frame(Modell = paste0("m", 1:6),
                       p = c(shapiro.test(m1$residuals)$p.value,
                             shapiro.test(m2$residuals)$p.value,
                             shapiro.test(m3$residuals)$p.value,
                             shapiro.test(m4$residuals)$p.value,
                             shapiro.test(m5$residuals)$p.value,
                             shapiro.test(m6$residuals)$p.value))


df_swtest$p <- round(df_swtest$p, 4)

# Visualisierung der Ergebnisse der Shaprio-Wilk-Tests

df_swtest %>%
  gt()%>% 
  tab_header(title = "Resultate der Shapiro-Wilk-Tests für Regressionsmodelle ") %>% 
  gt_theme_espn()

# Visuelle Testung der Normalverteilung der Residuen (QQ-Plot)

plot(m1, 2, main = "Normalverteilung der Residuen (m1)", sub = "")
plot(m2, 2, main = "Normalverteilung der Residuen (m2)", sub = "")
plot(m3, 2, main = "Normalverteilung der Residuen (m3)", sub = "")
plot(m4, 2, main = "Normalverteilung der Residuen (m4)", sub = "")
plot(m5, 2, main = "Normalverteilung der Residuen (m5)", sub = "")
plot(m6, 2, main = "Normalverteilung der Residuen (m6)", sub = "")


# Keine Multikollinearität

check_collinearity(m1)
check_collinearity(m2)
check_collinearity(m3)
check_collinearity(m4)
check_collinearity(m5)
check_collinearity(m6)

# Berechnung der Varianz-Inflations-Faktoren (Bestimmung der Multikollinearität)

vif(m1)
vif(m2)
vif(m3)
vif(m4)
vif(m5)
vif(m6)

# Visualisierung der Varianzinflationsfaktoren in einer Tabelle

vif_list <- list(
  Modell_1 = check_collinearity(m1),
  Modell_2 = check_collinearity(m2),
  Modell_3 = check_collinearity(m3),
  Modell_4 = check_collinearity(m4),
  Modell_5 = check_collinearity(m5),
  Modell_6 = check_collinearity(m6))


var_order <- c(
  "Alter", "Geschlecht", "Wohnort (Stadt/Land)", "Formeller Schulabschluss",
  "Sozialistische Einstellungen", "Wahl der Linkspartei",
  "Einkommen (kategorisiert)", "Eigene wirtschaftliche Lage",
  "Arbeitslosigkeit in den letzten zehn Jahren",
  "Verbundenheit mit Bundesland", "Verbundenheit mit Gemeinde","Wahl der AfD",
  "Relative Deprivation: Gerechtigkeitsempfinden", "Arbeitslosigkeit in den letzten zehn Jahren:Wahl der AfD")


vif_table <- purrr::imap_dfr(vif_list, ~ .x %>%
                               select(Term, VIF) %>%
                               rename(Variable = Term, !!.y := VIF),
                             .id = NULL) %>%
  group_by(Variable) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop") %>% 
  mutate(across(-Variable, ~ round(., 2))) %>% 
  mutate(Variable = factor(Variable, levels = var_order)) %>%
  arrange(Variable)

vif_table

# Schreiben der Tabelle in eine Excel-Datei

writexl::write_xlsx(vif_table, "Tabelle mit VIF-Werten.xlsx")

# Keine Autokorrelation der Residuen

check_autocorrelation(m1)
check_autocorrelation(m2)
check_autocorrelation(m3)
check_autocorrelation(m4)
check_autocorrelation(m5)
check_autocorrelation(m6)


# Zusammenfassung der Durbin-Watson-Tests

df_dwtest = data.frame(Modell = paste0("m", 1:6),
                       p = c(dwtest(m1)$p.value,
                             dwtest(m2)$p.value,
                             dwtest(m3)$p.value,
                             dwtest(m4)$p.value,
                             dwtest(m5)$p.value,
                             dwtest(m6)$p.value))


df_dwtest$p <- round(df_dwtest$p, 4)

# Visualisierung der Ergebnisse der Durbin-Watson-Tests (Tabelle)

df_dwtest %>%
  gt()%>% 
  tab_header(title = "Resultate der Durbin-Watson-Tests für Regressionsmodelle ") %>% 
  gt_theme_espn()


# Keine einflussreichen Ausreißer (Cook's Distance)

plot(m1, 4, main = "Ausreißer-Diagnose (m1)", sub = "")
plot(m2, 4, main = "Ausreißer-Diagnose (m2)", sub = "")
plot(m3, 4, main = "Ausreißer-Diagnose (m3)", sub = "")
plot(m4, 4, main = "Ausreißer-Diagnose (m4)", sub = "")
plot(m5, 4, main = "Ausreißer-Diagnose (m5)", sub = "")
plot(m6, 4, main = "Ausreißer-Diagnose (m6)", sub = "")



# Regressionstabelle mit standardisierten Regressionskoeffizienten -------------------------


# Berechnung der robusten HC3-Standardfehler

m1_robust = coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
m2_robust = coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
m3_robust = coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
m4_robust = coeftest(m4, vcov = vcovHC(m4, type = "HC3"))
m5_robust = coeftest(m5, vcov = vcovHC(m5, type = "HC3"))
m6_robust = coeftest(m6, vcov = vcovHC(m6, type = "HC3"))

# Darstellung mit Stargazer (für die robusten Standardfehler)

stargazer(m1_robust, m2_robust, m3_robust, m4_robust, m5_robust, m6_robust,
          type = "html",
          style = "ajps",
          out = "Regressionstabelle_mit robusten Standardfehlern.html",
          out.header = T)

# Erstellung der Visualisierung mit standardisierten Regressionskoeffizienten 
# und robusten Standardfehlern--------


# Berechnung der standardisierten Regressionskoeffizienten

df_ost_stand = df_ost %>%
  select(`Bewertung der Wiedervereinigung in Ostdeutschland`, Alter, Geschlecht, `Wohnort (Stadt/Land)`,
         `Formeller Schulabschluss`, `Sozialistische Einstellungen`, `Wahl der Linkspartei`,
         `Einkommen (kategorisiert)`, `Eigene wirtschaftliche Lage`, `Arbeitslosigkeit in den letzten zehn Jahren`,
         `Verbundenheit mit Bundesland`, `Verbundenheit mit Gemeinde`, `Wahl der AfD`,
         `Relative Deprivation: Gerechtigkeitsempfinden`) %>%
  mutate(across(where(is.numeric), scale))

m1_stand = lm(data = df_ost_stand,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss`+
          `Sozialistische Einstellungen` + `Wahl der Linkspartei`)
summary(m1_stand)


m2_stand = lm(data = df_ost_stand,
        `Bewertung der Wiedervereinigung in Ostdeutschland`~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` + 
          `Einkommen (kategorisiert)` + `Eigene wirtschaftliche Lage`+
          `Arbeitslosigkeit in den letzten zehn Jahren`)
summary(m2_stand)

m3_stand = lm(data = df_ost_stand,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~ 
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Verbundenheit mit Bundesland` + `Verbundenheit mit Gemeinde`)

summary(m3_stand)

m4_stand = lm(data = df_ost_stand,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Wahl der AfD` + `Relative Deprivation: Gerechtigkeitsempfinden`)

summary(m4_stand)

m5_stand = lm(data = df_ost_stand,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Sozialistische Einstellungen` + `Wahl der Linkspartei`+
          `Einkommen (kategorisiert)` + `Eigene wirtschaftliche Lage`+
          `Arbeitslosigkeit in den letzten zehn Jahren`+
          `Verbundenheit mit Bundesland` + `Verbundenheit mit Gemeinde`+
          `Wahl der AfD` + `Relative Deprivation: Gerechtigkeitsempfinden`)

summary(m5_stand)


m6_stand = lm(data = df_ost_stand,
        `Bewertung der Wiedervereinigung in Ostdeutschland` ~
          Alter + Geschlecht + `Wohnort (Stadt/Land)` + `Formeller Schulabschluss` +
          `Sozialistische Einstellungen` + `Wahl der Linkspartei`+
          `Einkommen (kategorisiert)` + `Eigene wirtschaftliche Lage`+
          `Arbeitslosigkeit in den letzten zehn Jahren`* `Wahl der AfD`+
          `Verbundenheit mit Bundesland` + `Verbundenheit mit Gemeinde` +
          `Relative Deprivation: Gerechtigkeitsempfinden`)

summary(m6_stand)


# Robuste Standardfehler für standardisierte Regressionskoeffizienten

m1_sr = coeftest(m1_stand, vcov = vcovHC(m1_stand, type = "HC3"))
m2_sr = coeftest(m2_stand, vcov = vcovHC(m2_stand, type = "HC3"))
m3_sr = coeftest(m3_stand, vcov = vcovHC(m3_stand, type = "HC3"))
m4_sr = coeftest(m4_stand, vcov = vcovHC(m4_stand, type = "HC3"))
m5_sr = coeftest(m5_stand, vcov = vcovHC(m5_stand, type = "HC3"))
m6_sr = coeftest(m6_stand, vcov = vcovHC(m6_stand, type = "HC3"))


# Visualisierung der standardisierten Regresionskoeffizienten (mit 95%-C.I.)

plot_coefs(m1_sr, m2_sr, m3_sr, m4_sr, m5_sr, m6_sr,
           legend.title = "Regressionsmodell",
           coefs = c("`Sozialistische Einstellungen`",
                     "`Wahl der Linkspartei`",
                     "`Einkommen (kategorisiert)`",
                     "`Eigene wirtschaftliche Lage`",
                     "`Arbeitslosigkeit in den letzten zehn Jahren`",
                     "`Verbundenheit mit Bundesland`",
                     "`Verbundenheit mit Gemeinde`",
                     "`Relative Deprivation: Gerechtigkeitsempfinden`",
                     "`Wahl der AfD`",
                     "`Arbeitslosigkeit in den letzten zehn Jahren`:`Wahl der AfD`"
           )) +
  theme_minimal(base_family = "serif") +
  labs(x = "Standardisierte Regressionskoeffizienten mit 95%-Konfidenzintervall",
       y = "",
       title = "Effektstärke der Prädiktoren für die Bewertung der Wiedervereinigung aus ostdeutscher Perspektive",
       color = "Regressionsmodell",
       shape = "Regressionsmodell") +
  scale_y_discrete(labels = c(
    "`Sozialistische Einstellungen`" = "Sozialistische Einstellungen",
    "`Wahl der Linkspartei`" = "Wahl der Linkspartei",
    "`Einkommen (kategorisiert)`" = "Einkommen (kategorisiert)",
    "`Eigene wirtschaftliche Lage`" = "Eigene wirtschaftliche Lage",
    "`Arbeitslosigkeit in den letzten zehn Jahren`" = "Arbeitslosigkeit in den letzten zehn Jahren",
    "`Verbundenheit mit Bundesland`" = "Verbundenheit mit Bundesland",
    "`Verbundenheit mit Gemeinde`" = "Verbundenheit mit Gemeinde",
    "`Relative Deprivation: Gerechtigkeitsempfinden`" = "Relative Deprivation:\n Gerechtigkeitsempfinden",
    "`Wahl der AfD`" = "Wahl der AfD",
    "`Arbeitslosigkeit in den letzten zehn Jahren`:`Wahl der AfD`" = 
      "Arbeitslosigkeit in den letzten zehn Jahren \n* Wahl der AfD"))+
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(
    values = c("Model 1" = "#49b7fc", "Model 2" = "#ff7b00", "Model 3" = "#17d898", 
               "Model 4" = "#ff0083", "Model 5" = "#0015ff", "Model 6" = "#e5d200"),
    breaks = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
    labels = c("m1", "m2", "m3", "m4", "m5", "m6")
  ) +
  scale_shape_manual(
    values = c("Model 1" = 21, "Model 2" = 22, "Model 3" = 23, 
               "Model 4" = 24, "Model 5" = 25, "Model 6" = 15),
    breaks = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
    labels = c("m1", "m2", "m3", "m4", "m5", "m6")
  )+
  theme(plot.title = element_text(hjust = .7,
                                  size = 16,
                                  face = "bold",
                                  margin = margin(b= 15)),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12,
                                 face = "bold"),
        legend.title = element_text(size = 13,
                                    face = "bold"),
        legend.text = element_text(size = 13),
        plot.margin = margin(t = 15, b = 10, r = 5, unit = "pt"))



# Erstellung des Interaktionsplots zwischen Arbeitslosigkeit und AfD-Wahl ----------------------------------------------

# Umbenennung der Variablen (leichtere Handhabung)

df_ost_int = df_ost %>%
  rename(
    bewertung_wv = `Bewertung der Wiedervereinigung in Ostdeutschland`,
    wohnort = `Wohnort (Stadt/Land)`,
    schulabschluss = `Formeller Schulabschluss`,
    sozialismus = `Sozialistische Einstellungen`,
    linkswahl = `Wahl der Linkspartei`,
    einkommen = `Einkommen (kategorisiert)`,
    wirtschaft = `Eigene wirtschaftliche Lage`,
    arbeitslos = `Arbeitslosigkeit in den letzten zehn Jahren`,
    afd_wahl = `Wahl der AfD`,
    verbunden_bl = `Verbundenheit mit Bundesland`,
    verbunden_gem = `Verbundenheit mit Gemeinde`,
    deprivation = `Relative Deprivation: Gerechtigkeitsempfinden` ) %>% 
  mutate(arbeitslos = factor(arbeitslos, labels = c("Nein", "Ja")),
         afd_wahl = factor(afd_wahl, labels = c("Nein", "Ja")))


# Erneute Aufstellung des sechsten Regressionsmodells

m6_neu <- lm(data = df_ost_int,
              bewertung_wv ~ Alter + Geschlecht + wohnort + schulabschluss +
                sozialismus + linkswahl + einkommen + wirtschaft +
                arbeitslos * afd_wahl + verbunden_bl + verbunden_gem + deprivation)

summary(m6_neu)


# Erstellen des Basic Plots (inkl. robuster HC3-Standardfehler)

p = plot_model(m6_neu,
           type = "pred",
           robust = T,
           vcov.args = "HC3",
           terms = c("arbeitslos", "afd_wahl"),
           ci.lvl = 0.95,
           axis.title = c("Arbeitslosigkeit in den letzten zehn Jahren", "Vorhergesagte Bewertung der Wiedervereinigung"))+
  labs(color = "Wahl der AfD",
       title = "Interaktion zwischen Arbeitslosigkeit und Wahl der AfD in Bezug 
              auf die Bewertung der Wiedervereinigung",
       subtitle = "Regressionsmodell: m6")+
  theme_lucid(base_family = "serif")+
  theme(plot.title = element_text(hjust = .5,
                                  size = 17,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = .5,
                                     size = 14),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 11,
                                   color = "grey30"),
        axis.text.x = element_text(size = 13,
                                   color = "grey30"),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.margin = margin(t = 10, l = 15, r = 10, b = 10))


# Herausfinden der x und y-Werte der Punkte
pdata <- ggplot_build(p)$data[[1]]


# Extraktion dieser, um Punkte mit Linien miteinander zu verknüpfen
line_data <- data.frame(
  x = c(pdata$x[1], pdata$x[3], pdata$x[2], pdata$x[4]),
  y = c(pdata$y[1], pdata$y[3], pdata$y[2], pdata$y[4]),
  group = factor(c(1, 1, 2, 2)))


# Finaler Interaktionsplot

p +
  geom_line(data = subset(line_data, group == 1),
            aes(x = x, y = y),
            color = "#e41a1c",
            linetype = "dashed",
            linewidth = 1.2,
            inherit.aes = F) +
  geom_line(data = subset(line_data, group == 2),
            aes(x = x, y = y),
            color = "#377eb8",
            linewidth = 1.2,
            inherit.aes = F)


# Auflistung aller verwendeten R-Packages -----------------------------------

citation("haven")
citation("tidyverse")
citation("stargazer")
citation("lmtest")
citation("sandwich")
citation("performance")
citation("jtools")
citation("see")
citation("car")
citation("interactions")
citation("gt")
citation("gtExtras")
citation("sjPlot")

