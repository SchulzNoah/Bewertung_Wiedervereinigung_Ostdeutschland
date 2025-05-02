# Tabelle: Zeittrendanalyse (Ost-West-Vergleich) --------------------------

# Laden relevanter Packages -----------------------------------------------

library(haven)     # Einlesen des ALLBUS
library(tidyverse) # Data-Cleaning, Pipes und Visualisierung
library(gt)        # Erstellung von Tabellen
library(gtExtras)  # Verfeinerungen von Tabellen

# Einlesen der Datensätze ------------------------------------------------

allbus = read_dta("allbus2023.dta")
allbus_kumulation = read_dta("allbus_kumulation.dta")

# Datenmanipulation -------------------------------------------------------

# Funktion zur Berechnung der Mittelwerte (inkl. Konfidenzintervalle)
# pro Jahr und Landesteil

allbus_manipulation <- function(allbus) {
  df <- allbus %>%
    mutate(eastwest = case_when(
      eastwest == 1 ~ "Alte Bundesländer",
      eastwest == 2 ~ "Neue Bundesländer"),
      pr05 = case_when(
        pr05 == 1 ~ 4,
        pr05 == 2 ~ 3,
        pr05 == 3 ~ 2,
        pr05 == 4 ~ 1)) %>%
    filter(pr05 != -11) %>%
    group_by(Jahr, eastwest) %>% 
  summarise(
    Mittelwert = mean(pr05, na.rm = T),
    n = n(),  
    sd = sd(pr05, na.rm = T),
    Standardfehler = qt(0.975, df = n - 1) * (sd / sqrt(n)),
    "Untere Grenze (C.I. 95%)" = Mittelwert - Standardfehler,
    "Obere Grenze (C.I. 95%)" = Mittelwert + Standardfehler)

return(df)
}

# Anwendung der Funktion auf jedes verfügbare Jahr ------------------------

df_2023 <- allbus_manipulation(allbus = allbus %>% mutate(Jahr = 2023))
df_2018 <- allbus_manipulation(allbus = allbus_kumulation %>%
                                 filter(year == 2018) %>%
                                 mutate(Jahr = 2018))
df_2010 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 2010) %>% 
                                 mutate(Jahr = 2010))
df_2006 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 2006) %>% 
                                 mutate(Jahr = 2006))
df_2000 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 2000) %>% 
                                 mutate(Jahr = 2000))
df_1998 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 1998) %>% 
                                 mutate(Jahr = 1998))
df_1994 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 1994) %>% 
                                 mutate(Jahr = 1994))
df_1992 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 1992) %>% 
                                 mutate(Jahr = 1992))
df_1991 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 1991) %>% 
                                 mutate(Jahr = 1991))

# Kombination der Dataframes ----------------------------------------------

df_zeittrend <- bind_rows(df_2023, df_2018, df_2010, df_2006,
                          df_2000, df_1998,
                          df_1994, df_1992, df_1991)


# Erstellung der Tabelle -------------------------------------

df_zeittrend %>%
  group_by(eastwest, Jahr) %>%
  summarise(Mittelwert,
            `Konfidenzintervall (95%)` = paste0("[", round(`Untere Grenze (C.I. 95%)`, 2), "; ", round(`Obere Grenze (C.I. 95%)`, 2), "]"),
            sd, n) %>% 
  gt() %>%
  tab_header(title = "Zeittrendanalyse: Bewertung der Wiedervereinigung aus ostdeutscher Sicht 
             (Ost-West-Vergleich)") %>% 
  fmt_number(columns = c(Mittelwert, sd),
             decimals = 2) %>% 
  cols_align(align = "center", 
             columns = "Konfidenzintervall (95%)") %>% 
  gt_theme_espn()


# Auflistung der verwendeten Packages -------------------------------------

citation("haven")
citation("tidyverse")
citation("gt")
citation("gtExtras")


