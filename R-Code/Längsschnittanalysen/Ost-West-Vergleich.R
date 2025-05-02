# Zeittrendanalyse - Ost-West-Vergleich  --------------------------------

# Laden relevanter Packages -----------------------------------------------

library(haven)     # Einlesen des ALLBUS
library(tidyverse) # Data-Cleaning, Pipes und Visualisierung

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
    summarise(mean_wdrvrg = mean(pr05, na.rm = T),
              n = n(),
              sd_wdrvrg = sd(pr05, na.rm = T),
              se_wdrvrg = sd_wdrvrg / sqrt(n),
              ci_grenze_unten = mean_wdrvrg - qt(0.975, df = n - 1) * se_wdrvrg,
              ci_grenze_oben = mean_wdrvrg + qt(0.975, df = n - 1) * se_wdrvrg,
              .groups = 'drop')
  
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


# Erstellung der Visualisierung -------------------------------------------

df_zeittrend %>%
  ggplot(aes(x = Jahr, 
             y = mean_wdrvrg, 
             color = eastwest, 
             group = eastwest)) +
  geom_line(linewidth = 1.1) +  
  geom_point(size = 3) +  
  geom_errorbar(aes(ymin = ci_grenze_unten, 
                    ymax = ci_grenze_oben), 
                width = 0.3, 
                linewidth = 0.7,
                alpha = 0.7) +
  labs(title = "Durchschnittliche Bewertung der Wiedervereinigung aus ostdeutscher Perspektive: \nWest-Ost-Vergleich im Zeitverlauf",
       subtitle = "'Die Wiedervereinigung hat den neuen Bundesländern mehr Vor- als Nachteile gebracht'",
       x = "Jahr",
       y = "",
       color = "Landesteil:") +
  scale_y_continuous(limits = c(1, 4), 
                     breaks = 1:4,
                     labels = c("1: (Stimme\nüberhaupt\nnicht zu)", 
                                "2: (Stimme\neher nicht\nzu)", 
                                "3: (Stimme\neher zu)", 
                                "4: (Stimme\nvoll zu)"),
                     expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(1991, 2024), 
    breaks = seq(1991, 2023, by = 4)) +
  scale_color_manual(values = c("Alte Bundesländer" = "steelblue", 
                                "Neue Bundesländer" = "#e31a1c"),
                     labels = c("Alte Bundesländer", 
                                "Neue Bundesländer")) +
  theme_minimal(base_family = "serif") +
  geom_text(aes(label = paste0("µ =", round(mean_wdrvrg, 2))), 
            vjust = ifelse(df_zeittrend$Jahr == 1992, -2.3, -1.4), 
            size = 3.5, 
            show.legend = F, 
            color = "black") + 
  theme(plot.title = element_text(hjust = .5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = .5, size = 14),
        axis.text = element_text(face = "bold", size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(t = 15, r = 20, b = 10, l = 10, unit = "pt"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14, face = "bold"), 
        legend.box = "horizontal",
        legend.box.background = element_rect(color = "grey40", linewidth = 0.4))


# Auflistung der verwendeten Packages --------------------------------------

citation("haven")
citation("tidyverse")









