# Zeittrend-Analyse - Ostdeutschland --------------------------------------

# Laden relevanter Packages -----------------------------------------------

library(haven)     # Einlesen des ALLBUS
library(tidyverse) # Data-Cleaning, Pipes und Visualisierung


# Einlesen der ALLBUS-Datensätze ------------------------------------------------

allbus = read_dta("allbus2023.dta")
allbus_kumulation = read_dta("allbus_kumulation.dta")


# Datenmanipulation für das gestapelte Liniendiagramm ---------------------

allbus_manipulation <- function(allbus) {
  allbus %>%
    mutate(eastwest = case_when(
      eastwest == 1 ~ "Alte Bundesländer",
      eastwest == 2 ~ "Neue Bundesländer"),
      pr05 = case_when(
        pr05 == 1 ~ 4,
        pr05 == 2 ~ 3,
        pr05 == 3 ~ 2,
        pr05 == 4 ~ 1)) %>%
    filter(pr05 != -11, 
           eastwest == "Neue Bundesländer") %>%
    group_by(Jahr, eastwest, pr05) %>%
    summarise(n = n(), 
              .groups = 'drop') %>%
    group_by(Jahr, eastwest) %>%
    mutate(total = sum(n),    
           anteil = n / total * 100)
}


# Berechnung der Prozentanteile der Variable pr05 für jedes verfügbare Jahre

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


# Zusammenfassung der Datensätze in df_zeittrend

df_zeittrend <- bind_rows(df_2023, df_2018, df_2010, df_2006,
                          df_2000, df_1998,
                          df_1994, df_1992, df_1991)


# Erstellung des gestapelten Diagramms

df_zeittrend %>%
  ggplot(aes(x = Jahr, 
             y = anteil, 
             fill = factor(pr05, levels = c("1", "2", "3", "4")),
             group = pr05)) +
  geom_area(position = "stack",
            alpha = .9, 
            color = "grey20", 
            linewidth = 0.2) +
  labs(title = "Zeittrendanalyse: Bewertung der Wiedervereinigung aus ostdeutscher Sicht",
       subtitle = "'Die Wiedervereinigung hat den neuen Bundesländern mehr Vor- als Nachteile gebracht'",
       x = "Jahr",
       y = "Anteil",
       fill = "Bewertung") +
  scale_fill_manual(values = c("1" = "#e31a1c",  
                               "2" = "#fdbf6f",  
                               "3" = "#a6cee3",  
                               "4" = "#1f78b4"), 
                    labels = c("1 (Stimme überhaupt nicht zu)", 
                               "2 (stimme eher nicht zu)", 
                               "3 (stimme eher zu)", 
                               "4 (Stimme voll zu)")) +  
  scale_x_continuous(limits = c(1991, 2024),
                     breaks = seq(1991, 2023, by = 4),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(1991, 2023)) +
  theme_bw(base_family = "serif") +
  theme(plot.title = element_text(hjust = .5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = .5, size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.text.y = element_text(margin = margin(r = 5)),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.background = element_blank(),
        plot.margin = margin(t = 15, r = 50, b = 10, l = 5, unit = "pt"))



# Auflistung der verwendeten Packages -------------------------------------

citation("haven")
citation("tidyverse")
