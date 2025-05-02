# Mittelwertvergleich: Ost-West-Berlin (Zeittrend) --------

# Laden relevanter Packages -----------------------------------------------

library(haven)     # Einlesen des ALLBUS
library(tidyverse) # Data-Cleaning, Pipes und Visualisierung
library(scales)    # Formatierung der p-Werte

# Einlesen der Datensätze ------------------------------------------------

allbus = read_dta("allbus2023.dta")
allbus_kumulation = read_dta("allbus_kumulation.dta")

# Datenmanipulation -------------------------------------------------------
# Funktion zur Berechnung der Mittelwerte (inkl. Konfidenzintervalle + p-Werte)
# pro Jahr und Landesteil

 
allbus_manipulation <- function(allbus) {
  df <- allbus %>%
    filter(dg10 %in% c(3,12)) %>%
    mutate(eastwest_berlin = case_when(
        dg10 == 3 ~ "West-Berlin",
        dg10 == 12 ~ "Ost-Berlin"),
      pr05 = case_when(
        pr05 == 1 ~ 4,
        pr05 == 2 ~ 3,
        pr05 == 3 ~ 2,
        pr05 == 4 ~ 1)) %>%
    filter(pr05 != -11) %>%
    group_by(Jahr, eastwest_berlin) %>%
    summarise(n = n(),
              mean_wdrvrg = mean(pr05, na.rm = T),
              sd_wdrvrg = sd(pr05, na.rm = T),
              se_wdrvrg = sd_wdrvrg / sqrt(n),
              ci_grenze_unten = mean_wdrvrg - qt(0.975, df = n - 1) * se_wdrvrg,
              ci_grenze_oben = mean_wdrvrg + qt(0.975, df = n - 1) * se_wdrvrg,
              .groups = 'drop')
  
# Berechnung der t-Tests zwischen den Gruppen (Ost- und West-Berlin) für 
# jedes Jahr
  
  t_tests <- allbus %>%
    filter(dg10 %in% c(3,12)) %>% 
    mutate(eastwest_berlin = case_when(
        dg10 == 3 ~ "West-Berlin",
        dg10 == 12 ~ "Ost-Berlin"),
        pr05 = case_when(pr05 == 1 ~ 4,
                         pr05 == 2 ~ 3,
                         pr05 == 3 ~ 2,
                         pr05 == 4 ~ 1)) %>%
    filter(pr05 != -11) %>%
    group_by(Jahr) %>%
    summarise(p_wert = t.test(pr05 ~ eastwest_berlin)$p.value,
      .groups = 'drop')
  
# Zusammenfügen in einen Dataframe
  df <- df %>%
    left_join(t_tests, by = "Jahr") %>%
    mutate(signifikanz = case_when(
        p_wert < 0.01 ~ "***",
        p_wert < 0.05 ~ "**",
        p_wert < 0.01 ~ "*",
        TRUE ~ "n.s."))
  
  return(df)
}


# Anwendung der Funktion für jedes Jahr, in dem Items pr05 und dg10 abgefragt wurden------

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
df_1991 <- allbus_manipulation(allbus = allbus_kumulation %>% 
                                 filter(year == 1991) %>% 
                                 mutate(Jahr = 1991))

# Zusammenfügen der Dataframes zu df_zeittrend ---------------------

df_zeittrend <- bind_rows(df_2023, df_2018, df_2010, df_2006, df_1991)

# Erstellung der Visualisierung -------------------------------------------

df_zeittrend %>% 
  ggplot(aes(x = eastwest_berlin,
             y = mean_wdrvrg, 
             color = eastwest_berlin, 
             group = eastwest_berlin)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_grenze_unten, ymax = ci_grenze_oben), 
                width = 0.2, 
                linewidth = 0.8,
                show.legend = F) +
  labs(title = "Durchschnittliche Bewertung der Wiedervereinigung aus ostdeutscher Perspektive: \nOst/West-Berlin-Vergleich im Zeitverlauf",
       x = "",
       y = "Durchschnittliche Bewertung (1: negativ, 4: positiv)",
       color = "Teil Berlins:") +
  scale_y_continuous(limits = c(1,4),
                     expand = c(0,0))+
  scale_color_manual(values = c("Ost-Berlin" = "steelblue", "West-Berlin" = "red"))+
  theme_bw(base_family = "serif") +
  geom_text(aes(label = paste("n =", n)), 
            vjust = 4.2, 
            size = 4,
            show.legend = F)+
  geom_text(aes(label = paste0("µ = ", round(mean_wdrvrg, 2))), 
            hjust = -0.35, 
            size = 4, 
            show.legend = F)+
  geom_text(aes(x = 1.5, y = 3.6, 
                label = paste0("p = ", scientific(p_wert, digits = 3), " (", signifikanz, ")")), 
            size = 4,
            color = "black") +
  theme(plot.title = element_text(hjust = .5,
                                  size = 17),
        plot.subtitle = element_text(hjust = .5,
                                     size = 14),
        axis.text = element_text(face = "bold",
                                 size = 12),
        axis.title = element_text(size = 14),
        plot.margin = margin(t = 15, r = 20, b = 10, l = 10, unit = "pt"),
        legend.position = "bottom",
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 13),  
        legend.title = element_text(size = 14), 
        legend.box = "box",
        legend.box.background = element_rect(colour = "black", linewidth = 0.5),
        legend.background = element_blank())+
  facet_wrap(~ Jahr)

# Auflistung der verwendeten Packages -------------------------------------

citation("haven")
citation("tidyverse")
citation("scales")
