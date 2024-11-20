# Zeittrendanalyse - Durchschnittliche Zufriedenheit mit Wiedervereinigung --------

# Setzen des Working Directories ------------------------------------------
setwd("C:/Users/Noah/Desktop/Bachelorarbeit")


# Laden relevanter Packages -----------------------------------------------
library(haven) # Einlesen des ALLBUS
library(tidyverse) # Data-Cleaning, Pipes und Visualisierung
library(sf) # Erstellung von Maps
library(rnaturalearth) # Laden der Map-Daten
library(rnaturalearthdata) # Daten für Maps
library(ggspatial) # coord_sf-Funktion zur Richtung der Karte
library(patchwork) # Zusammenfügen mehrerer ggplot-Objekte

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
  mutate(mean_wdrvrg = mean(pr05, na.rm = TRUE)) %>% 
  select(-pr05) %>% 
  unique()

return(df)
}
    


# Anwendung der allbus_manipulation-Funktion auf die Datensätze zu den
# verschiedenen Jahren -----------

df_2023 = allbus_manipulation(allbus = allbus) %>% 
  mutate(Jahr = 2023)
df_2018 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 2018))%>% 
                                mutate(Jahr = 2018)
df_2010 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 2010)) %>% 
                                mutate(Jahr = 2010)
df_2006 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 2006))%>% 
                                mutate(Jahr = 2006)
df_1991 = allbus_manipulation(allbus = allbus_kumulation %>% 
                                filter(year == 1991))%>% 
                                mutate(Jahr = 1991)



# Zusammenfassung aller Datensätze zu einem Datensatz: df_zeittrend ------------------------------------------------------------

df_zeittrend = bind_rows(df_2023, df_2018, df_2010, df_2006, df_1991)


# Akquise der Geo-Daten der Bundesländer  ---------------------------------------------

df_bundesl <- ne_states(country = "Germany", returnclass = "sf") %>% 
  rename(Bundesland = name)


# Mergen beider Datensätze ------------------------------------------------

map_df = left_join(df_zeittrend, df_bundesl, by = "Bundesland")


# Funktion zur Erstellung der Map ------------------------------------------------------

map_erstellen <- function(year) {
  map_df %>%
    filter(Jahr == year) %>%
    mutate(coords = st_coordinates(st_centroid(geometry)),
           x = coords[, 1],
           y = coords[, 2]) %>%
    ggplot(aes(geometry = geometry)) +
    geom_sf(aes(fill = mean_wdrvrg), color = "black", lwd = 0.1) +
    geom_text(aes(x = case_when(
      Bundesland == "Brandenburg" ~ x + 0.65,  
      Bundesland == "Saarland" ~ x + 0.3,     
      Bundesland == "Niedersachsen" ~ x + 0.65,  
      Bundesland == "Rheinland-Pfalz" ~ x - 0.2,
      TRUE ~ x),
      y = case_when(
        Bundesland == "Brandenburg" ~ y -0.6,  
        Bundesland == "Niedersachsen" ~ y - 0.3,  
        Bundesland == "Rheinland-Pfalz" ~ y + 0.2,
        TRUE ~ y),
      label = round(mean_wdrvrg, 2)),
      size = 3,   
      color = "white") +
    theme_void(base_family = "serif") +
    coord_sf(crs = 4326) +
    labs(title = as.character(year)) +
    theme(plot.title = element_text(hjust = .5, face = "bold"),
          legend.margin = margin(t = 0, r = 100, b = 0, l = 0),
          legend.text = element_text(size = 12),        
          legend.title = element_text(size = 14)) +     
    scale_fill_viridis_c(
      option = "F",
      direction = -1,
      limits = c(1,4),
      breaks = seq(1, 4, by = 1),  
      name = "Durchschnittliche Bewertung der\nWiedervereinigung",
      labels = c("1 (negativ)", "2", "3", "4 (positiv)"),
      guide = guide_colorbar(direction = "vertical",
                             barwidth = 1.5,
                             barheight = 8,
                             title.hjust = 0.5,
                             title.vjust = 4))
}



# Erstellung der Maps für die jeweiligen Jahre ----------------------------

map_1991 <- map_erstellen(1991)
map_2006 <- map_erstellen(2006)
map_2010 <- map_erstellen(2010)
map_2018 <- map_erstellen(2018)
map_2023 <- map_erstellen(2023)



# Kombination der Maps zu einer Darstellung -------------------------------

(map_1991 + map_2006 + map_2010) /
  (map_2018 + map_2023) +
  plot_annotation(title = "Entwicklung der Bewertung der Wiedervereinigung aus ostdeutscher Perspektive \nin den Bundesländern (1991-2023)",
                  theme = theme(plot.title = element_text(hjust = .5, 
                                                          size = 17, 
                                                          face = "bold",
                                                          family = "serif")))+
  plot_layout(guides = "collect")



# Auflistung der verwendeten Packages -------------------------------------

citation("haven") 
citation("tidyverse")
citation("sf") 
citation("rnaturalearth") 
citation("rnaturalearthdata") 
citation("ggspatial") 
citation("patchwork") 




