library(flexdashboard)
library(shiny)
library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(highcharter)
library(readxl)
library(geojsonsf)

source("utils.R")

df <- read.csv2("data\\EWRMIGRA201912H_Matrix.csv")
df2 <- read.csv2("data\\EWR201912E_Matrix.csv")

df <- df %>% inner_join(df2[, c('RAUMID', 'E_E')])


sf_use_s2(TRUE)
geo_df <- read_sf("data\\shp\\lor_planungsraeume.shp") %>% 
  st_transform(crs = st_crs("EPSG:4326"))


add_zero <- function(x) {
  if (nchar(x) < 8) 
    paste0("0", x)
  else 
    x
}

df$RAUMID <- map_chr(df$RAUMID, add_zero)


df %>% 
  inner_join(geo_df, by = c('RAUMID' = 'broker Dow')) %>% 
  st_as_sf() -> df

centers <- tribble(
  ~center, ~lat, ~long,
  "Alexanderplatz", 52.52211, 13.41334,
  "Friedrichstraße", 52.51987, 13.38844,
  "Potsdamer Platz", 52.50975, 13.37593,
  "Müllerstraße", 52.55061, 13.35179,
  "Frankfurter Allee", 52.51392, 13.47494,
  "Hermannplatz", 52.48716, 13.42488,
  "Pankow", 52.56702, 13.41025,
  "Zoo", 52.50537, 13.33434,
  "Wilmersdorferstr", 52.50553, 13.30710,
  "Altstadt-Spandau", 52.53729, 13.20483,
  "Schloßstr", 52.45870, 13.32196,
  "Bahnhofstr Köpenick", 52.45804, 13.57856,
  "Gorkistr", 52.58549, 13.28741
) %>% 
  st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(df))


ggplot() +
  geom_sf(data = df) + 
  geom_sf(data = st_centroid(df$geometry), col = 'black', size = 0.5) +
  geom_sf(data = centers, col = "red")


calc_clos_center <- function(poly) {
  
  poly_centroid <- st_centroid(poly) %>% 
    st_distance(centers) %>% 
    min() %>% 
    as.numeric() -> x
  
  return(x)
  
} 

df$dist <- map_dbl(seq_along(df$geometry), 
        ~calc_clos_center(df$geometry[.]))




cols <- c('HK_Polen', 'HK_EheJug', 
          'HK_EheSU', 'HK_Turk', 
          'HK_Arab', 'HK_Sonst', 
          'HK_NZOrd', 'HK_EU15',
          'HK_EU28', 'no_mh')

df <- df %>% 
  rowwise() %>% 
  mutate(no_mh = E_E - MH_E)

df$diversity <- df %>% 
  select(all_of(c(cols))) %>% 
  map_dfc(~./df$E_E) %>% 
  select(-geometry) %>% 
  map_dfc(~.*log10(1/.)) %>% 
  rowwise() %>% 
  mutate(diversity = sum(across(everything()), na.rm=T)/ log10(10)) %>% 
  pull(diversity)


saveRDS(df, "data\\final_data.RDS")

