library(httr)
library(sf)
library(dplyr)

get_X_Y_coordinates <- function(x) {
  
  sftype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  
  if(sftype == "POINT") {
    
    xy <- as.data.frame(sf::st_coordinates(x))
    dplyr::bind_cols(x, xy)
    
  } else {
    x
  }
  
}

sf_fisbroker <- function(url) {
  
  typenames <- basename(url)
  
  url <- httr::parse_url(url)
  
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetFeature",
                    srsName = "EPSG:25833",
                    TYPENAMES = typenames)
  
  request <- httr::build_url(url)
  
  print(request)
  
  out <- sf::read_sf(request)
  
  out <- sf::st_transform(out, 4326)
  
  out <- get_X_Y_coordinates(out)
  
  return(out)
}

export_format <- c(
  "geojson", 
  "sqlite"
)

sf_save <- function(z, fname) {
  
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), export_format, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
  saveRDS(z, paste0(file.path(fname, fname), ".rds"))
  
}



url <- "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan"

typenames <- basename(url)

url <- httr::parse_url(url)

url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetFeature",
                  srsName = "EPSG:25833",
                  TYPENAMES = typenames)


GET(request) -> test
request <- httr::build_url(url)



https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plan?service=wfs&version=2.0.0&request=GetFeature&srsName=EPSG%3A25833&TYPENAMES=s_lor_plan
