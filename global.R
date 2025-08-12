# global.R
library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(viridisLite)
library(ggplot2)
library(markdown)                     # makes it an explicit dependency
help_panel <- includeMarkdown("help.md")

# Turn off s2 (avoids strict polygon issues for admin boundaries)
sf::sf_use_s2(FALSE)

# ---------- helpers ----------
mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

first_non_empty <- function(x) {
  x <- as.character(x); x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x)) x[1] else NA_character_
}

fmt0 <- function(x) ifelse(is.na(x), "—", format(round(x, 0), trim = TRUE))
fmt2 <- function(x) ifelse(is.na(x), "—", format(round(x, 2), trim = TRUE))

show_metric_vec <- function(txt, mean_val) {
  txt_ok   <- !is.na(txt) & nzchar(trimws(txt))
  fallback <- ifelse(is.na(mean_val), "—", fmt2(mean_val))
  ifelse(txt_ok, txt, fallback)
}

pick_col <- function(df, candidates, fallback = NULL) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) df[[hit[1]]] else fallback
}

is_acute_vec <- function(df) {
  if ("acute" %in% names(df)) {
    v <- df$acute
    return((is.logical(v) & v) | grepl("^(1|y|yes|true|acute)$", as.character(v), ignore.case = TRUE))
  }
  for (col in c("type","facility_type","category","class","level","tags")) {
    if (col %in% names(df)) return(grepl("acute", df[[col]], ignore.case = TRUE))
  }
  rep(TRUE, nrow(df))
}

norm_postal <- function(x) {
  s <- gsub("\\D", "", paste0(x, collapse = ""))
  if (nchar(s) == 6) s else NA_character_
}

# ---------- Tab 1 data (planning areas + hospitals) ----------
basemap <- readRDS("./data/geo_pa_indices.rds") %>% st_make_valid()
if (is.na(st_crs(basemap))) basemap <- st_set_crs(basemap, 4326)
basemap <- st_transform(basemap, 4326)

num_cols <- intersect(c("SEDI","SAI","eshi_mean","cdi_mean","access_mean"), names(basemap))

basemap_pa <- basemap %>%
  mutate(across(all_of(num_cols), as.numeric)) %>%
  group_by(planning_area) %>%
  summarise(across(all_of(num_cols), mean_na), .groups = "drop") %>%
  left_join(
    basemap %>%
      st_drop_geometry() %>%
      select(planning_area, eshi, cdi, access) %>%
      group_by(planning_area) %>%
      summarise(
        eshi_txt   = first_non_empty(eshi),
        cdi_txt    = first_non_empty(cdi),
        access_txt = first_non_empty(access),
        .groups = "drop"
      ),
    by = "planning_area"
  )

labels <- sprintf(
  "<div style='font-size:12px; line-height:1.15'>
     <b>%s</b><br>
     SEDI: %s<br>
     SAI: %s<br>
     ESHI: %s<br>
     CDI: %s<br>
     ACCESS: %s
   </div>",
  basemap_pa$planning_area,
  fmt0(basemap_pa$SEDI),
  fmt0(basemap_pa$SAI),
  show_metric_vec(basemap_pa$eshi_txt,   basemap_pa$eshi_mean),
  show_metric_vec(basemap_pa$cdi_txt,    basemap_pa$cdi_mean),
  show_metric_vec(basemap_pa$access_txt, basemap_pa$access_mean)
)

metric_map <- c(
  "SEDI"   = "SEDI",
  "SAI"    = "SAI",
  "ESHI"   = "eshi_mean",
  "CDI"    = "cdi_mean",
  "ACCESS" = "access_mean"
)

hosp <- readRDS("./data/hospitals_sf.rds") %>% st_make_valid()
if (is.na(st_crs(hosp))) hosp <- st_set_crs(hosp, 4326)
hosp <- st_transform(hosp, 4326)
if (!all(st_geometry_type(hosp) %in% c("POINT","MULTIPOINT"))) hosp <- st_centroid(hosp)

hosp$acute_flag <- is_acute_vec(hosp)
hosp$name_disp  <- pick_col(hosp, c("name","hospital_name","Hospital.Name","facility","HOSPITAL"), "Hospital")
hosp$type_disp  <- pick_col(hosp, c("type","facility_type","category","class","level","tags"), NA_character_)
hosp$label_html <- sprintf(
  "<div style='font-size:12px'><b>%s</b>%s</div>",
  htmlEscape(hosp$name_disp),
  ifelse(is.na(hosp$type_disp) | hosp$type_disp == "", "", paste0("<br>", htmlEscape(as.character(hosp$type_disp))))
)
hosp_acute <- hosp[hosp$acute_flag %in% TRUE, , drop = FALSE]

icon_path <- "www/hospital_icon.png"
has_icon  <- file.exists(icon_path)
hosp_icon <- if (has_icon) {
  makeIcon(iconUrl = "hospital_icon.png", iconWidth = 28, iconHeight = 28, iconAnchorX = 14, iconAnchorY = 28)
} else NULL

# ---------- Tab 2 data (subzones + postal + eshi) ----------
subzones <- readRDS("./data/geo_subzones_clean.rds")
if (is.na(sf::st_crs(subzones))) subzones <- sf::st_set_crs(subzones, 4326)
subzones <- sf::st_transform(subzones, 4326)

# sf-only cleaning (no lwgeom / rmapshaper)
subzones <- subzones %>%
  sf::st_set_precision(1e6) %>%                 # ~1 m precision to snap tiny slivers
  sf::st_make_valid() %>%                        # fix invalid rings
  sf::st_collection_extract("POLYGON", warn = FALSE) %>% 
  sf::st_cast("MULTIPOLYGON", warn = FALSE)
subzones <- subzones[!sf::st_is_empty(subzones), ]

# Postal points
sg_postal <- readRDS("./data/sg_postal.rds") %>% sf::st_make_valid()
if (is.na(sf::st_crs(sg_postal))) sg_postal <- sf::st_set_crs(sg_postal, 4326)
sg_postal <- sf::st_transform(sg_postal, 4326)

# ESHI points
eshi_sf <- readRDS("./data/eshi.rds") %>% sf::st_make_valid()
if (is.na(sf::st_crs(eshi_sf))) eshi_sf <- sf::st_set_crs(eshi_sf, 4326)
eshi_sf <- sf::st_transform(eshi_sf, 4326)

# load modules
source("R/mod_map.R", local = TRUE)
source("R/mod_postal.R", local = TRUE)
