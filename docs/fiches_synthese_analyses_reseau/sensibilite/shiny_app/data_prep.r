# Packages
library(fuzzyjoin)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(tidyverse)
library(ggplot2)
library(plotly)
library(sf)
library(adespatial)
library(rmapshaper)
library(DT)
library(geodata)
library(rnaturalearth)

# raw data
sites <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/sites_data_18JUIN2026.rds"))

qc <- geodata::gadm("CAN", level = 1, path = getwd()) |>
    st_as_sf() |>
    st_transform(32618) |>
    filter(NAME_1 == "Québec")

lakes <- rnaturalearth::ne_download(
    scale = "medium",
    type = "lakes",
    destdir = getwd(),
    category = "physical",
    returnclass = "sf"
) |>
    st_transform(32618)

lakes_qc <- st_filter(lakes, qc)
lakes_qc2 <- st_intersection(lakes_qc, qc)

qc2 <- st_transform(qc, crs = st_crs(4326)) |> ms_simplify(0.05)
lakes_qc3 <- st_transform(lakes_qc2, crs = st_crs(4326)) |> ms_simplify(0.05)

# colors
colors <- c(
    "#2E483E", # "rgb(46,72,62)", # forestier
    # "#3E8986","rgb(62,137,134)", # lac
    "#B05B22", # "rgb(176,91,34)", # marais
    "#EFB850", # "rgb(239,184,80)", # mil. hum. cotier
    # "#81C8C5", "rgb(129,200,197)", # riviere
    "#58776E", # "rgb(88,119,110)", # toundrique
    "#D88219" # "rgb(216,130,25)" # tourbiere
)

cl_df <- data.frame(
    site_type = c("forestier", "marais", "milieu humide côtier", "toundrique", "tourbière"),
    col = colors,
    col_pale = adjustcolor(colors, alpha.f = 0.5)
)
cl_df <- rbind(
    cl_df,
    data.frame(site_type = "global", col = "#331bee", col_pale = adjustcolor("#331bee", alpha.f = 0.5))
)

# analyzed data
var_expl <- data.frame(
    sign = c("effets_spatiaux", "effets_environnementaux_locaux", "effets_anthropiques", "effets_climatiques"),
    short_sign = c("spat", "envi", "anthro", "clim"),
    long_sign = c("Effets spatiaux", "Effets environnementaux locaux", "Effets anthropiques", "Effects climatiques")
)

beta_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_betadiv_inventaire_terrestre_maj6MARS2026.rds"))
beta_hab_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_betadiv_inventaire_terrestre_per_habitat_maj6MARS2026.rds"))

# varpart analysis
varpart_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/LCBD_part_var_complete_results_maj28JANVIER2026.rds"))[-c(19)]
varpart_hab_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/part_var_complete_results_per_habitat_maj_28AVRIL2026.rds"))

taxon <- names(varpart_ls)
