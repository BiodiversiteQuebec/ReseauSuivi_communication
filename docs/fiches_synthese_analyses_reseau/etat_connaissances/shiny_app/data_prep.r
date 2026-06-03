# Packages
library(fuzzyjoin)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(rcoleo)
library(sf)
library(rmapshaper)
library(vegan)
library(adespatial)
library(shinyalert)

# Data
#### Local data ####
# ---------------- #
sites_est <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/GITHUB/ReseauSuivi_Indicateurs/Summer_blitz_2025/7_rarefaction/inext_estimates_per_sites.rds")

# taxon data
# ------------
taxon <- unique(sites_est$type_campaign)

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

cl_df <- data.frame(site_type = c("forestier", "marais", "milieu humide côtier", "toundrique", "tourbière"), col = colors, col_pale = adjustcolor(colors, alpha.f = 0.5))
cl_df <- rbind(cl_df, data.frame(site_type = "global", col = "#331bee", col_pale = adjustcolor("#331bee", alpha.f = 0.5)))

# Several Polygons for Qc
# -----------------------
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

qc3 <- st_transform(qc, crs = st_crs(4326)) |> ms_simplify(0.05)
lakes_qc3 <- st_transform(lakes_qc2, crs = st_crs(4326)) |> ms_simplify(0.05)
