# Packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)
library(rcoleo)
library(sf)
library(rmapshaper)
library(adespatial)
library(shinyalert)
library(geodata)
library(rnaturalearth)
# Data
#### Local data ####
# ---------------- #
# recup des sites dans coleo avec les lat/lon
sites <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/sites_data_18JUIN2026.rds"))

# habitat data
# ------------
habitat <- c(
    "global",
    "forestier",
    "marais",
    "tourbière",
    "toundrique"
)

# taxon data
# ------------
taxon <- c(
    "acoustique_anoures",
    "acoustique_chiropteres_species",
    "vegetation_transect_totale",
    "insectes_sol_Araneae",
    "insectes_sol_Coleoptera",
    "acoustique_oiseaux",
    "acoustique_orthopteres"
)

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

#### Production de carte de RS (diversité alpha) par site par groupe taxonomique ####
# ----- #
dat_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_occ_inventaire_terrestre_maj28JANVIER2026.rds"))
data <- dat_ls[names(dat_ls) %in% c(
    "acoustique_anoures",
    "acoustique_oiseaux",
    "acoustique_orthopteres",
    "insectes_sol_Araneae",
    "insectes_sol_Coleoptera",
    "acoustique_chiropteres_species",
    "vegetation_transect_totale"
)]
alpha_ls <- lapply(data, function(x) {
    x |>
        group_by(site_code) |>
        summarise(n_sp = length(unique(valid_scientific_name2)), habitat = unique(site_type))
})

# data - matrix
mat_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_matrices_inventaire_terrestre_maj28JANVIER2026.rds"))

mat_hab_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_matrices_inventaire_terrestre_per_habitat_maj28JANVIER2026.rds"))

# data - beta
beta_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_betadiv_inventaire_terrestre_maj28JANVIER2026.rds"))

beta_hab_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_betadiv_inventaire_terrestre_per_habitat_maj28JANVIER2026.rds"))
# à utiliser pour calculer LCBD dissi par habitat ****

# data - lcbd - global
dissi_res_ls <- list()
for (i in 1:length(taxon)) {
    inv <- taxon[i]
    mat <- mat_ls[[inv]]
    matm <- mat$matrix

    bt <- beta.div.comp(matm, coef = "J", quant = FALSE) # utilisation de l'indice de Jaccard (coef = "J") car donnees de pres/abs
    bt$inventaire <- inv
    dissi_res_ls[[i]] <- bt
}
names(dissi_res_ls) <- taxon

# ----- #
library(fuzzyjoin)

lcbd_dissi <- data.frame()

for (i in 1:length(taxon)) {
    inv <- taxon[i]
    print(inv)

    beta <- beta_ls[[inv]][["beta_jaccard"]]
    beta_df <- data.frame(
        inv = inv,
        type = "LCBD_dissimilarite",
        site_code = names(beta$LCBD),
        LCBD = beta$LCBD,
        p.LCBD = beta$p.LCBD,
        p.adj = beta$p.adj
    )
    lcbd_dissi <- rbind(lcbd_dissi, beta_df)
}

lcbd_dissi <- lcbd_dissi |> stringdist_left_join(sites[, c("site_code", "lat", "lon")], by = c("site_code" = "site_code"), max_dist = 1)
# lcbd_dissi <- left_join(lcbd_dissi, sites[, c("site_code", "lat", "lon")], by = join_by(site_code))
names(lcbd_dissi)[3] <- "site_code"
lcbd_dissi_sf <- st_as_sf(lcbd_dissi, coords = c("lon", "lat"), crs = st_crs(4326))
