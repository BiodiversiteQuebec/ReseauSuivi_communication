library(rcoleo)

sites <- coleo_request_general("sites", output_geometry = TRUE, schema = "public")

sites_coord <- sites |>
    st_coordinates(sites) |>
    as.data.frame() |>
    rename(lat = Y, lon = X)
# |>
# cbind(sites)

sites2 <- cbind(sites, sites_coord)

# saveRDS(sites2, "/home/local/USHERBROOKE/juhc3201/BdQc/ReseauSuivi/GITHUB/ReseauSuivi_communication/docs/fiches_synthese_analyses_reseau/sites_data_18JUIN2026.rds")
