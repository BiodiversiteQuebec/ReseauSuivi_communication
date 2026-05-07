# ================================================================================
# Chargement packages & data
# ================================================================================

#### Packages ####
# -------------- #
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(htmltools)
library(gdalcubes)
library(rstac)
library(terra)
library(stringr)
library(ENMeval)
library(rgbif)
library(geodata)
library(rmapshaper)
library(rnaturalearth)
library(RCurl) # for check if url exists - url.exists()
library(vegan)
library(adespatial)
library(rcoleo)
library(dplyr)
library(ggplot)
library(plotly)
library(bslib)

#### Local data ####
# ---------------- #
# recup des sites dans coleo avec les lat/lon
sites <- coleo_request_general("sites", response_as_df = TRUE, schema = "public")
coord_sites <- sites$geom$coordinates
coord <- lapply(coord_sites, function(x) {
    lon <- x[1]
    lat <- x[2]

    df <- data.frame(lon = lon, lat = lat)
    df
})
sites <- cbind(sites, do.call("rbind", coord))

# habitat data
# ------------
habitat <- c(
    "global",
    "forestier",
    "marais",
    "tourbière",
    "toundrique")

# taxon data
# ------------
taxon <- c(
    "acoustique_anoures",
    "acoustique_chiropteres_species",
    "vegetation_transect_totale",
    "insectes_sol_Araneae",
    "insectes_sol_Coleoptera",
    "acoustique_oiseaux",
    "acoustique_orthopteres")

# colors
colors <- c(
    "#2E483E", # "rgb(46,72,62)", # forestier
    #   "#3E8986","rgb(62,137,134)", # lac
    "#B05B22", # "rgb(176,91,34)", # marais
    "#EFB850", # "rgb(239,184,80)", # mil. hum. cotier
    #   "#81C8C5", "rgb(129,200,197)", # riviere
    "#58776E", # "rgb(88,119,110)", # toundrique
    "#D88219" # "rgb(216,130,25)" # tourbiere
)
colorsRGB <- c(
    rgb(46, 72, 62, 100, maxColorValue = 255), # forestier
    # "rgb(62,137,134)", # lac
    rgb(176, 91, 34, 100, maxColorValue = 255), # marais
    rgb(239, 184, 80, 100, maxColorValue = 255), # mil. hum. cotier
    #    "rgb(129,200,197)", # riviere
    rgb(88, 119, 110, 100, maxColorValue = 255), # toundrique
    rgb(216, 130, 25, 100, maxColorValue = 255) # tourbiere
)
cl_df <- data.frame(site_type = c("forestier", "marais", "milieu humide côtier", "toundrique", "tourbière"), col = colors, colRGB = colorsRGB)

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

# data - matrix
mat_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_matrices_inventaire_terrestre_maj28JANVIER2026.rds"))

mat_hab_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_matrices_inventaire_terrestre_per_habitat_maj28JANVIER2026.rds"))

# data - beta
beta_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_betadiv_inventaire_terrestre_maj28JANVIER2026.rds"))

beta_hab_ls <- readRDS(url("https://object-arbutus.cloud.computecanada.ca/bq-io/acer/reseau_suivi_data/xx_compo_communaute/COMPO_COMMU_betadiv_inventaire_terrestre_per_habitat_maj28JANVIER2026.rds"))


# data - lcbd

lcbd_dissi <- data.frame()
richdiff_taxo <- data.frame()
rempl_taxo <- data.frame()

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
lcbd_dissi <- left_join(lcbd_dissi, sites[, c("site_code", "lat", "lon")], by = join_by(site_code))
lcbd_dissi_sf <- st_as_sf(lcbd_dissi, coords = c("lon", "lat"), crs = st_crs(4326))


# ================================================================================
# server
# ================================================================================
server <- function(input, output, session) {
    #### Habitat selection depending on taxon choice
    output$habitat <- renderUI({
        n <- names(beta_hab_ls[[input$taxon_select]])
        selectInput("habitat_select",
            label = "",
            choices = c("global", sapply(strsplit(n, "-"), `[`, 2))
        )
    })

    # selection des données en fonction des filtres précédents
    # --- #
    remdiff <- reactive({
        if(input$habitat_select == "global"){
        data <- mat_ls[[input$taxon_select]]
        }else{
        data <- mat_hab_ls[[input$taxon_select]][[input$habitat_select]]
        }
        matm <- data$matrix
        remdiff <- beta.div.comp(matm, coef = "J", quant = FALSE)
        })
    # --- #
    lcbd <- reactive({
        lcbd_dissi_sf[lcbd_dissi_sf$inv == input$taxon_select,]
    })


    # généation du triplot
    output$triplot <- renderPlot({
       
        # data frame for triangular plot
        remdiff_3 <- cbind(
            (1 - remdiff()$D),
            remdiff()$repl,
            remdiff()$rich
        )
        colnames(remdiff_3) <- c("Similarité", "Rempl", "RichDiff")

        # triangle plot
        tplot <- ade4::triangle.plot(as.data.frame(remdiff_3[, c(3, 1, 2)]),
            show = FALSE,
            draw.line = FALSE,
            labeltriangle = FALSE,
            addmean = TRUE,
            cpoint = 0.25
        )
        points(tplot, col = "darkgrey", pch = 16, cex = 0.25)
        text(-0.45, 0.6, "RichDiff", cex = 1.5)
        text(0.4, 0.6, "Rempl", cex = 1.5)
        text(0, -0.6, "Similarité de Jaccard", cex = 1.5)
        })

        # génération du barplot
    output$barplot <- renderPlotly({

        # data pour le barplot
        repl_sites <- apply(remdiff()$repl, 1, mean)
        rich_sites <- apply(remdiff()$rich, 1, mean)
        dissi_sites <- apply(remdiff()$D, 1, mean)

        dis_df <- data.frame(
        site_code = rep(names(dissi_sites), 3),
        indice = c(
            rep("remplacement", length(names(dissi_sites))),
            rep("difference", length(names(dissi_sites))),
            rep("dissimilarity", length(names(dissi_sites)))
        ),
        value = c(
            round(repl_sites, digits = 2),
            round(rich_sites, digits = 2),
            round(dissi_sites, digits = 2)
        )
    )
    dis_df <- left_join(dis_df, sites[, c("site_code", "lat")], by = join_by("site_code"))

    dis_df <- dis_df |> arrange(lat)
    dis_df2 <- dis_df[dis_df$indice %in% c("remplacement", "difference"), ]

    dis_df3 <- dis_df2 |>
        mutate(value = ifelse(indice == "difference", -value, value))
    ## find the order
    temp_df <-
        dis_df3 %>%
        filter(indice == "remplacement") %>%
        arrange(lat, decrease = F)
    the_order <- temp_df$site_code
    dis_df3$site_code <- factor(dis_df3$site_code, the_order)
    p <-
        dis_df3 %>%
        ggplot(aes(
            x = site_code,
            y = value,
            group = indice,
            fill = indice,
            text = sprintf("code site: %s<br>Latitude: %s<br>%s: %s", site_code, lat, ifelse(indice == "remplacement", "Remplacement", "Différence"), abs(value))
        )) +
        geom_bar(stat = "identity", width = 0.75) +
        coord_flip() +
        scale_x_discrete(limits = the_order) +
        scale_fill_manual(values = c(
            "#a0a0a0", "#7a7a7a"
        )) +
        ylab("Dissimilarité") +
        ylim(-1, 1) +
        xlab("Sites par latitude croissante") +
        theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "white")
        )
        ggplotly(p, tooltip = c("text")) %>%
    plotly::layout(
        legend = list(
            x = 0.3,
            xanchor = "left",
            yanchor = "bottom",
            orientation = "h"
        ),
        yaxis = list(showticklabels = FALSE),
        xaxis = list(showticklabels = FALSE)
    )
        })

    # génération de la carte LCBDdissimilarité
    output$dissi_map <- renderPlot({
            ggplot() +
            geom_sf(
                data = qc3
            ) +
            geom_sf(
                data = lakes_qc3,
                fill = "white"
            ) +
            geom_sf(data = lcbd(), aes(
                size = LCBD,
                colour = "#2596be",
                fill = "#2596be",
                alpha = ifelse(p.adj <= 0.05, 1, 0.75)
            )) +
            labs(x = "Longitude", y = "Latitude") +
            theme(
                legend.position = "none",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                text = element_text(size = 20),
                panel.background = element_rect(fill = "transparent", color = "transparent"),
            )
    })

}

# ================================================================================
# UI
# ================================================================================
ui <- navbarPage(
    # tabPanel(
    # "SDMs",
    sidebarLayout(
        sidebarPanel(
            width = 2,
            h4("Groupe taxonomique"),
            selectInput("taxon_select",
                label = "",
                choices = taxon
            ),
            h4("Habitat"),
            uiOutput("habitat",
                label = ""
            ) # associated to renderUI in server section
            
        ),
        mainPanel(
            column(width = 4,
            fluidRow(

  card(full_screen = TRUE, card_header("Triplot"), plotOutput("triplot")),
  card(full_screen = TRUE, card_header("Barplot"), plotlyOutput("barplot"))

                # navset_card_underline(
                #     title = "Dissimilarité entre les sites",
                #     nav_panel("Triplot", plotOutput("triplot")),
                #     nav_panel("Barplot", plotlyOutput("barplot"))
                #     )  
    )),
            column(width = 8,
                    plotOutput("dissi_map")
            )
            )
)
)
# ================================================================================
# Lancer l'application
# ================================================================================

shinyApp(ui = ui, server = server)



