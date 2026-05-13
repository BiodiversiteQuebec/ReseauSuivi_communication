# Packages
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
# Data
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

lcbd_dissi <- left_join(lcbd_dissi, sites[, c("site_code", "lat", "lon")], by = join_by(site_code))
lcbd_dissi_sf <- st_as_sf(lcbd_dissi, coords = c("lon", "lat"), crs = st_crs(4326))

# ---------- #
# UI ----
# ----------#
ui <- navbarPage(
    shinyWidgets::useShinydashboard(),
    title = "My App",
    tabPanel(
        "Dissimilarité des sites",
        icon = icon("home"),
        fluidPage(
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
                    ,
                    # create extra vertical space in sidebar (for illustration only)
                    HTML(rep("<br>", 30))
                ),
                mainPanel(
                    width = 10,
                    # 1st fluid row for value boxes
                    fluidRow(
                        # box 1
                        box(
                            title = div(
                                tags$img(src = "https://avatars.githubusercontent.com/u/66145652?s=200&v=4", height = "30px", style = "margin-right: 10px;"),
                                tags$b("Dissimilarité et groupes taxonomiques")
                            ),
                            width = 4,
                            "Le niveau de dissimilarité entre les sites est variable en fonction des groupes taxonomiques."
                        ),
                        # box 2
                        box(
                            title = div(
                                tags$img(src = "docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png", height = "30px", style = "margin-right: 10px;"),
                                tags$b("Mécanismes opérants")
                            ),
                            width = 4,
                            "La force des mécanismes soujacents à la dissimilarité des sites est différente selon les groupes taxonomiques."
                        ),
                    ),
                    # box 3
                    box(
                        title = div(
                            tags$img(src = "Logo_bq_sans_texte.png", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Sites originaux")
                        ),
                        width = 4,
                        "Certains sites semblent présenter une contribution particulière quant à la différence en richesse spécifique pour les chiroptères, orthoptères et les insectes su sol."

                        # Value Box 1
                        # valueBoxOutput(outputId = "message_1", width = 4),

                        # Value Box 2
                        # valueBoxOutput(outputId = "message_2", width = 4),

                        # Value Box 3
                        # valueBoxOutput(outputId = "message_3", width = 4)
                    ),
                    br(),
                    hr(),

                    # 2nd fluid row for map and plots
                    fluidRow(

                        # 1st column for plots
                        column(
                            5,
                            # fluidRow for sales trend
                            fluidRow(
                                style = "border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px; height: 500px",
                                br(),
                                # sales trend title and info button
                                div(HTML("<b>Dissimilarité - Barplot</b> "), style = "display: inline-block;"),
                                uiOutput("barplot_button", style = "display: inline-block;"),
                                br(), br(),
                                # trend plot
                                plotlyOutput("barplot", height = "400px")
                            ),
                            br(),
                            # fluidRow for bar plot
                            fluidRow(
                                style = "border: 1px solid lightgrey; border-radius: 25px; margin-left: 10px; padding-left: 10px; height: 500px",
                                br(),
                                # bar plot title and info button
                                div(HTML("<b>Triplot</b> "), style = "display: inline-block;"),
                                uiOutput("triplot_button", style = "display: inline-block;"),
                                br(), br(),
                                # bar plot
                                plotlyOutput("triplot", height = "400px")
                            )
                        ),
                        # 2nd column for map
                        column(7,
                            style = "border: 1px solid lightgrey; border-radius: 25px; height: 900px",
                            br(),
                            # ntitle and info button
                            div(HTML("<b>Carte de contribution locale</b> "), style = "display: inline-block;"),
                            uiOutput("map_button", style = "display: inline-block;"),
                            br(), br(),
                            # map plot
                            plotlyOutput("dissi_map", height = "700px"),
                            br(), br(), br()
                        ),
                    )
                )
            )
        )
    )
)

# Server ----
server <- function(input, output) {
    #### Habitat selection depending on taxon choice
    output$habitat <- renderUI({
        n <- names(beta_hab_ls[[input$taxon_select]])
        selectInput("habitat_select",
            label = "",
            choices = c("global", sapply(strsplit(n, "-"), `[`, 2))
        )
    })

    # Box zone #
    # ------ #
    # first box content
    # output$my_image <- renderUI({
    #     tags$img(src = "https://avatars.githubusercontent.com/u/66145652?s=200&v=4", height = "50px")
    # })

    # output$my_text <- renderUI({
    #     HTML(<img src="https://avatars.githubusercontent.com/u/66145652?s=200&v=4" > "Le niveau de dissimilarité entre les sites est variable en fonction des groupes taxonomiques.")
    # })

    # second box content
    # output$my_image2 <- renderUI({
    #     tags$img(src = "https://www.r-project.org/logo/Rlogo.png", width = "250px")
    # })

    # output$my_text2 <- renderUI({
    #     HTML("La force des mécanismes soujacents à la dissimilarité des sites est différente selon les groupes taxonomiques.")
    # })

    # third box content
    # output$my_image3 <- renderUI({
    #     tags$img(src = "https://www.r-project.org/logo/Rlogo.png", width = "250px")
    # })

    # output$my_text3 <- renderUI({
    #     HTML("Certains sites semblent présenter une contribution particulière quant à la différence en richesse spécifique pour les chiroptères, orthoptères et les insectes su sol.")
    # })

    # Box 1
    # output$message_1 <- shinydashboard::renderValueBox({
    #     tags$style(".small-box.bg-yellow { background-color: #AABBCC !important; }")
    #     valueBox(5, "Message 1", color = "yellow")
    # })

    # Box 2
    # output$message_2 <- renderValueBox({
    #     valueBox(10, "Message 2", color = "#e0b658")
    # })

    # Box 3
    # output$message_3 <- renderValueBox({
    #     valueBox(15, "Message 3", color = "#e0b658")
    # })

    # Interactive data zone #
    # --------------------- #
    # selection des données en fonction des filtres précédents
    # --- #
    remdiff <- reactive({
        if (input$habitat_select == "global") {
            data <- mat_ls[[input$taxon_select]]
        } else {
            data <- mat_hab_ls[[input$taxon_select]][[input$habitat_select]]
        }
        matm <- data$matrix
        remdiff <- beta.div.comp(matm, coef = "J", quant = FALSE)
    })

    # Plot zone #
    # --------- #

    # 1 - généation du triplot
    output$triplot <- renderPlotly({
        # data frame for triangular plot
        remdiff_3 <- cbind(
            (1 - remdiff()$D),
            remdiff()$repl,
            remdiff()$rich
        )
        colnames(remdiff_3) <- c("Similarité", "Rempl", "RichDiff")

        fig <- as.data.frame(remdiff_3) %>% plot_ly()
        fig <- fig %>% add_trace(
            type = "scatterternary",
            mode = "markers",
            a = ~Similarité,
            b = ~Rempl,
            c = ~RichDiff,
            text = ~ paste("Similarité:", round(Similarité, 2) * 100, "%", "<br>Différence en RS:", round(RichDiff, 2) * 100, "%", "<br>Remplacement", round(Rempl, 2) * 100, "%"),
            hoverinfo = "text",
            marker = list(
                symbol = 200,
                color = cl_df$col[cl_df$site_type == input$habitat_select],
                opacity = 0.2,
                size = 10,
                line = list(
                    "width" = 1,
                    color = "black"
                )
            )
        )
        fig <- fig %>% layout(
            ternary = list(
                sum = 100,
                aaxis = list(title = "Similarité"),
                baxis = list(title = "Remplacement"),
                caxis = list(title = "Différence en RS")
            )
        )
    })

    # 2 - génération du barplot
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
                fill = indice
                # ,
                # text = sprintf("code site: %s<br>Latitude: %s<br>%s: %s", site_code, lat, ifelse(indice == "remplacement", "Remplacement", "Différence"), abs(value))
            )) +
            geom_bar(
                stat = "identity"
                # , width = 0.75
            ) +
            coord_flip() +
            scale_x_discrete(limits = the_order) +
            scale_fill_manual(values = c(cl_df$col[cl_df$site_type == input$habitat_select], cl_df$col_pale[cl_df$site_type == input$habitat_select])) +
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
        ggplotly(p, tooltip = "text") %>%
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
    # Map zone #
    # ------- #
    # 1 - génération de la carte LCBDdissimilarité
    output$dissi_map <- renderPlotly({
        lcbd_repl <- LCBD.comp(remdiff()$repl, sqrt.D = TRUE) # prendre la racine carre car pas euclidienne & Significance of the LCBD indices cannot be tested (cf function help)
        lcbd_richdiff <- LCBD.comp(remdiff()$rich, sqrt.D = TRUE)
        sit <- names(remdiff()$D)
        df_int <- data.frame(
            # inv = input$taxon_select,
            # hab = input$habitat_select,
            type = c(rep("repl", length(sit)), rep("richdiff", length(sit))),
            site_code = rep(sit, 2),
            LCBD = c(lcbd_repl$LCBD, lcbd_richdiff$LCBD)
        )
        df_int_sf <- left_join(df_int, sites[, c("site_code", "lat", "lon")], by = join_by(site_code)) |> st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))

        df_int_sf <- df_int_sf |>
            mutate(text = paste(
                "Code site: ", site_code,
                "\nMécanisme: ", ifelse(type == "repl", "Remplacement", "Différence en RS"),
                "\nValeur: ", round(LCBD, 2)
            ))
        p_map <- ggplot() +
            geom_sf(
                data = qc3
            ) +
            geom_sf(
                data = lakes_qc3,
                fill = "white"
            ) +
            geom_sf(data = df_int_sf, aes(
                size = LCBD,
                colour = type,
                fill = type,
                text = text
            )) +
            scale_color_manual(values = c(cl_df$col[cl_df$site_type == input$habitat_select], cl_df$col_pale[cl_df$site_type == input$habitat_select])) +
            scale_fill_manual(values = c(cl_df$col[cl_df$site_type == input$habitat_select], cl_df$col_pale[cl_df$site_type == input$habitat_select])) +
            facet_wrap(vars(type)) +
            # labs(x = "Longitude", y = "Latitude") +
            theme(
                legend.position = "none",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                text = element_text(size = 20),
                panel.background = element_rect(fill = "transparent", color = "transparent"),
            )
        ggplotly(p_map, tooltip = "text")
    })

    # Button zone #
    # ----------- #
    # barplot button
    output$barplot_button <- renderUI({
        actionButton("BarplotButton", NULL, icon = icon("info"), style = "border-radius: 50%;")
    })
    # triplot button
    output$triplot_button <- renderUI({
        actionButton("TriplotButton", NULL, icon = icon("info"), style = "border-radius: 50%;")
    })
    # map button
    output$map_button <- renderUI({
        actionButton("MapButton", NULL, icon = icon("info"), style = "border-radius: 50%;")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
