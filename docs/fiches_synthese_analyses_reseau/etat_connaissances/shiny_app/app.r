source("docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/data_prep.r")

# ---------- #
# UI ----
# ----------#
ui <- navbarPage(
    shinyWidgets::useShinydashboard(),
    title = "My App",
    tabPanel(
        "Représentativité des inventaires",
        # icon = icon("home"),
        icon = tags$img(src = "https://png.pngtree.com/element_our/20200702/ourmid/pngtree-construction-sign-psd-transparent-bottom-image_2292003.jpg", height = "30px"),
        fluidPage(
            mainPanel(
                width = 12,
                # 1st fluid row for value boxes
                fluidRow(
                    # box 1
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissance/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("TITLE")
                        ),
                        width = 4,
                        "xxx"
                    ),
                    # box 2
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissance/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("TITLE")
                        ),
                        width = 4,
                        "xxx"
                    ),
                    # box 3
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissance/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("TITLE")
                        ),
                        width = 4,
                        "xxx"
                    ),
                ),
                # second row for dataselection & map visualisation on two columns
                fluidRow(
                    column(
                        4,
                        box(
                            title = "Groupe taxonomique",
                            selectInput("taxon_select",
                                label = "",
                                choices = taxon
                            )
                        ),
                        box(
                            title = "Habitat",
                            uiOutput("habitat",
                                label = ""
                            ) # associated to renderUI in server section
                        )
                    ),
                    column(
                        8,
                        box(
                            width = 12,
                            div(HTML("<b>Carte de représentativité</b> "), style = "display: inline-block;"),
                            actionButton("map_button", label = "", icon = icon("info"), style = "display: inline-block;"),
                            plotlyOutput("", height = "900px")
                        )
                    )
                )
            )
        )
    )
)

# ---------- #
# Server ----
# ---------- #

server <- function(input, output) {
    #### Habitat selection depending on taxon choice
    output$habitat <- renderUI({
        n <- unique(sites_est$type_site[sites_est$type_campaign == input$taxon_select])
        selectInput("habitat_select",
            label = "",
            choices = c("global", n)
        )
    })

    # Interactive data zone #
    # --------------------- #
    # selection des données en fonction des filtres précédents
    # --- #
    est <- reactive({
        if (input$habitat_select == "global") {
            data <- sites_est[sites_est$type_campaign == input$taxon_select, ]
        } else {
            data <- sites_est[sites_est$type_campaign == input$taxon_select & sites_est$type_site == input$habitat_select, ]
        }

        data <- data |>
            st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
    })

    # Plot zone #
    # --------- #

    # 1 - génération de la carte de représentativite des inventaires
    output$map <- renderPlotly({
        data_sf <-
            est() |>
            mutate(text = paste(
                "Code site: ", code_site,
                "\nHabitat: ", type_site,
                "\nNb espèces observées: ", sp_observed,
                "\nNb espèces estimées: ", sp_estimator,
                "\nReprésentativité: ", prop_obs, "%"
            ))
        p_map <- ggplot() +
            geom_sf(
                data = qc3
            ) +
            geom_sf(
                data = lakes_qc3,
                fill = "white"
            ) +
            geom_sf(
                data = data_sf, aes(
                    size = prop_obs,
                    text = text
                ),
                color = cl_df$col[cl_df$site_type == input$habitat_select],
                fill = cl_df$col_pale[cl_df$site_type == input$habitat_select],
                shape = 21
            ) +
            theme(
                legend.position = "none",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                text = element_text(size = 10),
                panel.background = element_rect(fill = "transparent", color = "transparent"),
            )
        ggplotly(p_map, tooltip = "text")
    })

    # Button zone #
    # ----------- #
    # map button
    observeEvent(input$map_button, {
        shinyalert(
            title = "Méthodologie & interprétations",
            text = tagList(
                tags$span(style = "color: red;", "Red Text"),
                tags$br(),
                "Standard text follows."
            ),
            size = "m",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = TRUE,
            type = "",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#E0B658",
            timer = 0,
            imageUrl = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissance/shiny_app/www/Logo_bq_sans_texte.png?raw=true",
            imageWidth = 100,
            imageHeight = 100,
            animation = FALSE
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
