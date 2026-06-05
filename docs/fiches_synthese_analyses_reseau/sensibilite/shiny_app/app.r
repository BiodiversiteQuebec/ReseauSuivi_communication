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
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Représentativité")
                        ),
                        width = 4,
                        "Les protocoles mis en place par le Réseau de Suivi présente une excellente représentativité de la biodiversité du Québec à hauteur de 84.4%, tous groupes taxonomiques confondus."
                    ),
                    # box 2
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Groupes taxonomiques gagnants vs perdants")
                        ),
                        width = 4,
                        "Les groupes taxonomiques les mieux représentés sont les chiroptères et les anoures, alors que les papillons et les insectes du sol présentent des inventaites moins exhaustifs. "
                    ),
                    # box 3
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Où mettre plus d'efforts ?")
                        ),
                        width = 4,
                        "Certains sites en particuliers présentent un fort déficit dans leur niveau de représentativité de la biodiversité pour certains groupes taxonomiques."
                    ),
                ),
                # second row for dataselection & map visualisation on two columns
                fluidRow(
                    column(
                        4,
                        fluidRow(box(
                            width = 12,
                            title = "Groupe taxonomique",
                            selectInput("taxon_select",
                                label = "",
                                choices = taxon
                            )
                        )),
                        fluidRow(box(
                            width = 12,
                            title = "Habitat",
                            uiOutput("habitat",
                                label = ""
                            ) # associated to renderUI in server section
                        )),
                        fluidRow(box(
                            width = 12,
                            title = "",
                            plotlyOutput("taxon_map", height = "600px")
                        ))
                    ),
                    column(
                        8,
                        box(
                            width = 12,
                            div(HTML("<b>Carte de représentativité</b> "), style = "display: inline-block;"),
                            actionButton("map_button", label = "", icon = icon("info"), style = "display: inline-block;"),
                            plotlyOutput("map", height = "900px")
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
    # 0 - Génération de violin plot par groupe taxo
    output$taxon_map <- renderPlotly({
        v_plot <- sites_est |>
            ggplot(aes(x = id, y = prop_obs, fill = type_campaign, colour = type_campaign)) +
            geom_violin() +
            scale_fill_manual(values = c("#242e8675", "#b881577d", "#88bcb985", "#e3bc6983", "#435a518d")) +
            scale_colour_manual(values = c("#242e86", "#b88157", "#88bcb9", "#e3bd69", "#435a51")) +
            geom_jitter(aes(text = code_site), shape = 16, position = position_jitter(0.05), colour = "black", alpha = 0.5, cex = 2) +
            labs(x = "", y = "") +
            scale_y_continuous(limits = c(0, 100)) +
            scale_x_continuous(breaks = 1:5, labels = c("Chiroptères", "Oiseaux", "Orthoptères", "Plantes", "Insectes du sol")) +
            theme(
                legend.position = "none",
                # strip.background = element_blank(),
                # strip.text.x = element_blank(),
                # axis.text.x = element_blank(), # Masque les labels texte originaux
                # axis.ticks.x = element_blank(), # Masque les graduations de l'axe X
                text = element_text(size = 15),
                panel.background = element_rect(fill = "transparent", color = "transparent")
            )

        v_plot2 <- ggplotly(v_plot, tooltip = "text")
        gg_plotly <- style(v_plot2, hoverinfo = "skip", traces = 2)
        gg_plotly
    })

    # 1 - génération de la carte de représentativite des inventaires
    output$map <- renderPlotly({
        data_sf <-
            est() |>
            mutate(text = paste(
                "Code site: ", code_site,
                "\nTaxon: ", type_campaign,
                "\nHabitat: ", type_site,
                "\nNb espèces observées: ", sp_observed,
                "\nNb espèces estimées: ", round(sp_estimator, digits = 0),
                "\nReprésentativité: ", round(prop_obs, digits = 2), "%"
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
                    text = text,
                    fill = type_site,
                    color = type_site
                ),
                shape = 21
            ) +
            scale_fill_manual(values = cl_df$col_pale[cl_df$site_type %in% unique(data_sf$type_site)]) +
            scale_color_manual(values = cl_df$col[cl_df$site_type %in% unique(data_sf$type_site)]) +
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
                tags$span(style = "color: black; font-weight: bold;", "Méthode"),
                tags$br(),
                "Dans un premier temps, le nombre d'espèces attendues a été estimé pour chaque site à partir des occurrences présentes dans la base de données. Dans un second temps, nous avans calculé le ratio entre le nombre d'espèces observées et le nombre d'espèces attendues afin d'estimer la représentativité associés au inventaires.",
                tags$br(),
                tags$span(style = "color: black; font-weight: bold;", "Interprétation"),
                tags$br(),
                "Plus la valeur de représentativité est élevée, plus les inventaires utilisés peuvent être considérés comme efficaces pour capturer de façon exhaustive la biodiversité."
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
            imageUrl = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/www/Logo_bq_sans_texte.png?raw=true",
            imageWidth = 100,
            imageHeight = 100,
            animation = FALSE
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
