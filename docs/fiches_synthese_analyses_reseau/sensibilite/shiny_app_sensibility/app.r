source("data_prep.r")

# ---------- #
# UI ----
# ----------#
ui <- navbarPage(
    shinyWidgets::useShinydashboard(),
    title = "My App",
    tabPanel(
        "Sensibilité des groupes taxonomiques",
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
                            tags$b("Importance d'une approche holistique")
                        ),
                        width = 4,
                        "Ces analyses mettent en évidence l’importance des pressions anthropiques et des facteurs climatiques lorsqu’ils sont pris en compte avec les autres variables affectant la biodiversité."
                    ),
                    # box 2
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Sentinelle face aux changements climatiques")
                        ),
                        width = 4,
                        "En considérant une distinction par habitat, plusieurs groupes sentinelle pourraient être identifiés. Concernant les pressions associées aux changements climatiques, la végétation semble être un bon candidat pour les habitats associés aux forêts (toutes strates confondues), à la toundra (strate arbustif) et aux marais (herbacés) alors que les oiseaux semblent les plus pertinents pour les habitats associés aux tourbières."
                    ),
                    # box 3
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/etat_connaissances/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Sentinelle face aux pressions anthropiques")
                        ),
                        width = 4,
                        "Concernant les pressions anthropiques, la végétation semble être un bon candidat pour devenir un groupesentinelle pour les habitats associés aux forêts (toutes strates confondues) et aux marais (herbacés) alors que les coléoptères et les oiseaux semblent les plus pertinents pour les habitats associés à la toundra et aux tourbières, respectivement."
                    ),
                ),
                # second row for dataselection & map visualisation on two columns
                fluidRow(
                    box(
                        width = 6,
                        title = "Groupe taxonomique",
                        selectInput("taxon_select",
                            label = "",
                            choices = taxon
                        )
                    ),
                    box(
                        width = 6,
                        title = "Habitat",
                        uiOutput("habitat",
                            label = ""
                        ) # associated to renderUI in server section
                    )
                ),
                fluidRow(
                    box(
                        width = 4,
                        title = "",
                        plotlyOutput("map", height = "600px")
                    ),
                    box(
                        width = 4,
                        title = "",
                        plotOutput("venn_diag", height = "600px")
                    ),
                    box(
                        width = 4,
                        title = "",
                        dataTableOutput("var_dt")
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
        n <- names(beta_hab_ls[[input$taxon_select]])
        selectInput("habitat_select",
            label = "",
            choices = c("global", sapply(strsplit(n, "-"), `[`, 2))
        )
    })

    # Interactive data zone #
    # --------------------- #
    # selection des données en fonction des filtres précédents
    # --- #
    lcbd <- reactive({
        if (input$habitat_select == "global") {
            data <- data.frame(
                taxon = input$taxon_select,
                lcbd = beta_ls[[input$taxon_select]][[1]]$LCBD,
                site_code = names(beta_ls[[input$taxon_select]][[1]]$LCBD)
            )
            data <- data |>
                left_join(sites[, c("site_code", "lat", "lon", "type")], by = join_by(site_code)) |>
                rename(habitat = type) |>
                st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
        } else {
            data <- data.frame(
                taxon = input$taxon_select,
                habitat = input$habitat_select,
                lcbd = beta_hab_ls[[input$taxon_select]][[paste(input$taxon_select, input$habitat_select, sep = "-")]]$LCBD,
                site_code = names(beta_hab_ls[[input$taxon_select]][[paste(input$taxon_select, input$habitat_select, sep = "-")]]$LCBD)
            )
            data <- data |>
                left_join(sites[, c("site_code", "lat", "lon")], by = join_by(site_code)) |>
                st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))
        }
    })

    venn <- reactive({
        if (input$habitat_select == "global") {
            data <- varpart_ls[[input$taxon_select]]
        } else {
            data <- varpart_hab_ls[[input$taxon_select]][[input$habitat_select]]
        }
    })

    # Plot zone #
    # --------- #
    # 1 - generation de la carte de LCBD - y matrice
    output$map <- renderPlotly({
        data2 <-
            lcbd() |>
            mutate(text = paste(
                "Code site: ", site_code,
                "\nHabitat: ", habitat,
                "\nLCBD: ", round(lcbd, digits = 2)
            ))
        p_map <- ggplot() +
            geom_sf(
                data = qc2
            ) +
            geom_sf(
                data = lakes_qc3,
                fill = "white"
            ) +
            geom_sf(
                data = data2, aes(
                    size = lcbd,
                    text = text,
                    fill = habitat,
                    color = habitat
                ),
                shape = 21
            ) +
            scale_fill_manual(values = cl_df$col_pale[cl_df$site_type %in% unique(data2$habitat)]) +
            scale_color_manual(values = cl_df$col[cl_df$site_type %in% unique(data2$habitat)]) +
            theme(
                legend.position = "none",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                text = element_text(size = 10),
                panel.background = element_rect(fill = "transparent", color = "transparent"),
            )
        ggplotly(p_map, tooltip = "text")
    })

    # 2 - génération du diagramme de Venn
    output$venn_diag <- renderPlot({
        them <- data.frame(
            them = venn()[["varpart_correspondance"]]$sign
        ) |>
            left_join(var_expl, by = join_by(them == sign))
        plot(
            venn()[["partition_table"]],
            Xnames = them$short_sign,
            bg = venn()[["varpart_correspondance"]]$col,
            cex = 1,
            id.size = 1.5
        )
    })

    # 3 - génération du tableau résumant les variables selectionnées pour les matrices Xs
    output$var_dt <- renderDataTable({
        dt <- venn()$variables_selec
        dt$var[nchar(dt$var) == 0] <- NA
        dt <- dt[!is.na(dt$var), ]
        dt_ls <- split(dt, dt$categorie)

        dt_ls2 <- lapply(dt_ls, function(x) {
            if (str_detect(x$var, ",") == F) {
                x <- x
            } else {
                var <- strsplit(x$var, ",")
                x <- data.frame(
                    categorie = x$categorie,
                    var = unlist(var)
                )
            }
        })

        dt2 <- do.call("rbind", dt_ls2) |>
            left_join(var_expl[, c("sign", "long_sign")], by = join_by(categorie == sign)) |>
            select(-categorie) |>
            rename(Catégorie = long_sign, Variables = var) |>
            relocate(Catégorie)
        datatable(
            dt2,
            rownames = F
        )
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
                "Carte des valeurs de contribution locale à la diversité béta (LCBD)",
                tags$br(),
                tags$span(style = "color: black; font-weight: bold;", "Interprétation"),
                tags$br(),
                "Ajout D'informations ici"
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
    # Venn diagram button
    # Table button
}

# Run the application
shinyApp(ui = ui, server = server)
