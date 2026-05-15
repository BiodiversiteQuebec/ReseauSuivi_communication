# source("docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/data_prep.r")
# ---------- #
# UI ----
# ----------#
ui <- navbarPage(
    shinyWidgets::useShinydashboard(),
    title = "My App",
    tabPanel(
        "Dissimilarité des sites",
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
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Dissimilarité et groupes taxonomiques")
                        ),
                        width = 4,
                        "Le niveau de dissimilarité entre les sites est variable en fonction des groupes taxonomiques."
                    ),
                    # box 2
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Mécanismes opérants")
                        ),
                        width = 4,
                        "La force des mécanismes soujacents à la dissimilarité des sites est différente selon les groupes taxonomiques."
                    ),
                    # box 3
                    box(
                        title = div(
                            tags$img(src = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png?raw=true", height = "30px", style = "margin-right: 10px;"),
                            tags$b("Sites originaux")
                        ),
                        width = 4,
                        "Certains sites semblent présenter une contribution particulière quant à la différence en richesse spécifique pour les chiroptères, orthoptères et les insectes su sol."
                    ),
                ),
                # second row for dataselection
                fluidRow(
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
                # third fluid row for map and plots
                fluidRow(
                    # 1st column for plots
                    column(
                        6,
                        # fluidRow for barplot
                        fluidRow(
                            box(
                                width = 12,
                                div(HTML("<b>Dissimilarité - Barplot</b> "), style = "display: inline-block;"),
                                actionButton("info_btn", label = "", icon = icon("info"), style = "display: inline-block;"),
                                plotlyOutput("barplot", height = "400px")
                            )
                        ),
                        br(),
                        # fluidRow for alpha div map
                        fluidRow(
                            box(
                                width = 12,
                                div(HTML("<b>Diversité alpha</b> "), style = "display: inline-block;"),
                                actionButton("alpha_button", label = "", icon = icon("info"), style = "display: inline-block;"),
                                plotlyOutput("alpha_map", height = "400px")
                            )
                        )
                    ),
                    # 2nd column for map
                    column(
                        6,
                        box(
                            width = 12,
                            div(HTML("<b>Carte de contribution locale</b> "), style = "display: inline-block;"),
                            actionButton("map_button", label = "", icon = icon("info"), style = "display: inline-block;"),
                            plotlyOutput("dissi_map", height = "900px")
                        )
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

    # 1 - génération de la carte de diversité alpha
    output$alpha_map <- renderPlotly({
        if (input$habitat_select == "global") {
            data <- alpha_ls[[input$taxon_select]]
        } else {
            dt <- alpha_ls[[input$taxon_select]]
            data <- dt[dt$habitat == input$habitat_select, ]
        }

        data_sf <-
            left_join(data, sites[, c("site_code", "lat", "lon")], by = join_by(site_code)) |>
            st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)) |>
            mutate(text = paste(
                "Code site: ", site_code,
                "\nHabitat: ", habitat,
                "\nDiversité alpha: ", n_sp
            ))
        p_map <- ggplot() +
            geom_sf(
                data = qc3
            ) +
            geom_sf(
                data = lakes_qc3,
                fill = "white"
            ) +
            geom_sf(data = data_sf, aes(
                size = n_sp,
                # shape = 21,
                color = habitat,
                fill = habitat,
                text = text
            )) +
            scale_fill_manual(cl_df$col_pale[cl_df$site_type == input$habitat_select]) +
            scale_color_manual(cl_df$col[cl_df$site_type == input$habitat_select]) +
            theme(
                legend.position = "none",
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                text = element_text(size = 10),
                panel.background = element_rect(fill = "transparent", color = "transparent"),
            )
        ggplotly(p_map, tooltip = "text")
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
                fill = indice,
                text = sprintf("code site: %s<br>Latitude: %s<br>%s: %s", site_code, lat, ifelse(indice == "remplacement", "Remplacement", "Différence"), abs(value))
            )) +
            geom_bar(
                stat = "identity"
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
    observeEvent(input$info_btn, {
        shinyalert(
            title = "Méthodologie & interprétations",
            text = "This is a modal",
            size = "l",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#E0B658",
            timer = 0,
            imageUrl = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png?raw=true",
            imageWidth = 100,
            imageHeight = 100,
            animation = FALSE
        )
    })
    # triplot button
    observeEvent(input$alpha_button, {
        shinyalert(
            title = "Méthodologie & interprétations",
            text = "This is a modal",
            size = "l",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#E0B658",
            timer = 0,
            imageUrl = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png?raw=true",
            imageWidth = 100,
            imageHeight = 100,
            animation = FALSE
        )
    })
    # map button
    observeEvent(input$map_button, {
        shinyalert(
            title = "Méthodologie & interprétations",
            text = "This is a modal",
            size = "l",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "OK",
            confirmButtonCol = "#E0B658",
            timer = 0,
            imageUrl = "https://github.com/BiodiversiteQuebec/ReseauSuivi_communication/blob/main/docs/fiches_synthese_analyses_reseau/dissimilarite/shiny_app/www/Logo_bq_sans_texte.png?raw=true",
            imageWidth = 100,
            imageHeight = 100,
            animation = FALSE
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
