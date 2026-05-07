if (FALSE) { # rlang::is_interactive()

library(shiny)
library(ggplot2)
library(bslib)

ui <- page_fillable(
  h1("Example", code("mtcars"), "dashboard"),
    card(
      full_screen = TRUE,
      card_header("Number of forward gears"),
      plotOutput("gear")
    ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Number of carburetors"),
      plotOutput("carb")
    ),
  card(
    full_screen = TRUE,
    card_header("Weight vs. Quarter Mile Time"),
    layout_sidebar(
      sidebar = sidebar(
        varSelectInput("var_x", "Compare to qsec:", mtcars[-7], "wt"),
        varSelectInput("color", "Color by:", mtcars[-7], "cyl"),
        position = "right"
      ),
      plotOutput("var_vs_qsec")
    )
  ))
)

server <- function(input, output) {
  for (var in c("cyl", "vs", "am", "gear", "carb")) {
    mtcars[[var]] <- as.factor(mtcars[[var]])
  }

  output$gear <- renderPlot({
    ggplot(mtcars, aes(gear)) + geom_bar()
  })

  output$carb <- renderPlot({
    ggplot(mtcars, aes(carb)) + geom_bar()
  })

  output$var_vs_qsec <- renderPlot({
    req(input$var_x, input$color)

    ggplot(mtcars) +
      aes(y = qsec, x = !!input$var_x, color = !!input$color) +
      geom_point()
  })
}

shinyApp(ui, server)
}
