#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(tidyverse)
library(broom)
f <- "https://raw.githubusercontent.com/EF237/Extra-Credit-Shiny-Web-Application/refs/heads/main/CDV-Data.csv"
d <- read_csv(f, col_names = TRUE)

d <- d |>
  rename(
    HR_press = "HR-press",
    LR_press = "LR-press",
    TOT_HR_percent = "TOT HR%"
  )

d$Subject <- factor(d$Subject)
d$Sex <- factor(d$Sex)

r <- c("TOT_HR_percent", "HR_press", "LR_press")
p <- names(d)

# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Blackjack Task Visualizer - Risk Training")),
  sidebarLayout(
    sidebarPanel(width = 5,
                 selectInput("response",
                             label = "Choose a response variable...",
                             choices = c("", r)),
                 selectInput("predictors",
                             label = "Choose one or more predictor variables...",
                             choices = p,
                             multiple = TRUE),
                 textOutput("model"),
                 tableOutput("modelresults")
    ),
    mainPanel(width = 5,
              dataTableOutput("datatable"),
              plotOutput("plot"))
  )
)

# Define server logic ----
server <- function(input, output) {
  output$datatable <- renderDataTable(d, options = list(
    paging = TRUE,
    lengthMenu = list(c(5, 10, 25, -1), c('5', '10', '25', 'All')),
    pageLength = 5
  ))

  m <- reactive({
    model <- NULL
    if (input$response == "" | length(input$predictors) == 0) {
      return(model)
    }
    model <- paste0(input$response, " ~ ", paste(input$predictors, collapse = " + "))
    return(model)
  })

  output$model <- renderText({
    paste0("Model: ", print(m()))
  })

  output$modelresults <- renderTable({
    if (!is.null(m())) {
      residuals <- lm(data = d, formula = as.formula(m()))
      residuals <- as.data.frame(coefficients(residuals))
      names(residuals) <- "Beta"
      residuals
    }
  }, width = "100%", rownames = TRUE, digits = 3)

  output$plot <- renderPlot({
    if (!is.null(m()) & length(input$predictors) == 1) {
      y <- input$response
      x <- input$predictors
      if (class(d[[x]]) != "factor") {
        p <- ggplot(data = d, aes(x = .data[[x]], y = .data[[y]])) +
          geom_point() +
          geom_smooth(method = lm)
      } else {
        p <- ggplot(data = d, aes(x = .data[[x]], y = .data[[y]])) +
          geom_violin() +
          geom_jitter(width = 0.2, alpha = 0.5)
      }
      p + xlab(x) + ylab(y) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else if (!is.null(m()) & length(input$predictors) == 2) {
      y <- input$response
      x <- input$predictors
      if (class(d[[x[1]]]) == "factor" & class(d[[x[2]]]) == "factor") {
        p <- ggplot(data = d, aes(x = .data[[x[1]]], y = .data[[y]])) +
          geom_violin() +
          geom_jitter(width = 0.2, alpha = 0.5) +
          facet_wrap(~d[[x[2]]])
        p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) != "factor" & class(d[[x[2]]]) == "factor") {
        p <- ggplot(data = d, aes(x = .data[[x[1]]], y = .data[[y]])) +
          geom_point() +
          geom_smooth(method = lm) +
          facet_wrap(~d[[x[2]]])
        p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) == "factor" & class(d[[x[2]]]) != "factor") {
        p <- ggplot(data = d, aes(x = .data[[x[2]]], y = .data[[y]])) +
          geom_point() +
          geom_smooth(method = lm) +
          facet_wrap(~d[[x[1]]])
        p + xlab(x[2]) + ylab(y)
      }
    }
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
