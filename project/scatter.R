
library(ggplot2)
library(dplyr)
library(shiny)
library(readr)

indicator.2 <- read_csv("/cloud/project/indicator 2.csv")
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a country:",
                  choices = unique(indicator.2$country)),
      selectInput("sex", "Select Gender:",
                  choices = unique(indicator.2$sex))
    ),
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)
server <- function(input, output) {

  filtered_data <- reactive({
    indicator.2 %>%
      filter(country == input$country, sex == input$sex) %>%
      select(year, obs_value) %>%
      filter(year >= 2001 & year <= 2020)
  })
  
  output$scatterplot <- renderPlot({
    ggplot(filtered_data(), aes(x = year, y = obs_value)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Observed BMI value by year",
           x = "Year",
           y = "Observed BMI value") +
      theme_minimal()
  })
}

shinyApp(ui, server)
