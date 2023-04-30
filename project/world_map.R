library(shiny)
library(ggplot2)
library(dplyr)

indicator.2 <- read.csv("/cloud/project/indicator 2.csv")
mapdata <- map_data("world")
joined_data <- left_join(indicator.2, mapdata, by = c("country" = "region"))

ui <- fluidPage(
  titlePanel("Visualize BMI Trends Across the World: An Interactive Map Showing Male, Female and Total BMI Values of children aged 2 to 3 years from 1987 to 2021"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year", label = "Select year", choices = unique(joined_data$year)),
      selectInput(inputId = "sex", label = "Select Gender", choices = unique(joined_data$sex))
    ),
    mainPanel(
      plotOutput("map_plot")
    )
  )
)

server <- function(input, output) {
  mean_data <- reactive({
    joined_data %>%
      filter(year == input$year, sex == input$sex) %>%
      group_by(country) %>%
      summarise(mean_obs_value = mean(obs_value, na.rm = TRUE))
  })
  mapdata_mean <- reactive({
    left_join(mapdata, mean_data(), by = c("region" = "country"))
  })
  output$map_plot <- renderPlot({
    ggplot(mapdata_mean(), aes(x = long, y = lat, group = group, fill = mean_obs_value)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "#CD0161", high = "#F89246", na.value = "grey90") +
      theme_void()
  })
}
shinyApp(ui = ui, server = server)

