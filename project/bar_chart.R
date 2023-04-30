indicator.2 <- read.csv("/cloud/project/indicator 2.csv")
library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  selectInput("year", "Select year:",
              choices = 2001:2020),
  plotOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    mean_data <- indicator.2 %>%
      filter(year == input$year) %>%
      group_by(sex) %>%
      summarize(mean_obs_value = mean(obs_value, na.rm = TRUE))
    
    ggplot(mean_data, aes(x = sex, y = mean_obs_value, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste0("Average BMI value according to gender 
                          (Required BMI for Female=15.5, Required BMI for Male=15.9) for the year ", input$year),
           x = "Sex",
           y = "Average BMI Value") +
      theme_minimal()
  })
}

shinyApp(ui, server)

