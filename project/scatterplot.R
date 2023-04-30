# Load required packages
library(ggplot2)
library(dplyr)
library(shiny)
library(readr)

# Read data from CSV file
indicator.2 <- read_csv("/cloud/project/indicator 2.csv")

# Define UI for Shiny app
ui <- fluidPage(
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      
      # Dropdown menu for selecting country column
      selectInput("country", "Select a country:",
                  choices = unique(indicator.2$country)),
      
      # Dropdown menu for selecting sex column
      selectInput("Gender", "Select gender:",
                  choices = unique(indicator.2$sex))
    ),
    
    # Main panel
    mainPanel(
      
      # Plot output
      plotOutput("scatterplot")
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  
  # Create reactive filtered dataset based on user inputs
  filtered_data <- reactive({
    indicator.2 %>%
      filter(country == input$country, sex == input$sex) %>%
      select(year, obs_value) %>%
      filter(year >= 2001 & year <= 2020)
  })
  
  # Create scatterplot with regression line
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

# Run Shiny app
shinyApp(ui, server)
