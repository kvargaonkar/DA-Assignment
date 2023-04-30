
indicator.2 <- read.csv("/cloud/project/indicator 2.csv")

indicator.2$year <- as.Date(paste(indicator.2$year, "-01-01", sep = ""), "%Y-%m-%d")

library(dplyr)
df <- indicator.2 %>% 
  filter(year >= as.Date("2001-01-01") & year <= as.Date("2020-01-01") & obs_value != 0)

library(shiny)

ui <- fluidPage(
  titlePanel("Long-Term Trends in BMI for Children Aged 2-3 Years: 2001-2020)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country:", choices = unique(df$country), selected = "Afghanistan")
    ),
    mainPanel(
      plotOutput("tsplot")
    )
  )
)

server <- function(input, output) {
  output$tsplot <- renderPlot({
    # filter the data based on the selected country
    df_filtered <- df %>% 
      filter(country == input$country) %>% 
      group_by(year, sex) %>% 
      summarize(mean_obs_value = mean(obs_value))
    
    # create the time series chart using ggplot2
    ggplot(df_filtered, aes(x = year, y = mean_obs_value, color = sex)) +
      geom_line() +
      labs(title = paste("Average BMI for children aged 2-3 by Year and Gender in", input$country, "(excluding zero values)"),
           x = "Year",
           y = "Average BMI Value")
  })
}

shinyApp(ui, server)
