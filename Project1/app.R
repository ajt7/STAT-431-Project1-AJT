#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)

datByDegreesUnclean <- read_csv(here("Project1/degrees-that-pay-back.csv"))
datByCollegeUnclean <- read_csv(here("Project1/salaries-by-college-type.csv"))
datByRegionUnclean <- read_csv(here("Project1/salaries-by-region.csv"))

# Converting Salary columns from strings to numeric
datByDegrees <- datByDegreesUnclean %>% mutate(
  `Starting Median Salary` = as.numeric(gsub('[$,]', '', `Starting Median Salary`)),
  `Mid-Career Median Salary` = as.numeric(gsub('[$,]', '', `Mid-Career Median Salary`))
  )
datByCollege <- datByCollegeUnclean %>% mutate(
  `Starting Median Salary` = as.numeric(gsub('[$,]', '', `Starting Median Salary`)),
  `Mid-Career Median Salary` = as.numeric(gsub('[$,]', '', `Mid-Career Median Salary`))
)
datByRegion <- datByRegionUnclean  %>% mutate(
  `Starting Median Salary` = as.numeric(gsub('[$,]', '', `Starting Median Salary`)),
  `Mid-Career Median Salary` = as.numeric(gsub('[$,]', '', `Mid-Career Median Salary`))
)

dataChoices = c("Undergraduate Degree", "Type of College", "Region of College")

# Must correspond to the column names in the datasets
responseChoices = c("Starting Median Salary", "Mid-Career Median Salary")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Where it Pays to Attend College"),

    # Sidebar with selection inputs 
    sidebarLayout(
        sidebarPanel(
            selectInput("dataset",
                        "Data categorized by:",
                        choices = dataChoices),
            selectInput("response",
                        "Salary type:",
                        choices = responseChoices)
        ),

        # Show the plot
        mainPanel(
           plotOutput("salaryPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$salaryPlot <- renderPlot({
      if(input$dataset == dataChoices[1]){
        
        # Example plot of salaries by degree (insert your plot here)
        datByDegrees %>% ggplot(aes(x = `Undergraduate Major`, y = .data[[input$response]])) +
          geom_point()
        
      } else if(input$dataset == dataChoices[2]){
        
        # Example plot of salaries by college type (insert your plot here)
        datByCollege %>% ggplot(aes(x = `School Type`, y = .data[[input$response]])) + 
          geom_boxplot()
        
      } else if(input$dataset == dataChoices[3]){
        
        # Example plot of salaries by region (insert your plot here)
        datByRegion %>% ggplot(aes(x = Region, y = .data[[input$response]])) + 
          geom_boxplot()
        
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
