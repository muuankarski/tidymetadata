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
library(tidymetadata)
library(haven)

d <- read_sav("http://www.lisdatacenter.org/wp-content/uploads/it04ip.sav")
meta <- create_metadata(d)
d <-    strip_attributes(d)

numeric_vars <-        unique(meta[meta$class %in% "numeric",]$code)
names(numeric_vars) <- unique(meta[meta$class %in% "numeric",]$name)

factor_vars <-        unique(meta[meta$class %in% "factor",]$code)
names(factor_vars) <- unique(meta[meta$class %in% "factor",]$name)

ui <- fluidPage(

  # Application title
  titlePanel("Tidymetadata package with Luxembourg Income Study data"),

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var_numeric", "Select numeric variable:",
                  choices = numeric_vars,
                  selected = "pi"),
      selectInput(inputId = "var_factor", "Select classifying factor variable:",
                  choices = factor_vars,
                  selected = "educ"),
      radioButtons(inputId = "method", label = "Select summary method:",
                   choices = c("mean","median"),
                   selected = "mean")
    ),

    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({

    d$var <- d[[input$var_numeric]]
    d$class <- label_data(d, variable.data = input$var_factor, into.factor=TRUE)


    if (input$method == "mean") d %>% group_by(class) %>%
      summarise(value = mean(var, na.rm=TRUE)) -> dat
    if (input$method == "median") d %>% group_by(class) %>%
      summarise(value = median(var, na.rm=TRUE)) -> dat

    ggplot(dat, aes(x=class,y=value)) + geom_col()

  })
}

# Run the application
shinyApp(ui = ui, server = server)

