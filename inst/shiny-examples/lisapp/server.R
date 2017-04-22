#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    d$var <- d[[input$var_numeric]]
    d$class <- label_data(d, variable.data = input$var_factor, into.factor=TRUE)


    if (input$method == "mean") d %>% group_by(class) %>%
      summarise(value = mean(var, na.rm=TRUE)) -> dat
    if (input$method == "median") d %>% group_by(class) %>%
      summarise(value = median(var, na.rm=TRUE)) -> dat

    ggplot(dat, aes(x=class,y=value)) + geom_col()

  })

})
