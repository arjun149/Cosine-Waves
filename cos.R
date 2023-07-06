
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cosine Wave - Try for yourself"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("freq",
                        "Frequency:",
                        min = 1,
                        max = 10,
                        value = 4),
                           
             shinythemes::themeSelector(),
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("cosinePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$cosinePlot <- renderPlot({
       t <- seq(0, 10, 0.1)
       #as.numeric(input$freq)
       y <- sin(as.numeric(input$freq)*t)
      
       qplot(t, y, geom="path", xlab="x" , ylab = "y", color = "red", fill="blue")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
