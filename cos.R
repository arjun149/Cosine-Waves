
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ODE Solver"),
  #xc
  #u
  #x
  #tx
  #B
  #k
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("k",
                  "K value:",
                  min = 1,
                  max = 10,
                  value = 1),
      sliderInput("xc", 
                  "Xc value:", 
                  min = 1, 
                  max = 10,
                  value = 1),
      sliderInput("u", 
                  "U (mew) value:",
                  min = 1,
                  max = 10,
                  value = 1),
      sliderInput("x", 
                  "X value:",
                  min = 1,
                  max = 10,
                  value = 1),
      sliderInput("tx", 
                  "Tx (tau-x) value:",
                  min = 1,
                  max = 10,
                  value = 1),
      sliderInput("b", 
                  "B value:",
                  min = 1,
                  max = 10, 
                  value = 1),
      
      shinythemes::themeSelector(),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("odePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  source(file.path("odeSolver.R"), local = TRUE)$value 
  
  output$odePlot <- renderPlot({
    x0 <- c(x = input$x, xc = input$xc)
    # parameters
    params <- c(k = input$k, u = input$u, tx = input$tx, b = input$b)
    # time points
    times <- seq(0, 30, by = 0.1)
    # solve ODE
    out <- ode(y = x0, times = times, func = model, parms = params)
    # convert result to dataframe
    out <- as.data.frame(out)
    # plot results
    plot(out$time, out$x1, type = "l", xlab = "Time", ylab = "x1")
  })
  
  #    output$cosinePlot <- renderPlot({
  #       t <- seq(0, 10, 0.1)
  #as.numeric(input$freq)
  #       y <- sin((as.numeric(input$freq))*t)
  
  #      qplot(t, y, geom="path", xlab="x" , ylab = "y", color = "red", fill="blue")
  
  #   })
}

# Run the application 
shinyApp(ui = ui, server = server)
