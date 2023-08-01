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
      verbatimTextOutput("txtout"),
      sliderInput("n",         #found
                  "n value:", 
                  min = 0,
                  max = 1, 
                  value = 0.5),
      sliderInput("beta",       #found     
                  "beta value:", 
                  min = 0,
                  max = 1,
                  value = 0.0075),
  
      sliderInput("maxLux", 
                  "Maximum Lux value:", 
                  min = 0,
                  max = 10,
                  value = 5),
      
      sliderInput("lights_on", 
                  "Lights On value:", 
                  min = 0,
                  max = 24,
                  value = 10),
      
      sliderInput("k",                  #found
                  "k value:", 
                  min = 0, 
                  max = 1,
                  value = 0.55),
      sliderInput("u",                   #found
                  "U (mew) value:",
                  min = 0,
                  max = 1,
                  value = 0.23),
      sliderInput("tx",                  #found  
                  "Tx (tau-x) value:",
                  min = 0,
                  max = 30,
                  value = 24.2),
      sliderInput("b", 
                  "B value:",
                  min = 0,
                  max = 1, 
                  value = 0.5),
      
      shinythemes::themeSelector()
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("odePlot1"),
      plotOutput("odePlot2"),
      plotOutput("odePlot3"),
      plotOutput("ligPlot")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  source(file.path("odeSolver.R"), local = TRUE)$value 
  #source(file.path("plotty.R"), local = TRUE)$value 
  
  output$txtout <- renderText({
    "Please input your parameter values below..."
  })
  
  parameters <- reactive({
    params <- c(k = input$k,  u = input$u, 
                tx = input$tx,  b = input$b, 
                beta = input$beta, maxLux = input$maxLux, 
                lights_on = input$lights_on)
  })
  
  simulation_result <- reactive({
    params <- parameters()
    x0 <- c(x = -0.08, xc = -1.1, n = 0.5)
    times <- seq(0, 96, by = 0.1)
    out <- ode(y = x0, times = times, func = model, parms = params)
    out <- as.data.frame(out)
  })
  
  output$odePlot1 <- renderPlot({
    out <- simulation_result()
    plot(out$time, out$x, type = "l", xlab = "Time", ylab = "x")
  })

  output$odePlot2 <- renderPlot({
    out <- simulation_result()
    plot(out$time, out$xc, type = "l", xlab = "Time", ylab = "xc")

  })

  output$odePlot3 <- renderPlot({
    out <- simulation_result()
    plot(out$time, out$n, type = "l", xlab = "Time", ylab = "n")
  })
  
  output$ligPlot <- renderPlot({
    out <- simulation_result()
    times <- out$time
    params <- parameters()
    
    lightsOut <- c()
    
    for (t in times) {
      lightsOut <- append(lightsOut, light(t, params))
    }
    plot(times, lightsOut, type = "l", xlab = "Time", ylab = "Light")

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

