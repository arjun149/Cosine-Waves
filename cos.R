
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
  
  
 # x0 <- c(x = -0.08, xc = -1.1, n = input$n)
#xoVars <- c("x", "xc", "n")
  # parameters
#  params <- c(k = input$k, u = input$u, tx = input$tx, b = input$b, beta = input$beta, maxLux = input$maxLux, lights_on = input$lights_on)
  # time points
#  times <- seq(0, 96, by = 0.1)
  # solve ODE
#  out <- ode(y = x0, times = times, func = model, parms = params)
  # convert result to dataframe
#  out <- as.data.frame(out)
  
  params <- c(k = reactive({req(input$k)}), u = reactive({req(input$u)}), tx = reactive({req(input$tx)}), b = reactive({req(input$b)}), beta = reactive({req(input$beta)}), maxLux = reactive({req(input$maxLux)}), lights_on = reactive({req(input$lights_on)})) 
  params <- as.numeric(unlist(params))
  x0 <- c(x = -0.08, xc = -1.1, n = 0.5)
  times <- seq(0, 96, by = 0.1)
  out <- ode(y = x0, times = times, func = model, parms = params)
  out <- as.data.frame(out)
  
  output$odePlot1 <- renderPlot({
    plot(out$times, out$x, type = "l", xlab = "Time", ylab = "x")
    
    
  })
  
  output$odePlot2 <- renderPlot({
    
    plot(out$times, out$xc, type = "l", xlab = "Time", ylab = "xc")
    
  })
  
  output$odePlot3 <- renderPlot({
    
    plot(out$times, out$n, type = "l", xlab = "Time", ylab = "n")
    
  })
  
  #working
  
  output$ligPlot <- renderPlot({
    
    lightsOut <- c()
    
    for (t in times) {
    lightsOut <- append(lightsOut, light(t, params)) 
    }
    
    plot(times, lightsOut, type = "l", xlab = "Time", ylab = "Light")
    
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
}


