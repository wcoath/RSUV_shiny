library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI
ui <- fluidPage(
    titlePanel("SUVR simulation"),
    sidebarLayout(
        sidebarPanel(
            numericInput("n", "Number of obs:", value = 1000, min = 10, max = 10000),
            numericInput("x_mean", "Reference region uptake mean:", value = 5, min = 0, max = 10),
            numericInput("x_sd", "Reference region uptake SD:", value = 1, min = 0.1, max = 10),
            numericInput("slope1", "Slope (Population 1):", value = 1, min = 0, max = 10),
            numericInput("intercept1", "Intercept (Population 1):", value = 0, min = -10, max = 10),
            numericInput("e_mean1", "Mean error (Population 1):", value = 0, min = -10, max = 10),
            numericInput("e_sd1", "SD error (Population 1):", value = 0.5, min = 0.01, max = 10),
            numericInput("slope2", "Slope (Population 2):", value = 1.3, min = 0, max = 10),
            numericInput("intercept2", "Intercept (Population 2):", value = 0, min = -10, max = 10),
            numericInput("e_mean2", "Mean error (Population 2):", value = 0, min = -10, max = 10),
            numericInput("e_sd2", "SD error (Population 2):", value = 0.5, min = 0.01, max = 10),
            numericInput("cutpoint", "SUVR cutpoint:", value = 1.2, min = 0, max = 5),
            sliderInput("proportion", "% Population 1:", min = 0, max = 100, value = 75),
            actionButton("update", "Update Plot")
        ),
        mainPanel(
            plotOutput("regressionPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Generate data based on user inputs
    regression_data <- reactive({
        x <- abs(rnorm(input$n, mean = input$x_mean, sd = input$x_sd))
        
        # Number of observations for each population
        n_population1 <- round(input$proportion / 100 * input$n)
        n_population2 <- input$n - n_population1
        
        e_population1 <- rnorm(n_population1, mean = input$e_mean1, sd = input$e_sd1)
        e_population2 <- rnorm(n_population2, mean = input$e_mean2, sd = input$e_sd2)
        # Generate y for each population based on specified regression equations
        y_population1 <- pmax(input$intercept1 + input$slope1 * x[1:n_population1] + e_population1,0)
        y_population2 <- pmax(input$intercept2 + input$slope2 * x[(n_population1+1):input$n] + e_population2,0)
        
        # Combine x and y for both populations
        x <- c(x[1:n_population1], x[(n_population1+1):input$n])
        y <- c(y_population1, y_population2)
        
        # Create a data frame
        data.frame(x, y, group = factor(rep(c("Population 1", "Population 2"), c(n_population1, n_population2))))
    })
  
  # Render the plot
  output$regressionPlot <- renderPlot({
    data <- regression_data()
    
    cols <- c("#69b3a2", "#404080")
    
    # Scatter plot with linear regression line
    plot1 <- ggplot(data, aes(x = x, y = y, col = group)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = "red") +
      scale_color_manual(values=cols) +
        labs(title = "Target vs reference uptake",
             x = "Reference region uptake",
             y = "Target region uptake",
             color = "Population") +
        theme_minimal() +
      theme(plot.title = element_text(size=14,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size=12,face = "bold"),
            axis.title.y = element_text(size=12,face = "bold"),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
    
    # Scatter plot of ratio of y to x
    plot2 <- ggplot(data, aes(x = x, y = y / x, col = group)) +
      geom_point() +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = "red") +
      scale_color_manual(values=cols) +
      labs(title = "SUVR vs reference uptake",
           x = "Reference region uptake",
           y = "SUVR",
           color = "Population") +
      theme_minimal() +
      theme(plot.title = element_text(size=14,face = "bold",hjust = 0.5),
            axis.title.x = element_text(size=12,face = "bold"),
            axis.title.y = element_text(size=12,face = "bold"),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
    
    #histogram of y / x
    plot3 <- ggplot(data, aes(y / x, fill = group)) + 
        geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') +
        scale_fill_manual(values=cols) +
        geom_vline(xintercept = input$cutpoint) +
        labs(title = "Distribution of SUVR",
             x = "SUVR",
             fill = "Population") +
        theme_minimal() +
      theme(plot.title = element_text(size=14,face = "bold",hjust = 0.5),
        axis.title.x = element_text(size=12,face = "bold"),
            axis.title.y = element_text(size=12,face = "bold"),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
    
    # Arrange the plots side by side using gridExtra
    grid.arrange(plot1, plot2, plot3, ncol = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
