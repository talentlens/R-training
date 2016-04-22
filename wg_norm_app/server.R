library(shiny)
library(ggplot2)

# Load data
load("applicants.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  
  # Fill in the spot we created for a plot
  output$densityPlot <- renderPlot({
    
    # Subset data
    sub_occ <- applicants$Occupation %in% input$occ
    if(any(input$occ == "All")) sub_occ <- rep(TRUE, nrow(applicants))
    
    sub_edu <- applicants$Education %in% input$edu
    if(any(input$edu == "All")) sub_edu <- rep(TRUE, nrow(applicants))
    
    sub_title <- grepl(input$title, applicants$JobTitle, ignore.case = TRUE)
    if(input$title == "") sub_title <- rep(TRUE, nrow(applicants))
    
    sub <- sub_occ & sub_edu & sub_title 
    
    # Calculate mean, sd and density at candidate theta
    mu <- mean(applicants[sub, "wgcta.theta.score"])
    sigma <- sd(applicants[sub, "wgcta.theta.score"])
    d <- dnorm(input$cand, mean = mu, sd = sigma)
    
    # Create plot
    p <- ggplot(applicants[sub, ], aes(x = wgcta.theta.score)) + 
      scale_x_continuous(limits = c(-4, 4))
    
    # Render a histogram
    p <- p + geom_histogram(aes(y = ..density..), bins = 30, fill = "grey")  

    #Add candidate theta line
    p <- p + geom_segment(aes(x = input$cand, xend = input$cand, 
                              y = 0, yend = d),
                          size = 1, colour = "blue") 
    
    #Add shaded area under the curve
    shade <- data.frame(x = c(input$cand, seq(-4, input$cand, 0.1), input$cand),
                        y = c(0, dnorm(seq(-4, input$cand, 0.1), 
                                       mean = mu, sd = sigma), 0))
    p <- p + geom_polygon(data = shade, aes(x,y, fill = "blue"), alpha = 0.3) +
      scale_fill_identity(name = "Percentile", guide = "legend",
                          labels = round(pnorm(input$cand, mean = mu, sd = sigma) * 100, 0))
    
    #Add density function
    p <- p + stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                           colour = "blue", size = 1)
    
    #Add labels and print
    p + labs(title = paste0("Occupation: ", paste(input$occ, collapse=" & "), "\n",
                            "Education: ", paste(input$edu, collapse=" & "), "\n",
                            "Job title: ", input$title,
                            "\n(n=", sum(sub),")"),
             x = "Theta", y = "Density")

  })
})