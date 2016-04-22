library(shiny)

#Load data
load("applicants.RData")

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Watson-Glaser interactive norms"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        
        sliderInput("cand", "Candidate theta:",
                    min = -3, max = 3, value = 0,
                    step = 0.1),
        
        selectInput("occ", "Occupation:", 
                    choices=c("All", levels(applicants$Occupation)[-1]),
                    selected = "All"),
        
        textInput("title", "Job title:", value = ""),
        
        checkboxGroupInput("edu", "Education:",
                    choices=c("All", levels(applicants$Education)),
                    selected = "All"),
        
        hr(),
        helpText("Select reference group using choices above")
      ),
      
      # Create a spot for the density plot
      mainPanel(
        plotOutput("densityPlot")  
      )
      
    )
  )
)