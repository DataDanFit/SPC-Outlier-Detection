library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ## Data ----
    #dataset <- read.csv("spc_dataset.csv", stringsAsFactors = FALSE)
    dataset$Date <- as.Date(dataset$Date)
    
})
