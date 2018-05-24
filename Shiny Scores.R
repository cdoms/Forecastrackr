library(shiny)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(scoring)
library(data.table)

## first step: retrieve data
mens <- read_csv("https://raw.githubusercontent.com/cdoms/ShinyTrack/master/Mens%20NCAA%20Tourney%20Probabilities%202018.csv")
womens <- read_csv("https://raw.githubusercontent.com/cdoms/ShinyTrack/master/Womens%20NCAA%20Tourney%20Probabilities%202018.csv")

## make sure there are 63 games per site and each row matches for round, team, and result

mens %>%
  count(Site)

womens %>%
  count(Site)

## exclude ESPN BPI for womens because I could only find their predictions for the first round

womens <- womens[womens$Site != "ESPN BPI",]

## respell Louisville because I misspelled it in the CSV

womens$`High Seed in Game` <- mapvalues(womens$`High Seed in Game`, 
                                              from = "Lousville", to = "Louisville")

mens <- brierscore(Result ~ `Probability High Seed Wins`,
           data = mens, 
           group = "Site")

mens <- as.data.frame(mens$brieravg)

mens <- setDT(mens, keep.rownames = TRUE)[]
colnames(mens)[1:2] <- c("Site", "Avg_Brier")
## get rid of Vegas for now
mens <- mens[1:5,]

womens <- brierscore(Result ~ `Probability High Seed Wins`,
                     data = womens, 
                     group = "Site")

womens <- as.data.frame(womens$brieravg)
womens <- setDT(womens, keep.rownames = TRUE)[]
colnames(womens)[1:2] <- c("Site", "Avg_Brier")
womens$tourney <- c("womens", "womens")
mens$tourney <- c("mens", "mens", "mens", "mens", "mens")
all <- rbind(mens, womens)
ui <- fluidPage(  
  
  # Give the page a title
  titlePanel("Accuracy by Tourney"),
  # # Generate a row with a sidebar
   sidebarLayout(      
    
  #   # Define the sidebar with one input
   sidebarPanel(
      selectInput("tourney", "Select Tourney:", choices = c("mens", "womens")),
      selected = "mens"
    ),
    
    # Create a spot for the barplot
    mainPanel(
     plotOutput("brierPlot")  
   )
    
  )
)
server <- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$brierPlot <- renderPlot({
    data <- switch(input$tourney,
                   "mens" = all[all$tourney == "mens",],
                   "womens" = all[all$tourney == "womens",])
    
    # Render a barplot
    ggplot(data = data,
           aes(x = Site, y=Avg_Brier)) + geom_bar(stat = "identity")
  })
}

shinyApp(ui, server)

