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
nba <- read_csv("https://raw.githubusercontent.com/cdoms/ShinyTrack/master/NBA%20Playoffs%20Probabilities.csv")

colnames(nba)[8] <- "Result"
nba <- nba[nba$Round == 1,]
nba <- nba[!nba$Site == "Vegas",]
mens <- mens[!mens$Site == "Vegas",]

## exclude ESPN BPI for womens because I could only find their predictions for the first round

womens <- womens[womens$Site != "ESPN BPI",]

## respell Louisville because I misspelled it in the CSV

womens$`High Seed in Game` <- mapvalues(womens$`High Seed in Game`, 
                                              from = "Lousville", to = "Louisville")

## function for computing average brier

compute_brier <- function(df){
  df <- brierscore(Result ~ `Probability High Seed Wins`,
                     data = df, 
                     group = "Site")
  df <- as.data.frame(df$brieravg)
  df <- setDT(df, keep.rownames = TRUE)[]
  colnames(df)[1:2] <- c("Site", "Avg_Brier")
  return(df)
}

df_list <- list(mens, womens, nba)



womens$sport <- c("womens", "womens")
mens$sport <- c("mens", "mens", "mens", "mens", "mens")
nba$sport <- c("nba", "nba", "nba")
all <- rbind(mens, womens, nba)
ui <- fluidPage(  
  
  # Give the page a title
  titlePanel("Accuracy by Sport"),
  # # Generate a row with a sidebar
   sidebarLayout(      
    
  #   # Define the sidebar with one input
   sidebarPanel(
      selectInput("sport", "Select Sport:", choices = c("Men's", "Women's", "NBA")),
      selected = "Men's"
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
    data <- switch(input$sport,
                   "Men's" = all[all$sport == "mens",],
                   "Women's" = all[all$sport == "womens",],
                   "NBA" = all[all$sport == "nba",])
    
    # Render a barplot
    ggplot(data = data,
           aes(x = Site, y=Avg_Brier)) + geom_bar(stat = "identity", fill = "lightslategray") + 
      xlab("Website") + ylab("Avg. Brier Score") +
      theme_bw() + 
      theme(axis.line = element_line(colour = "gray"),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      scale_y_continuous(limits=c(0,0.25))
  })
}

shinyApp(ui, server)

