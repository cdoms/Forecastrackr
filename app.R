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
nba <- nba[nba$Round == 1 | nba$Round == 4,]
nba <- nba[!nba$Site == "Vegas",]
mens <- mens[!mens$Site == "Vegas Spread",]

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

mens <- compute_brier(mens)
womens <- compute_brier(womens)
nba <- compute_brier(nba)

womens$sport <- c("womens", "womens")
mens$sport <- c("mens", "mens", "mens", "mens", "mens")
nba$sport <- c("nba", "nba", "nba")
all <- rbind(mens, womens, nba)
ui <- fluidPage(  
  
  titlePanel("Accuracy by sport", windowTitle = "Site Prediction Accuracy"),

     sidebarLayout(      
    
   sidebarPanel(
      selectInput("sport", "Select Sport:", choices = c("Men's Tourney", "Women's Tourney", "NBA Playoffs")),
      selected = "Men's",
      br(),
      p("These are average brier scores from the sites I tracked for the 2018 Men's and Women's NCAA Tournament. The first round and the Finals are also included for the NBA Playoffs."),
      p("Like golf, the lower the brier score the better.")
),
   
    # Create barplot
    mainPanel(
     plotOutput("brierPlot")
  )
)
)

server <- function(input, output) {
  
  # Fill in plot
  output$brierPlot <- renderPlot({
    data <- switch(input$sport,
                   "Men's Tourney" = all[all$sport == "mens",],
                   "Women's Tourney" = all[all$sport == "womens",],
                   "NBA Playoffs" = all[all$sport == "nba",])
    
    # barplot
    ggplot(data = data,
           aes(x = Site, y=Avg_Brier)) + geom_bar(stat = "identity", fill = "dodgerblue3") + 
      theme_bw() + 
      theme(axis.line = element_line(colour = "gray"),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_y_continuous(limits=c(0,0.25))
    
  })
}

shinyApp(ui, server)
