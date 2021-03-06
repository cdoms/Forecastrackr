library(shiny)
library(readr)
library(plyr)
library(ggplot2)
library(scoring)
library(data.table)

## first step: retrieve data
mens <- read_csv("https://raw.githubusercontent.com/cdoms/ShinyTrack/master/Mens%20NCAA%20Tourney%20Probabilities%202018.csv")
womens <- read_csv("https://raw.githubusercontent.com/cdoms/ShinyTrack/master/Womens%20NCAA%20Tourney%20Probabilities%202018.csv")
nba <- read_csv("https://raw.githubusercontent.com/cdoms/ShinyTrack/master/NBA%20Playoffs%20Probabilities.csv")
wc <- read_csv("https://raw.githubusercontent.com/cdoms/Forecastrackr/master/World%20Cup%20Probabilities.csv")

colnames(nba)[8] <- "Result"
nba <- nba[nba$Round == 1 | nba$Round == 4,]
nba <- nba[!nba$Site == "Vegas",]
mens <- mens[!mens$Site == "Vegas Spread",]

wc <- wc[-11]
wc <- wc[!wc$Site == "NumberFire" & !wc$Site == "Bing",]
wc <- wc[complete.cases(wc),]

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

## different for world cup because there are three possible outcomes

wc$brier <- ifelse(wc$Results == 1,
                   (1 - wc$`Probability First Team Wins`)^2 + (0 - wc$`Probability Tie`)^2 + (0-wc$`Probability Second Team Wins`)^2,
                   ifelse(wc$Results == 0.5,
                          (0 - wc$`Probability First Team Wins`)^2 + (1 - wc$`Probability Tie`)^2 + (0-wc$`Probability Second Team Wins`)^2, 
                          (0 - wc$`Probability First Team Wins`)^2 + (0 - wc$`Probability Tie`)^2 + (1-wc$`Probability Second Team Wins`)^2)  
)

womens$sport <- c("womens", "womens")
mens$sport <- c("mens", "mens", "mens", "mens", "mens")
nba$sport <- c("nba", "nba", "nba")



all <- rbind(mens, womens, nba)
all$Site <- mapvalues(all$Site, 
                       from = "FiveThirtyEight", to = "538")
ui <- fluidPage(  
  
  titlePanel("Accuracy by sport", windowTitle = "Forecastrackr"),
    
   sidebarPanel(
      selectInput("sport", "Select Sport:", choices = c("Men's Tourney", "Women's Tourney", "NBA Playoffs")),
      selected = "Men's",
      p("These are average brier scores from the sites I tracked for the 2018 Men's and Women's NCAA Tournament. The first round and the Finals are also included for the NBA playoffs."),
      p("Like golf, the lower the brier score the better."),
      em("This site is for informational purposes only.")
),
   
    # Create barplot
    mainPanel(
     plotOutput("brierPlot"),
     br(),
     br(),
     fluidRow(
       column(10, align="center",
              tableOutput("table")
       )
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
           aes(x = reorder(Site, Avg_Brier), y=Avg_Brier)) + geom_bar(stat = "identity", fill = "dodgerblue3") + 
      theme_bw() + 
      theme(axis.line = element_line(colour = "gray"),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_y_continuous(limits=c(0,0.25))
    
  })
  
  output$table <- renderTable({
    data <- switch(input$sport,
                   "Men's Tourney" = all[all$sport == "mens",],
                   "Women's Tourney" = all[all$sport == "womens",],
                   "NBA Playoffs" = all[all$sport == "nba",])
    
    colnames(data)[2] <- "Score"
    data <- data[order(data$Score),]
   head(data[,1:2])
  })
  
}

shinyApp(ui, server)
