### Player data from 2004-2018.
### Source: https://www.kaggle.com/xavya77/nhl04to18
### Season standing data from 2004-2018.
#Each seasons standing data was downloaded to a csv file, cleaned and combined using "StandingsClean.R" and selected columns to be used
#in this program were selected and parsed creating a dataframe using "FinalStandingsData.R".
### Source: https://www.hockey-reference.com/leagues/NHL_2018_standings.html

##Data Notes:
# NHL Lockout cancelled 2005 season.
# NHL Lockout occurred during 2012-2013 season half a season was played.

#Importing needed libraries.
library(dplyr)
library(Hmisc)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)



###Importing data and arranging by season, team and player in alphabetical order.
NHLData <- read.csv("NHL 2004-2018 Player Data.csv")
NHLData <- NHLData %>%
  arrange(NHLData$Season, NHLData$Tm, NHLData$Player)
## MDA is Mighty Ducks of Anaheim, should I convert to ANA for consistency?
## Could also convert ATL to WPG and PHX to ARI, but is it better to showcase these teams separately?

#Renaming duplicate columns to distinguish between different types of goals and assists (Even Strength, Short-Handed, Powerplay).
colnames(NHLData)[13] <- "EVG"
colnames(NHLData)[14] <- "PPG"
colnames(NHLData)[15] <- "SHG"
colnames(NHLData)[17] <- "EVA"
colnames(NHLData)[18] <- "PPA"
colnames(NHLData)[19] <- "SHA"

#Splitting player name from last/first combination.
PlayerNames <- unlist(str_split_fixed(as.character(NHLData$Player),pattern="\\\\", 2))
NHLData$Player <- PlayerNames[,1]




#####Found duplicate rows in 2009 section of data, removing these rows.
DupRows <- NHLData %>%
  filter(Season == 2009)
#609 rows that need to be removed.
sum(duplicated(DupRows$Player))
#Rows we want to keep based off of correct player age in 2009.
keeprows <- aggregate(Age ~ Player, DupRows, max)
#Rows we need to remove.
remrows <- anti_join(DupRows, keeprows, by = c("Player", "Age"))
#Rewriting NHLData removing the rows that are duplicates.
NHLData <- anti_join(NHLData, remrows)







#Creating dataframe of NHL team statistics from aggregated player statistics.
#Showcasing average player age, total goals, total assists, points per season, total PIM, total shots, total hits, total blocks.
NHLTeams <- NHLData %>%
  group_by(Tm, Season) %>%
  summarise(AvgAge = round(mean(Age), 2), Goals = sum(G), Assists = sum(A), PlayerPoints = sum(PTS), TotalPIM = sum(PIM),
            TotalShots = sum(S), TotalHIT = sum(HIT), TotalBLK = sum(BLK), Player_Count = n())

#Reading in standings and dictionary files and combining with NHL Team Data.
NHLStandings <- read.csv("NHLStandings_2004_2018.csv")
TeamDict <- read.csv("TeamDictionary.csv")

#Creating dataframe with team metrics as well as win-loss-overtimeloss records per each season.
TeamStandings <- merge(NHLTeams, NHLStandings, by=c("Tm","Season"))
TeamStandings <- merge(TeamStandings, TeamDict, by="Tm")

#Adding season point totals to team standings data.
TeamStandings$Points <- (1*TeamStandings$OTL)+(2*TeamStandings$Win)








#Creating function which will create binary flag of Stanley Cup winning team each season in NHLTeams dataframe.
#This will also be applied to the NHLData dataframe so that individual players will have a count of stanley cups won.
CupTeams <- function(Tm, Season){
  if(Tm == "TBL" & Season == 2004){
    return(1)
  }else if(Tm == "CAR" & Season == 2006){
    return(1)
  }else if(Tm == "ANA" & Season == 2007){
    return(1)
  }else if(Tm == "DET" & Season == 2008){
    return(1)
  }else if(Tm == "PIT" & (Season == 2009 | Season == 2016)){
    return(1)
  }else if(Tm == "CHI" & (Season == 2010 | Season ==  2013 | Season == 2015)){
    return(1)
  }else if(Tm == "BOS" & Season == 2011){
    return(1)
  }else if(Tm == "LAK" & (Season == 2012 | Season == 2014)){
    return(1)
  }else if(Tm == "WSH" & Season == 2017){
    return(1)
  }else return(0)
}

#Vectorizing function, then applying function to create column in dataframes.
SCup <- Vectorize(CupTeams)
NHLTeams$StanleyCup <- SCup(NHLTeams$Tm, NHLTeams$Season)
NHLData$StanleyCup <- SCup(NHLData$Tm, NHLData$Season)







#Creating a color table for team trend lines.
Colors = c("ATL" = "steelblue2", "BOS" = "gold", "BUF" = "navyblue", "CAR" = "red3", "CBJ" = "blueviolet","CGY" = "orangered2", "CHI" = "brown3", "COL" = "violetred4", "DAL" =" springgreen4",
          "DET" = "firebrick3", "EDM" = "darkslateblue", "FLA" = "indianred4", "LAK" = "gray15", "MDA" = "green4", "MIN" = "forestgreen", "MTL" = "darksalmon", "NJD" = "red", "NSH" = "goldenrod2", "NYI" = "orange",
          "NYR" = "royalblue1", "OTT" = "tan3", "PHI" = "darkorange", "PHX" = "navajowhite1", "PIT" = "grey45", "SJS" = "cyan4", "STL" = "midnightblue", "TBL" = "blue4", "TOR" = "dodgerblue", "TOT" = "gray89", "VAN" = "darkblue",
          "WSH" = "red", "ANA" = "sandybrown", "WPG" = "gray65", "ARI" = "indianred4", "VEG" = "yellow3")







##### Testing Shiny App
## Helpful tutorial http://stcorp.nl/R_course/tutorial_shiny.html

#Creation of UI.
ui <- fluidPage(
  #App Title ---
  h1(headerPanel(div(strong("NHL Stats"), style="color: orange;"))),
  
  
  #Creating various tabs.
  tabsetPanel(
    
    
    ### First tab for comparison of NHL Teams.
    tabPanel("Team Trends", fluid = TRUE, br(),
      
      #Sidebar panel for inputs on first tab ---
      sidebarPanel(#Selecting Team
                   pickerInput("team_name","Select Team", choices= unique(as.character(TeamStandings$TeamName)),
                               options = list(`actions-box` = TRUE),multiple = T, selected = "Anaheim Ducks"),
                   
                   #Selecting characteristic to plot team trends over time.
                   selectInput('team_char', label = "Characteristic",
                               choices = c("Average Player Age", "Goals", "Assists", "Player Points", "Wins", "Losses",
                                           "Overtime Losses", "Points")),
                   #Width of sidebar.
                   width = 3),
      
      
      
      #Main panel for displaying outputs on first tab ---
      mainPanel(
        #Explanation of ui of first tab.
        strong("Select team(s) for trend line showcasing changes in selected characteristic throughout the last 14 seasons of the National
               Hockey League."), br(), br(),

        
        #Adding plot output to main panel on first tab.
        (plotOutput('dynamicPlot', height = 600, width = 600)))
      ),
    
    
    
    
    ###Second tab for player statistics and comparison with their teammates.
    tabPanel("Player Statistics", fluid = TRUE, br(),
      
      #Sidebar panel for inputs on second tab ---
      sidebarPanel(
          #First input, Selecting Season.
          selectInput('season_select', label = "Select Season", choices = unique(NHLData$Season)),
          
          #Second input, Selecting Team.
          selectInput('team_select', label = 'Select Team', choices = NULL),
                   
          #Third input, Selecting Player from team.
          selectInput('player_select', label = "Select Player", choices = NULL),
          
          #Fourth input, radio button metric to compare selected player against their teammates.
          radioButtons('metric_select', label = "Team Comparison",
                      choices = c("Goals", "Assists", "Points", "Plus/Minus", "Penalty Minutes", "Shots", "Shooting Percentage", "Average Time on Ice",
                                  "Blocked Shots", "Hits")),
          strong("Note:"), "Hit and blocked shot data not available until the 2008 season."
      ),
      
      
      
      
      #Main panel showing table of player statistics and barplot comparing against teammates.
      mainPanel(
        tableOutput('player_table'),
        
        plotOutput('TeamBar', height = 550)
      )
    ),
    
    
    
    
    
    ### Third tab for readme file and project information.
    tabPanel("ReadMe", fluid = TRUE, br(),
             
             #Main panel consisting of readme information.
             #This may not be the best way to do this.
             mainPanel(
               strong("This tab outlines the details of this ShinyApp as well as information regarding the data and
                      creation of this project."),br(),br(),
               "The data used in this project was collected from", a("Kaggle", href="https://www.kaggle.com/xavya77/nhl04to18"),
               ". This data outlines individual player statistics from the National Hockey League from the 2004-2018 seasons.",br(),br(),
               
               "Additional data was then collected from", a("Hockey-Reference.com", href="https://www.hockey-reference.com/leagues/NHL_2018_standings.html"),
               ". This data was regarding the team standing data for the corresponding years (2004-2018). Data was cleaned and added
               to a dataframe of aggregated player statistics representative of team statistics for each NHL team throughout the last
               14 seasons.", br(), br(),
               
               strong("App Purpose:"), "This app was created to more easily provide a comparison between National Hockey League teams using
               various leauge statistics.", br(),
               "This comparison can also be made on the player level between a player and their teammates for that specific season.",
               br(), br(), br(), br(),
               
               strong("IMPORTANT NOTES:"),br(),
               "- The 2005 NHL season was cancelled due to a lockout, therefore there is no data reported for this season.",br(),
               "- The 2013 NHL season was shortened due to a lockout. This season was shortened to 48 games.",br(),
               "- The kaggle data set collected does not have any recorded data for hits or blocked shots until the 2008 season.", br(),
               "- 'TOT' for team name in this data set represents players who have been traded through the duration of a season.
               These players' statistics are not accounted for in the team comparisson plot tab and they are not represented in the
               output comparing against their teammates for that season on the player statistics tab."
               
               )
    )
  )
)





#Creating server for shinyapp.
server <- function(input, output, session){
  
  #Characteristics for first tab team line graph.
  char = reactive({
    switch(input$team_char,
      "Average Player Age" = "AvgAge",
      "Goals" = "Goals",
      "Assists" = "Assists",
      "Player Points" = "PlayerPoints",
      "Wins" = "Win",
      "Losses" = "Loss",
      "Overtime Losses" = "OTL",
      "Points" = "Points")
  })
  
  
  
  #Characteristics for second tab team bar plot.
  teammet = reactive({
    switch(input$metric_select,
      "Goals" = "G",
      "Assists" = "A",
      "Points" = "PTS",
      "Plus/Minus" = "plusminus",
      "Penalty Minutes" = "PIM",
      "Shots" = "S",
      "Shooting Percentage" = "S_percent",
      "Average Time on Ice" = "ATOI",
      "Blocked Shots" = "BLK",
      "Hits" = "HIT"
    )
  })

  
  
  
  #Creating line graph for team statistics on first tab.
  output$dynamicPlot <- renderPlot({
    #Creating data filter for inputs of team selection.
    Team <- TeamStandings %>%
      filter(TeamName %in% input$team_name)
    
    #Creating plot of characteristic selection for each team as a line, with points representing values for each season.
    ggplot(Team, aes(x = Season, y = Team[, char()], color=Tm), height = 500, width = 700) + geom_line(size = 1.2) + geom_point() +
      scale_y_continuous(limits = c(min(TeamStandings[, char()]), max(TeamStandings[, char()]))) + scale_color_manual(name = "Team", values = Colors) +
      scale_x_continuous(limits = c(2004, 2018), breaks = seq(2004, 2018, by=1))+
      xlab("Season") + ylab(input$team_char) + theme(text = element_text(size=12), axis.text.x = element_text(angle=90, hjust=1))
    })
  
  
  
  
  #Creating observation of events of selectors on different tabs.
  #This allows the graphs to change depending on what is selected for each input.
  
  ### For tab 2.
    #Observing season selection to narrow teams which played in that season.
    observeEvent(input$season_select,{
      updateSelectInput(session, 'team_select', choices = unique(NHLData$Tm[NHLData$Season == input$season_select]))
    })
  
    #Observing team selection to narrow players which played on that team during that season.
    observeEvent(input$team_select,{
      updateSelectInput(session,'player_select',
                        choices=unique(NHLData$Player[NHLData$Tm== input$team_select & NHLData$Season == input$season_select]))
    })
    
    
    
    
    #Creating outputs: player data table and team bargraph for corresponding statistic selected on fourth input of second tab.
    
    #Player Data table.
    output$player_table <- renderTable(
      Playstats <- NHLData %>%
        filter(Season == input$season_select, Tm == input$team_select, Player == input$player_select) %>%
        select(Player, Age, Pos, Tm, GP, G, A, PTS, plusminus, PIM, S, S_percent, ATOI))
    
    
    #Player vs Team bargraph.
    output$TeamBar <- renderPlot({
      #Filtering data used in plot to corresponding season year and team selected.
      TeamSelection <- NHLData %>%
        filter(Season == input$season_select, Tm == input$team_select) %>%
        mutate(thisplayer = ifelse(Player == input$player_select, 'yes', 'no'))
      
      
    #Features for barplot for players on selected team and metric selected in the fourth input select on the second tab.
    ggplot(TeamSelection, aes(x = reorder(Player, -TeamSelection[, teammet()]), y = TeamSelection[, teammet()], fill = thisplayer)) +
      geom_bar(stat = 'sum') +
      theme(text = element_text(size=16),axis.text.x = element_text(angle=90, hjust=1, vjust = 0.2), legend.position = "none") +
      scale_y_continuous(limits = c(min(TeamSelection[, teammet()]), max(TeamSelection[, teammet()]))) +
      xlab("Player Name") + ylab(input$metric_select) +
      scale_fill_manual(values=c('yes' = 'red', 'no' = 'black'), guide = FALSE)
    
  })
}

#Running this line runs ShinyApp.
shinyApp(ui, server)
