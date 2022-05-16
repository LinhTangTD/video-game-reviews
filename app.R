#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# -------------------------
#Package Loading
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shiny)
#library(purrr)
library(ggplot2)
library(plotly)
library(corrplot)
library(RColorBrewer)
require(lubridate)
library(shinythemes)
library(randomForest)
library(xgboost)
library(leaps)
library(caret)
library(pls)

# -------------------------
#load data
games <- read.csv("clean_data_model.csv")
codebook <- read.csv("codebook.csv")
# -------------------------

#Visualization Preparation that are mainly adopted from EDA done previously 
#Popular Genres
#Convert release date to year
games$year_released = year(as.character(games$release_date))
genres = games %>% select(year_released, starts_with("genres_"))

#Genres type by frequency, sorted in descending order
sort_genres = data.frame(colSums(genres[2:ncol(genres)])) %>% rownames_to_column()  %>% rename(count = colSums.genres.2.ncol.genres..., genre_type = rowname) %>% arrange(-count)

#Get the top 20 most frequent genres
top20_genres = sort_genres[1:20,]

#Group top 20 by year and summarize the count
top20_genres_by_year = genres %>% select(year_released, top20_genres$genre_type) %>% group_by(year_released) %>% summarise(across(everything(), sum)) 

#Tidy data for plotly
top20_genres_by_year_tidy_count = top20_genres_by_year %>% gather(key = genre_type, value = count, 2:21)
top20_genres_by_year_tidy_count$genre_type = gsub("genres_", "", top20_genres_by_year_tidy_count$genre_type) #clean type name

#Top 20 by percent
total_games_by_year = games %>% count(year_released) # calculate total number of games released per year
top20_genres_by_year_tidy_percent<- top20_genres_by_year %>% left_join(total_games_by_year, by="year_released")
top20_genres_by_year_tidy_percent<- top20_genres_by_year_tidy_percent %>% mutate(across(starts_with("genres_"), .fns = ~./n * 100))

top20_genres_by_year_tidy_percent = top20_genres_by_year_tidy_percent %>% select(-n) %>% gather(key = genre_type, value = percent, starts_with("genres_"))
top20_genres_by_year_tidy_percent$genre_type = gsub("genres_", "", top20_genres_by_year_tidy_percent$genre_type) #clean type name

#ESRB Rating Trends across year
ESRB = games %>% count(year_released, esrb_ratings) %>% na.omit()
ESRB_by_year = ESRB %>% group_by(year_released) %>% summarise(sum(n)) %>% rename(total_by_year = `sum(n)`)
ESRB = ESRB %>% left_join(ESRB_by_year, by="year_released")
ESRB$percent = ESRB$n / ESRB$total_by_year
ESRB = ESRB %>% select(-n, -total_by_year)
#Colors
colors_rsrb_rating = list(
    "E" = "#FF99FF",
    "E10+" = "#FFCCFF",
    "M" = "#CC99FF",
    "T" = "#99CCFF",
    "AO" = "#6699CC",
    "K-A" = "#9999FF",
    "RP" = "#003399"
)
ESRB$color = dplyr::recode(ESRB$esrb_ratings, !!!colors_rsrb_rating)

#Trends of ESRB content descriptions over years
esrb_content = games %>% select(year_released, starts_with("esrb_descs_"))

#Group by year and summarize the count
esrb_content_by_year = esrb_content %>% group_by(year_released) %>% summarise(across(everything(), sum)) 

#Tidy data for plotly
esrb_content_by_year_tidy_count = esrb_content_by_year %>% gather(key = esrb_content, value = count, 2:8)
esrb_content_by_year_tidy_count$esrb_content = gsub("esrb_descs_", "", esrb_content_by_year_tidy_count$esrb_content) #clean type name
esrb_content_by_year_tidy_count = esrb_content_by_year_tidy_count[!esrb_content_by_year_tidy_count$esrb_content == "missing",] #drop missing values

#by percent
total_games_by_year = games %>% count(year_released) # calculate total number of games released per year
esrb_content_by_year_tidy_percent = esrb_content_by_year %>% left_join(total_games_by_year, by="year_released")
esrb_content_by_year_tidy_percent = esrb_content_by_year_tidy_percent %>% mutate(across(starts_with("esrb_descs_"), .fns = ~./n * 100))

#Tidy data for plotly
esrb_content_by_year_tidy_percent = esrb_content_by_year_tidy_percent %>% select(-n) %>% gather(key = esrb_content, value = percent, starts_with("esrb_descs_"))
esrb_content_by_year_tidy_percent$esrb_content = gsub("esrb_descs_", "", esrb_content_by_year_tidy_percent$esrb_content) #clean type name

#Set up for the model tab 
sort_genres2 = data.frame(colSums(genres[2:ncol(genres)])) %>% rownames_to_column()  %>% rename(count = colSums.genres.2.ncol.genres..., genre_type = rowname) %>% arrange(-count)
sort_genres2$genre_type = gsub("genres_", "", sort_genres$genre_type) #clean type name
genre_name <-unique (sort_genres2$genre_type)
genre_name_update <- list(genre_name, "None")

# --------- MODEL ----------------
df = read_csv("clean_data_model.csv", na = "missing")
df$is_same_dev_pub = df$developers == df$publishers
df = dplyr::select(df, -c(platform, developers, publishers, release_date, esrb_descs, allow_multiplayer, allow_online))
df$esrb_ratings = factor(df$esrb_ratings)
df$company = as.factor(df$company)
final_df = df[complete.cases(df),]
model_df = dplyr::select(final_df, -title)
names(model_df) <- make.names(names(model_df))
model_matrix = model.matrix(user_score ~ ., data = model_df)
rf.fit <- readRDS("rf_model_final.rds")

# --------- Preparation for the Model ----------------
#define a new data_frame such that it have 0 for all columns
new_df = as.data.frame(model_matrix)[0,]
for(i in 1:ncol(new_df)) {
    
    new_df[1,i] = 0
}

#define "today" such taht it can be used for calculating time differences later
today = as.Date(Sys.Date())

# --------- DEFINE THE FUNCTION ----------------

# define the function called  "prediction_input" such that it takes all input required to run the predication functiona and return a dataframe

prediction_input = function(x, y, z, year_since, company, genre1, genre2, genre3, esrb_desc1, esrb_desc2, esrb_desc3, esrb_rating, best_game_ornot, best_game_score, most_dis_ornot, most_dis_score, most_shared_ornot, most_shared_score){
    
    new_df = as.data.frame(model_matrix)[0,]
    for(i in 1:ncol(new_df)) {
        new_df[1,i] = 0}
    
    new_df$meta_reviews = x
    new_df$user_reviews = y
    new_df$meta_score = z
    
    new_df$years_since_released = as.numeric(year_since)
    
    if(company == "Nintendo"){new_df$companyNintendo = 1} 
    else {
        if(company == "PC"){new_df$companyPC = 1} else{
            if (company == "Sony"){new_df$companySony = 1} else{} }
    }#end of the else statements of company related input
    
    if(genre1 == "NA"){} else{
        if(genre1 == "Sim"){new_df$genres_Sim = 1} else{}
        if(genre1 == "Team"){new_df$genres_Team = 1} else{}
        if(genre1 == "Simulation"){new_df$genres_Simulation = 1} else{}
        if(genre1 == "Sports"){new_df$genres_Sports = 1} else{}
        if(genre1 == "Fist.Person"){new_df$genres_First.Person = 1} else{}
        if(genre1 == "Virtual.Life"){new_df$genres_Virtual.Life = 1} else{}
        if(genre1 == "Action.Adventure"){new_df$genres_Action.Adventure = 1} else {}
        if(genre1 == "Tactical"){new_df$genres_Tactical = 1} else{}
        if(genre1 == "Shooter"){new_df$genres_Shooter = 1} else {}
        if(genre1 == "Arcade"){new_df$genres_Arcade = 1}else{}
        if(genre2 == "NA"){} else{
            if(genre2 == "Sim"){new_df$genres_Sim = 1} else{}
            if(genre2 == "Team"){new_df$genres_Team = 1} else{}
            if(genre2 == "Simulation"){new_df$genres_Simulation = 1} else{}
            if(genre2 == "Sports"){new_df$genres_Sports = 1} else{}
            if(genre2 == "Fist.Person"){new_df$genres_First.Person = 1} else{}
            if(genre2 == "Virtual.Life"){new_df$genres_Virtual.Life = 1} else{}
            if(genre2 == "Action.Adventure"){new_df$genres_Action.Adventure = 1} else {}
            if(genre2 == "Tactical"){new_df$genres_Tactical = 1} else{}
            if(genre2 == "Shooter"){new_df$genres_Shooter = 1} else {}
            if(genre2 == "Arcade"){new_df$genres_Arcade = 1}else{}
            if(genre3 == "NA"){} else{
                if(genre3 == "Sim"){new_df$genres_Sim = 1} else{}
                if(genre3 == "Team"){new_df$genres_Team = 1} else{}
                if(genre3 == "Simulation"){new_df$genres_Simulation = 1} else{}
                if(genre3 == "Sports"){new_df$genres_Sports = 1} else{}
                if(genre3 == "Fist.Person"){new_df$genres_First.Person = 1} else{}
                if(genre3 == "Virtual.Life"){new_df$genres_Virtual.Life = 1} else{}
                if(genre3 == "Action.Adventure"){new_df$genres_Action.Adventure = 1} else {}
                if(genre3 == "Tactical"){new_df$genres_Tactical = 1} else{}
                if(genre3 == "Shooter"){new_df$genres_Shooter = 1} else {}
                if(genre3 == "Arcade"){new_df$genres_Arcade = 1}else{}}
        }}
    
    if(esrb_desc1 == "NA"){new_df$esrb_descs_missing = 1}else{
        if(esrb_desc1 == "Violence"){new_df$esrb_descs_Violence = 1} else{}
        if(esrb_desc1 == "Blood"){new_df$esrb_descs_Blood = 1} else{}
        if(esrb_desc2 == "NA"){new_df$esrb_descs_missing = 1}else{
            if(esrb_desc2 == "Violence"){new_df$esrb_descs_Violence = 1} else{}
            if(esrb_desc2 == "Blood"){new_df$esrb_descs_Blood = 1} else{} 
            if(esrb_desc3 == "NA"){new_df$esrb_descs_missing = 1} else{
                if(esrb_desc3 == "Violence"){new_df$esrb_descs_Violence = 1} else{}
                if(esrb_desc3 == "Blood"){new_df$esrb_descs_Blood = 1} else{} 
            }}}
    
    if(esrb_rating == "E"){new_df$esrb_ratingsE = 1} else{}
    
    if(best_game_ornot == "Yes"){new_df$best_game = as.numeric(best_game_score)} 
    else{new_df$best_game = 100}
    
    if(most_dis_ornot == "Yes"){new_df$most_discussed = as.numeric(most_dis_score)}
    else{new_df$most_discussed = 100}
    
    if(most_shared_ornot == "Yes"){new_df$most_shared = as.numeric(most_shared_score)}
    else{new_df$most_shared = 100}
    
    return(new_df)
}
# -------------------------

# Define UI for application
ui <- fluidPage(theme = shinytheme("cerulean"),
                #"cerulean" is the theme for the app selected from the shinytheme package
                navbarPage("Video Game Reviews",  #present the full name of this tab at the top left corner of the nav bar panel, call this app "Video Game Reviews" 
                           #Presenting EDA Results in the first tab
                           tabPanel("EDA",
                                    titlePanel("Exploratory Data Analysis"),
                                    #set up the sidebar panel such that user can choose the yearrange and specific animations they want to explore
                                    sidebarPanel(
                                        radioButtons("edachoice", "Please select which visualization you want to see: ",
                                                                   choices = list("Top 20 Genres of Game Trends Across Year (Percentage)", 
                                                                                  "ESRB Ratings Trends Across Year (Percentage)",
                                                                                  "ESRB Content Descriptors Trends Across Year (Percentage)"),
                                                                   selected = "Top 20 Genres of Game Trends Across Year (Percentage)"
                                                                   ),#end of checkboxGroup,
                                                p("Notice that for the ESRB Content Descriptors Trends Across Year (Percentage) visualization, there is no data after 2015" ),
                                                sliderInput ("yearrange", "Year Range:",
                                                             min = 1996, max = 2021,
                                                             value = c(1997, 2020)
                                                             )
                                             ),#end of sidebar panel
                                             mainPanel(
                                                 plotlyOutput("eda1"),#present the corresponding plotly output based on user's choice
                                             )#end of main panel
                                    ),#end of tab 1  
                           
                           #Present the Predication Model in the second tab
                           tabPanel("Model ",
                                    titlePanel("Predication Model"),
                                    #set up the sidebar panel such that it allow users to input different information about the game
                                    # the default choice are mainly picked based on the most frequently value in the original dataset
                                    sidebarPanel(
                                        selectizeInput("genre", "Genre(s) of the game (Please select the top three genres of the game, and ends you selection with None if the game have less than three genres):",
                                                    choices = c(genre_name, "None"), selected = c("Action", "None"), multiple = TRUE,
                                                    options = list(maxItems = 3)),#end of the genre input
                                        selectInput("esrb_rating", "ESRB Rating of the game: ",
                                                     choices = c("E","E10+","M","T","AO","K-A","RP")), #end of esrb rating input
                                        selectizeInput("esrb_desc","Descriptive Keyword(s) (Please select the top three genres of the game, and ends you selection with NA if the game have less than three ESRB Descriptors): ",
                                                     choices = c("Blood", "Gambling","Humor", "Nudity","Violence","NA"), selected = c("Violence", "NA"), multiple = TRUE,
                                                     options = list(maxItems = 3)),#end of the input description keyword
                                        selectInput("company", "Company of the game's platform:",
                                                     choices = c("PC","Ninendo", "Sony", "Sega", "Other"), selected = "Sony"), #end of company input
                                        numericInput("meta_score", "Meta Score: ", 
                                                     80, min = 0, max = 100),#end of the metascore input
                                        numericInput("meta_review", "Number of Meta Reviews: ", 
                                                     5, min = 0), max = 300000,#end of the input for  number of meta reviews
                                        numericInput("user_review", "Number of User Reviews: ", 
                                                     7, min = 0, max = 300000),#end of the input for number of user review
                                        dateInput ("date", "Released Date:",
                                                   value = "2006-11-14"), #end of released date input
                                        radioButtons("best_game_ornot", "Has this game been nominated/won the Best Game Award of the year?",
                                                     choices = list ("Yes", "No"), selected = "No"),# end of the input that indicates whether the game has been nominated/won the best game awards
                                        numericInput("best_game_score", "If the game has been nominated/won the Best Game Award of the year, then please put down the rank of the game(do not need to change the default if the game has not been nominated/won the Best Game Award)",
                                                     0, min = 0, max = 99),
                                        radioButtons("most_dis_ornot", "Has this game been nominated/won the Most Discussed Award of the year?",
                                                     choices = list ("Yes", "No"), selected = "No"),# end of the input that indicates whether the game has been nominated/won the best game awards
                                        numericInput("most_dis_score", "If the game has been nominated/won the Most Discussed Award of the year, then please put down the rank of the game(do not need to change the default if the game has not been nominated/won the Most Discussed Award)",
                                                     0, min = 0, max = 99),
                                        radioButtons("most_shared_ornot", "Has this game been nominated/won the Most Shared Award of the year?",
                                                     choices = list ("Yes", "No"), selected = "No"),# end of the input that indicates whether the game has been nominated/won the best game awards
                                        numericInput("most_shared_score", "If the game has been nominated/won the Most Shared Award of the year, then please put down the rank of the game(do not need to change the default if the game has not been nominated/won the Most Shared Award)",
                                                     0, min = 0, max = 99),
                                        , width = 6
                                         ),#end of sidebar panel 
                                    mainPanel (
                                        h3("Based on the information given about the game, the predicted user score of the game is :"),
                                        verbatimTextOutput("result"),
                                        , width = 6 #present the predicated user score in the main panel 
                                    )#end of main panel
                                    ), #end of tab panel-model
                           
                           #Create the third tab such that users can see more visulizations if they want to 
                           tabPanel("Data Visualizations",
                                    titlePanel("Additional Data Visualizations"),
                                    #set up the sidebar such that users can choose which variable they want to see for the historgram graph as well as set up pairs of variables for correlation plot
                                    sidebarPanel(
                                        selectInput ("var", "Please select the variable you want to see the histogram and summary statistics for:",
                                                     choices = c("user_score", 
                                                                 "user_reviews",
                                                                 "meta_score",
                                                                 "meta_reviews", 
                                                                 "best_game", 
                                                                 "most_discussed", 
                                                                 "most_shared", 
                                                                 "years_since_released"))#end of input var
                                        ),#end of sidebar panel
                                    mainPanel(
                                        #present the corresponding histogram and correlation plot based on user's choice in the main panel
                                        plotOutput("hist"),
                                        verbatimTextOutput("summary")#end of plot output-histogram
                                    )#end of main Panel
                                    ),#end of more visualization panel
                           
                           #create the 4th tab called "Codebook" such that user can get more information about the data
                           tabPanel("Codebook",
                                    titlePanel("Codebook"),
                                    mainPanel(
                                        dataTableOutput("codebook")
                                    )#end of main Panel 
                                    ),#end of codebook tab
                          
                           #create a separate tab for "Reference" to cite the source of raw data and creator of this app
                           tabPanel("Reference",
                                    titlePanel("References"),
                                    h3("Source of Raw Data :"),
                                    h4("From Dr. Sean McMannamy who scripted from the Metacritic website"),
                                    h3("Link to the Metacritic website:"),
                                    h4("https://www.metacritic.com"),
                                    h3("This app is created by:"),
                                    h4("Linh Tang, Britney He, and Jenny Li from Grinnell College"))#end of reference tab
                           
                ))#end of ui

# -------------------------
# Define server
server <- function(input, output) {
    #defining different reactive datasets here such that it is responsive to user's input and can be used for displaying final results/graphs 
    
    df_top20percent<- reactive({
        filter(top20_genres_by_year_tidy_percent, year_released <= input$yearrange[2] & year_released >= input$yearrange[1])
    })#end of reactive data frame-top20percent
    
    df_esrb <- reactive ({
        filter(ESRB, year_released <= input$yearrange[2] & year_released >= input$yearrange[1])
    })#end of the reactive data frame - ESRB
    
    df_esrbcontent <- reactive ({
        filter(esrb_content_by_year_tidy_percent, year_released <= input$yearrange[2] & year_released >= input$yearrange[1])
    })# end of reactive data frame-esrb content
    
    df_hist <- reactive({
        select(games, input$var)
    })# end of reactive data frame that will be used for histogram construction
    
    #df_cor <- reactive({
        #select(games, input$varlist)
    #})#end of the reactive data frame that will be used for correlation plot
    
    genre_converted <- reactive({
        if(input$genre[1] == "None"){c("NA", "NA", "NA")}
        else {if(input$genre[2] == "None"){c(input$genre[1], "NA", "NA")}
            else {if (input$genre[3] == "None"){c(input$genre[1], input$genre[2], "NA")} 
                else{c(input$genre[1], input$genre[2], input$genre[3])}}}
    })
    
    new_esrb_desc <- reactive({
        if(input$esrb_desc[1] == "NA"){c("NA", "NA", "NA")}
        else {if(input$esrb_desc[2] == "NA"){c(input$esrb_des[1], "NA", "NA")}
            else {if (input$esrb_desc[3] == "NA"){c(input$esrb_desc[1], input$esrb_desc[2], "NA")} 
                else{c(input$esrb_desc[1], input$esrb_desc[2], input$esrb_desc[3])}}}
    })
    
    input_df <- reactive({
        prediction_input(input$meta_review, input$user_review, input$meta_score, (time_length(difftime(today, input$date), "years")) , input$company, genre_converted()[1], genre_converted()[2],genre_converted()[3],
                         new_esrb_desc()[1], new_esrb_desc()[2], new_esrb_desc()[3], input$esrb_rating, input$best_game_ornot, input$best_game_score, input$most_dis_ornot, input$most_dis_score, input$most_shared_ornot, input$most_shared_score)
    })
    
    # define different output items such that it takes earlier defined reactive dataset into account and produce the ideal output such as plotly animations, data table, etc. 
    output$eda1<-renderPlotly({
        if(input$edachoice =="Top 20 Genres of Game Trends Across Year (Percentage)"){
            eda1 <- plot_ly(df_top20percent(), type = "bar",
                    x = ~fct_reorder(genre_type, percent), y = ~percent,
                    frame = ~year_released, showlegend = FALSE)
            #eda1 <- eda1 %>% animation_button(hide = T)#remove the animation button
            eda1
                    }#end of eda choice 1
            else {
                if(input$edachoice == "ESRB Ratings Trends Across Year (Percentage)"){
                    eda2 <- plot_ly(df_esrb(), type = "pie",labels = ~esrb_ratings, values = ~percent, frame = ~year_released,
                            textinfo = 'label+percent', marker = list( colors = ~color), sort = FALSE)
                    #eda2 <- eda2 %>% animation_button(hide = T)#remove the animation button
                    eda2
                }#end of eda choice 2
                else {
                    if (input$edachoice == "ESRB Content Descriptors Trends Across Year (Percentage)"){
                        eda3 <- plot_ly(df_esrbcontent(),
                                type = "bar", x = ~fct_reorder(esrb_content, percent),
                                y = ~percent, frame = ~year_released, showlegend = FALSE) 
                        #eda3 <- eda3 %>% animation_button(hide = T)#remove the animation button
                        eda3
                    }#end of choice 3
                    else{}
                }# end of else on 225
            }#end of else on 221
            })#end of the output "eda"
    
    output$hist <-renderPlot({
        hist(df_hist()[,1], main = "Histogram of the Selected Variable", 
             xlab = "Selected Variable", col="#4EA8CB")
    })# end of the output "histogram"
    
    output$summary <-renderPrint({
        summary(df_hist()[,1])
    })
    
    output$result <-renderPrint({
        result = predict(rf.fit, input_df())
        dd<- as.data.frame(result)
        dd[1,]
    })#end of the output "result"
    
    output$codebook <-renderDataTable({
        output_codebook <- codebook
        output_codebook
    })#end of the output "codebook"
    
}#end of server

# -------------------------
# Run the application 
shinyApp(ui = ui, server = server)
