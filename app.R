#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load all needed packages
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
#library(geojsonio)
#library(leaflet)

#load raw data
setwd('/Users/linhtang/Desktop/STA-395/Code')
games <- read.csv("clean_data/clean_data_model.csv")

#Visualization Preparation
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

#esrb_content_by_year_tidy_count2 = esrb_content_by_year %>% gather(key = esrb_content, value = count, 2:8)
#esrb_content_by_year_tidy_count2$esrb_content = gsub("esrb_descs_", "", esrb_content_by_year_tidy_count2$esrb_content)
#content_name <-unique (esrb_content_by_year_tidy_count2$esrb_content)


# --------- MODEL ----------------

df = read_csv("clean_data/clean_data_model.csv", na = "missing")
df$is_same_dev_pub = df$developers == df$publishers
df = dplyr::select(df, -c(platform, developers, publishers, release_date, esrb_descs, allow_multiplayer, allow_online))
df$esrb_ratings = factor(df$esrb_ratings)
df$company = as.factor(df$company)
final_df = df[complete.cases(df),]
model_df = dplyr::select(final_df, -title)
names(model_df) <- make.names(names(model_df))
model_matrix = model.matrix(user_score ~ ., data = model_df)
rf.fit <- readRDS("./rf_model_final.rds")
# -------------------------
# Define UI for application
ui <- fluidPage(theme = shinytheme("cerulean"),
                #"united" is the theme for the app selected from the shinytheme package
                navbarPage("Video Game Reviews",
                           #Presenting EDA Results in the first tab
                           tabPanel("EDA",
                                    titlePanel("Exploratory Data Analysis"),
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
                                                 plotlyOutput("eda1"),
                                             )#end of main panel
                                    ),#end of tab 1  
                           
                           tabPanel("Model (Tentative) ",
                                    titlePanel("(Tentative) Predication Model"),
                                    sidebarPanel(
                                        selectInput("Genre", "Genre of the game:",
                                                    choices = c(genre_name)),#end of genre input
                                        selectInput("esrbrating", "ESRB Rating of the game: ",
                                                     choices = c("E","E10+","M","T","AO","K-A","RP")), #end of esrb rating input
                                        selectInput("desc1","Description Keyword 1: ",
                                                     choices = c("Blood", "Gambling","Humor", "Nudity","Violence","NA")),#end of description keyword 1
                                        selectInput("desc2", "Description Keyword 2: ",
                                                     choices = c("Blood", "Gambling","Humor", "Nudity","Violence","NA")), #end of description keyword 2
                                        selectInput("desc3", "Description Keyword 3: ",
                                                     choices = c("Blood", "Gambling","Humor", "Nudity","Violence","NA")), #end of description keyword 3
                                        selectInput("company", "Company of the platform the game is on: ",
                                                     choices = c("PC","Ninendo", "Sony")), #end of esrb rating input
                                        selectInput("esrbrating", "ESRB Rating of the game: ",
                                                     choices = c("E","E10+","M","T","AO","K-A","RP")), #end of esrb rating input
                                        numericInput("metascore", "Meta Score: ", 
                                                     90, min = 0, max = 100),#end of metascore input
                                        numericInput("metascore", "Number of Meta Reviews: ", 
                                                     90, min = 0, max = 100),#end of number of meta reviews
                                        numericInput("metascore", "Number of User Reviews: ", 
                                                     90, min = 0, max = 100),#end of number of meta reviews
                                        numericInput ("year", "Released Year:",
                                                      2019), #end of released year input
                                    ),#end of sidebar panel 
                                    mainPanel (
                                        verbatimTextOutput("result")
                                    )#end of main panel
                                    ), #end of tab panel-model
                           
                           tabPanel("More Visualizations",
                                    titlePanel("Other Visualizations"),
                                    sidebarPanel(
                                        selectInput ("var", "Variables:",
                                                     choices = c("user_score", 
                                                                 "user_reviews",
                                                                 "meta_score",
                                                                 "meta_reviews", 
                                                                 "best_game", 
                                                                 "most_discussed", 
                                                                 "most_shared", 
                                                                 "years_since_released")),#end of input var
                                        checkboxGroupInput("varlist", "Numeric variables that you want include in the correlation plot 
                                                           (please select at least two variables to see the correlation plot): ",
                                                           choices = list("user_score", 
                                                                          "user_reviews",
                                                                          "meta_score",
                                                                          "meta_reviews", 
                                                                          "best_game", 
                                                                          "most_discussed", 
                                                                          "most_shared", 
                                                                          "years_since_released"), 
                                                           selected = c("user_score", "meta_score"))),#end of sidebar panel
                                    mainPanel(
                                        plotOutput("hist"),#end of plot output-histogram
                                        plotOutput("cor")#end of correlation output
                                    )#end of main Panel
                                    ),#end of more visualization panel
                           
                           tabPanel("Codebook",
                                    titlePanel("Codebook"),
                                    sidebarPanel(
                                        selectInput("var_code", "Variable:",
                                                    choices = c("1","2")),#end of var_code input
                                    ),#end of sidebarpanel
                                    mainPanel(
                                        verbatimTextOutput("varexplanation")
                                    )#end of main Panel 
                                    ),#end of codebook tab
                           
                           tabPanel("Reference",
                                    titlePanel("References"),
                                    h3("Source of Raw Data :"),
                                    h4("From Dr. Sean McMannamy who scripted from the Metacritic website"),
                                    h3("Link to the Metacritic website:"),
                                    h4("https://www.metacritic.com"),
                                    h3("This app is created by:"),
                                    h4("Linh Tang, Britney He, and Jenny Li from Grinnell College"))#end of reference tab
                           
                ))#end of ui

# Define server
server <- function(input, output) {
    
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
    
    df_cor <- reactive({
        select(games, input$varlist)
    })#end of the reactive data frame that will be used for correlation plot
    
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
            })#end of eda
    
    output$hist <-renderPlot({
        hist(df_hist()[,1], main = "Histogram of the Selected Variable", 
             xlab = "Selected Variable", col="#4EA8CB")
    })# end of histogram 
    
    output$cor <-renderPlot({
        M<-cor(df_cor())
        corrplot(M, type="upper", order="hclust",
                 col=brewer.pal(n=8, name="RdYlBu")
                 #, title = "Correlation Plot of the Selected Variables"
                 )
    })#end of correlation plot
    
    output$result <-renderPrint({
        "We will display predicted score here"
    })#end of result 
    
    output$varexplanation <-renderPrint({
        "We will display the corresponding variable explanation here"
    })#end of varexplanation
    
}#end of server

# Run the application 
shinyApp(ui = ui, server = server)
