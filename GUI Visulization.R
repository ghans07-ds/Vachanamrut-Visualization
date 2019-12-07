library(shiny)
library(shinythemes)
library(DBI)
library(RSQLite)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(stringi)
library(tm)
library(xfun)
library(readr)
library(wordcloud)
library(wordcloud2)
library(plotly)
library(qcc)

###Set Your Folder Path Here
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

con <- dbConnect(RSQLite::SQLite(),"Granth DB/granth.db")
Vtable <- dbReadTable(con,"VachanamrutGranth")
dbDisconnect(con)


choice <- unique(na.omit(Vtable$Gam))
season <- unique(na.omit(Vtable$Season6))
season3 <- unique(na.omit(Vtable$Season3))
festival <- unique(na.omit(Vtable$Utsav))


ui <- fluidPage(
  theme = shinytheme("paper"),
  #shinythemes::themeSelector(),
  # Application title
  titlePanel("Vachnamrut Visulization"),

    # Show a plot of the generated distribution
      tabsetPanel(
        tabPanel("Important Words",
          tabsetPanel(
            tabPanel("WordCloud",
                     sidebarPanel(
                       selectInput("village","FilterBy village:",choices = c("All",choice),selected = NULL),
                       selectInput("G_season","FilterBy Gujarati Season:",choices = c("All",season),selected = NULL),
                       selectInput("E_season","FilterBy Season:",choices = c("All",season3),selected = NULL),
                       selectInput("festival","FilterBy Festival:",choices = c("All",festival),selected = NULL),
                       actionButton("update","Create/Change"),
                       width = 4
                     )
                     ,mainPanel(wordcloud2Output("AmrutCloud"))),
            tabPanel("Overview",
                     fluidRow(
                       column(6,titlePanel("Core Wordlist"),dataTableOutput("freqtable")),
                       column(6,titlePanel("Bar Graph"),plotOutput("mainBarGraph"))
                     )
            )
          )
        ),
        
        tabPanel("Vachnamrut Varnan",
          tabsetPanel(
            tabPanel("WordCloud",
                     sidebarPanel(
                       selectInput("Varnan_village","FilterBy village:",choices = c("All",choice),selected = NULL),
                       selectInput("Varnan_G_season","FilterBy Gujarati Season:",choices = c("All",season),selected = NULL),
                       selectInput("Varnan_E_season","FilterBy Season:",choices = c("All",season3),selected = NULL),
                       selectInput("Varnan_festival","FilterBy Festival:",choices = c("All",festival),selected = NULL),
                       actionButton("changeVarnan","Create/Change"),
                       width = 4
                     )
                     ,mainPanel(wordcloud2Output("VarnanCloud"))),
            tabPanel("Overview",
                     fluidRow(
                       column(6,titlePanel("Varnan Wordlist"),dataTableOutput("VarnanFreqTable")),
                       column(6,titlePanel("Bar Graph"),plotOutput("varnanBarGraph"))
                     )
            )
          )
        ),
        
        tabPanel("Question Analysis",
                 tabsetPanel(
                   tabPanel("Pie Chart",
                            fluidRow(
                              column(6,titlePanel("List of Parmhans"),dataTableOutput("QuesPersonTable")),
                              column(6,titlePanel("Pie Chart"),plotOutput("QuesPieChart"))
                            )
                            ),
                   tabPanel("Pareto Chart",plotOutput("paretoChart",height = "600px"))
                 )
        ),
        
        tabPanel("Length Analysis",titlePanel("Vachnamrut Length Analysis"),
                 tabsetPanel(
                   tabPanel("Histogram",plotlyOutput("histLength")),
                   tabPanel("Scatter plot",plotlyOutput("scatterPlot",height = "600px"))
                 )
        ),
        tabPanel("Vachnamrut Counts",
                 fluidRow(
                   column(6,titlePanel("Seasonwise Vachnamrut"),plotlyOutput("stackBarChart")),
                   column(6,titlePanel("Villagewise Vachnamrut"),plotOutput("GamwiseVachnamrut")),
                   column(6,titlePanel("Monthwise Vachnamrut"),plotOutput("monthWiseVachanamrut")),
                   column(6,titlePanel("Tithiwise Vachnamrut"),plotOutput("tithiwiseVachnamrut")),
                   column(6,titlePanel("Samvatwise Vachnamrut"),plotOutput("Samvatwisevachnamrut")),
                   column(6,titlePanel("Yearwise Vachnamrut"),plotOutput("E_yearwisevachnamrut"))
                 )
        ),
        tabPanel("Gap Analysis",
                 tabsetPanel(
                   tabPanel("Bubble Graph",plotlyOutput("dateAnalysis",height = "600px")),
                   tabPanel("Scatter Plot",plotlyOutput("scatterGap",height = "600px")),
                   tabPanel("Histogram",plotlyOutput("gapAnalysis",height = "600px")),
                   tabPanel("Summary Table",dataTableOutput("summaryGap"))
                 )
                 
                 )
        , type = "pills"
      )
    )
#)


server <- function(input, output) {
  
  AmrutCloud = eventReactive(input$update,{
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        FilterByGam <- input$village
        FilterBySeason6 <- input$G_season
        FilterBySeason3 <- input$E_season
        FilterByUtsav <- input$festival
        if(FilterByGam != "All"){
          Vtable <- Vtable %>% filter(Gam==FilterByGam)
        }
        if(FilterBySeason6 != "All"){
          Vtable <- Vtable %>% filter(Season6==FilterBySeason6)
        }
        if(FilterBySeason3 != "All"){
          Vtable <- Vtable %>% filter(Season3==FilterBySeason3)
        }
        if(FilterByUtsav !="All"){
          Vtable <- Vtable %>% filter(Utsav==FilterByUtsav)
        }
        
        Text_guj <- Vtable$AmrutGuj
        Stemming_words <- read_utf8("Resource Files\\stemming.txt")
        C_text = str_replace_all(Text_guj,'[\r\n.;“”"\',?()!‘’]',' ')
        replaceBy <- "NULL"
        tList <- "NULL"
        space <- " "
        for (i in 1:length(Stemming_words)) 
        {
          tList <- unlist(strsplit(Stemming_words[i],","))
          replaceBy <- tList[1]
          replaceBy <- paste(space,replaceBy)
          #print(replaceBy)
          for(j in 2:length(tList))
          {
            C_text <- str_replace_all(C_text,tList[j],replaceBy)
          }
        }
        GStopword <- read_utf8("Resource Files\\stopwords.txt")
        raw_data <- data.frame(words = unlist(strsplit(x = C_text, split = ' ')))
        imp_words <- raw_data$words[!raw_data$words %in% GStopword]
        imp_words_freq <- data.frame(table(imp_words))
        colnames(imp_words_freq) = c('Words', 'Freq')
        imp_words_freq <- imp_words_freq %>% arrange(-Freq)
        write_excel_csv(imp_words_freq,"Generated Files\\Auto.csv")
        if(imp_words_freq$Freq[1] > imp_words_freq$Freq[2]+30){
          imp_words_freq$Freq[1] <- (imp_words_freq$Freq[2]+30) 
        }
        top_1000_words <- imp_words_freq %>% head(250)
        wordcloud_rep <- repeatable(wordcloud2)
        wordcloud_rep(top_1000_words,size = 1,color = 'random-dark')
      })
    })
    
  })
  
  output$AmrutCloud <- renderWordcloud2({
    withProgress({
      setProgress(message = "Creating Wordcloud...")
      AmrutCloud()
    })
  })
  
####   Varnan Cloud Process   ####
  
  VarnanCloud = eventReactive(input$changeVarnan,{
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        FilterByGam <- input$Varnan_village
        FilterBySeason6 <- input$Varnan_G_season
        FilterBySeason3 <- input$Varnan_E_season
        FilterByUtsav <- input$Varnan_festival
        if(FilterByGam != "All"){
          Vtable <- Vtable %>% filter(Gam==FilterByGam)
        }
        if(FilterBySeason6 != "All"){
          Vtable <- Vtable %>% filter(Season6==FilterBySeason6)
        }
        if(FilterBySeason3 != "All"){
          Vtable <- Vtable %>% filter(Season3==FilterBySeason3)
        }
        if(FilterByUtsav !="All"){
          Vtable <- Vtable %>% filter(Utsav==FilterByUtsav)
        }
        
        Text_guj <- Vtable$VarnanGuj
        Stemming_words <- read_utf8("Resource Files\\stemming_varnan.txt")
        C_text = str_replace_all(Text_guj,'[\r\n.;“”"\',?()!‘’]',' ')
        replaceBy <- "NULL"
        tList <- "NULL"
        space <- " "
        for (i in 1:length(Stemming_words)) 
        {
          tList <- unlist(strsplit(Stemming_words[i],","))
          replaceBy <- tList[1]
          replaceBy <- paste(space,replaceBy)
          #print(replaceBy)
          for(j in 2:length(tList))
          {
            C_text <- str_replace_all(C_text,tList[j],replaceBy)
          }
        }
        GStopword <- read_utf8("Resource Files\\stopwords_varnan.txt")
        raw_data <- data.frame(words = unlist(strsplit(x = C_text, split = ' ')))
        imp_words <- raw_data$words[!raw_data$words %in% GStopword]
        imp_words_freq <- data.frame(table(imp_words))
        colnames(imp_words_freq) = c('Words', 'Freq')
        imp_words_freq <- imp_words_freq %>% arrange(-Freq)
        write_excel_csv(imp_words_freq,"Generated Files\\Auto_varnan.csv")
        if(imp_words_freq$Freq[1] > imp_words_freq$Freq[2]+30){
          imp_words_freq$Freq[1] <- (imp_words_freq$Freq[2]+30) 
        }
        
        top_1000_words <- imp_words_freq %>% head(250)
        wordcloud_rep <- repeatable(wordcloud2)
        wordcloud_rep(top_1000_words,size = 1,color = 'random-dark')
      })
    })
    
  })
  
  #wordcloud_rep <- repeatable(wordcloud2)
  output$VarnanCloud <- renderWordcloud2({
    withProgress({
      setProgress(message = "Creating Wordcloud...")
      VarnanCloud()
    })
  })
  
  #### Frequency Table  #####
  freqtable = eventReactive(input$update,{
    freqData <- read_csv("Generated Files\\Auto.csv",col_names = TRUE)
    freqData <- as.table(as.matrix(freqData))
  })
  
  output$freqtable <- renderDataTable({
    freqtable()
  })
  
  
  VarnanFreqTable = eventReactive(input$changeVarnan,{
    freqData <- read_csv("Generated Files\\Auto_varnan.csv",col_names = TRUE)
    freqData <- as.table(as.matrix(freqData))
  })
  
  output$VarnanFreqTable <- renderDataTable({
    VarnanFreqTable()
  })
  
  mainBarGraph = eventReactive(input$update,{
    freqData <- read_csv("Generated Files\\Auto.csv",col_names = TRUE)
    barplot(freqData[1:15,]$Freq,ylim = c(0,freqData$Freq[1]),col = "lightblue",names.arg = freqData[1:15,]$Words,las = 2 , cex.names = 1.2,cex.axis = 1)
  })
  
  output$mainBarGraph <- renderPlot({
    mainBarGraph()
  })
  
  varnanBarGraph = eventReactive(input$changeVarnan,{
    freqData <- read_csv("Generated Files\\Auto_varnan.csv",col_names = TRUE)
    barplot(freqData[1:15,]$Freq,ylim = c(0,freqData$Freq[1]),col = "blue",names.arg = freqData[1:15,]$Words,las = 2 , cex.names = 1.2,cex.axis = 1)
  })
  
  output$varnanBarGraph <- renderPlot({
    varnanBarGraph()
  })
  
  output$QuesPersonTable <- renderDataTable({
    sheet_data <- read_csv("Resource Files\\Que_Paramahans.csv",col_names = TRUE)
    swami <- read_utf8("Resource Files\\swami.txt")
    for(i in 1:length(sheet_data$Paramhans_Name)){
      sheet_data[i,]$Paramhans_Name <- paste(sheet_data[i,]$Paramhans_Name,swami)
    }
    sheet_data <- as.table(as.matrix(sheet_data))
  })
  
  output$QuesPieChart <- renderPlot({
    sheet_data <- read_csv("Resource Files\\Que_Paramahans.csv",col_names = TRUE)
    pie(sheet_data[1:10,]$Count,labels = sheet_data[1:10,]$Count,main = "Questions Per Paramhans", col = rainbow(10),radius = 1)
    legend("topright",text.width = 0.6,sheet_data[1:10,]$Paramhans_Name,fill = rainbow(10),cex = 1)
  })
  
  
  output$stackBarChart <- renderPlotly({
    GroupedData <- Vtable %>% group_by(Season6,Season3) %>% count(Season6)
    Gujarati_Season <- GroupedData$Season6
    English_Season <- GroupedData$Season3
    No_Of_Vachnamrut <- GroupedData$n
    
    df <- data.frame(Gujarati_Season,English_Season,No_Of_Vachnamrut)
    #p <- ggplot(df,aes(x = English_Season,y = No_Of_Vachnamrut))+geom_col(aes(fill = Gujarati_Season),width = 0.7)
    #plot(p)
    write_excel_csv(df,"Generated Files\\GujaratiSeason.csv")
    df %>%
      plot_ly(type = "bar", orientation = "v",x = ~English_Season, y = ~No_Of_Vachnamrut, name = ~Gujarati_Season,hoverinfo = "name+y") %>%
      layout(title = "Seasonwise number of Vachnamrut",barmode = "stack",xaxis = list(title = "Season"))
    
  })
  
  
  output$scatterPlot <- renderPlotly({
    
    Vtable <- Vtable %>% mutate(WordCount = (str_count(AmrutGuj," ") + str_count(VarnanGuj," ") + 2))
    VachnamrutLength <- data.frame(Vtable$AmrutNo,Vtable$WordCount)
    
    colnames(VachnamrutLength) = c('AmrutNo', 'no_of_words')
    VachnamrutLength <- VachnamrutLength %>% arrange(-no_of_words)
    write_excel_csv(VachnamrutLength,"Generated Files\\Vachnamrutlength.csv")
    no_of_words<-VachnamrutLength$no_of_words
    AmrutNo<-VachnamrutLength$AmrutNo
    p <- plot_ly(type = 'scatter', mode = 'markers') %>%
      add_trace(
        x = no_of_words, 
        y =AmrutNo,
        text = paste(AmrutNo,no_of_words,sep = '\n'),
        hoverinfo = 'text',
        marker = list("blue"),
        showlegend = F
        
      ) %>%
      layout(title= "Length of Vachanamrut",yaxis = list(title = "Vachnamrut_No"),xaxis = list(title = "Length of Vachnamrut(In words)"))
    ggplotly(p)

  })
  
  
  
  output$histLength <- renderPlotly({
    Vtable <- Vtable %>% mutate(WordCount = (str_count(AmrutGuj," ") + str_count(VarnanGuj," ") + 2))
    VachnamrutLength <- data.frame(Vtable$AmrutNo,Vtable$WordCount)
    
    colnames(VachnamrutLength) = c('AmrutNo', 'no_of_words')
    VachnamrutLength <- VachnamrutLength %>% arrange(-no_of_words)
    write_excel_csv(VachnamrutLength,"Generated Files\\Vachnamrutlength.csv")
    no_of_words<-VachnamrutLength$no_of_words
    AmrutNo<-VachnamrutLength$AmrutNo
    
    plot_ly(x = VachnamrutLength$no_of_words ,type = "histogram") %>%
      layout(title = "Length Analysis of Vachnamrut",yaxis = list(title = "No_of_Vachnamrut"),xaxis = list(title = "No_of_Words"))
  })
  
  output$monthWiseVachanamrut <- renderPlot({
    tithi <- Vtable$Tithi 
    
    for(i in 1:length(tithi)){
      tithi[i] = strsplit(tithi[i],' ')[[1]][1]
    }
    Vtable$Tithi <- tithi
    groupedData <- Vtable %>% group_by(Tithi) %>% count(Tithi)
    
    
    No_of_vachnamrut <- groupedData$n
    G_Month <-  groupedData$Tithi
    write_excel_csv(groupedData,"Generated Files\\Gujaratimonth.csv")
    #bubbles(value = No_of_vachnamrut, label = G_Month,key = G_Month,
    #        color = rainbow(12, alpha=NULL)[sample(12)]
    #)
    barplot(No_of_vachnamrut,ylim = c(0,40),col = "blue",names.arg = G_Month,las = 2 , cex.names = 1.2,cex.axis = 1,xlab = "Month",ylab = "No_of_Vachnamrut")
    
  })
  
  output$tithiwiseVachnamrut <- renderPlot({
    tithi <- Vtable$Tithi 
    tithi = str_replace_all(tithi,',','')
    for(i in 1:length(tithi)){
      tithi[i] = strsplit(tithi[i],' ')[[1]][3]
    }
    Stemming_words <- read_utf8("Resource Files\\tithi_text.txt")
    
    Vtable$Tithi <- tithi
    groupedData <- Vtable %>% group_by(Tithi) %>% count(Tithi)
    
    
    No_of_vachnamrut <- groupedData$n
    G_Tithi <-  groupedData$Tithi
    for (i in 1:length(Stemming_words)) 
    {
      tList <- unlist(strsplit(Stemming_words[i],","))
      replaceBy <- tList[1]
      #print(replaceBy)
      for(j in 1:length(G_Tithi)){
        if(as.character(G_Tithi[j]) == as.character(tList[2])){
          G_Tithi[j] <- str_replace(G_Tithi[j],tList[2],replaceBy)
          #print(G_Tithi[j])
        }
      }
      
    }
    df <- data.frame(G_Tithi,No_of_vachnamrut)
    df <- df %>% arrange(-No_of_vachnamrut)
    write_excel_csv(df,"Generated Files\\Gujaratitithi.csv")
    
    #bubbles(value = No_of_vachnamrut, label = G_Tithi,key = G_Tithi,
    #        color = rainbow(length(G_Tithi), alpha=NULL)[sample(No_of_vachnamrut)]
    #)
    
    barplot(df$No_of_vachnamrut,ylim = c(0,40),col = "blue",names.arg = df$G_Tithi,las = 2 , cex.names = 1.2,cex.axis = 1,xlab = "Tithi",ylab = "No_of_Vachnamrut")
    
  })
  
  output$Samvatwisevachnamrut <- renderPlot({
    Vtable <- Vtable %>% filter(Id != 263)
    tithi <- Vtable$Tithi
    for(i in 1:length(tithi)){
      t = strsplit(tithi[i],' ')[[1]]
      tithi[i] = t %>% tail(1)
    }
    
    Vtable$Tithi <- tithi
    groupedData <- Vtable %>% group_by(Tithi) %>% count(Tithi)
    
    No_of_vachnamrut <- groupedData$n
    G_Year <-  groupedData$Tithi
    
    df <- data.frame(G_Year,No_of_vachnamrut)
    write_excel_csv(df,"Generated Files\\SamvatWise.csv")
    
    barplot(No_of_vachnamrut,ylim = c(0,max(No_of_vachnamrut)),col = "blue",names.arg = G_Year,las = 2 , cex.names = 1.2,cex.axis = 1,xlab = "Year",ylab = "No_of_Vachnamrut")
    
  })
  
  output$GamwiseVachnamrut <- renderPlot({
    
    Gam <- Vtable$Gam
    
    groupedData <- Vtable %>% group_by(Gam) %>% count(Gam)
    
    No_of_vachnamrut <- groupedData$n
    Gam <-  groupedData$Gam
    
    df <- data.frame(Gam,No_of_vachnamrut)
    write_excel_csv(df,"Generated Files\\GamWise.csv")
    
    barplot(No_of_vachnamrut,ylim = c(0,max(No_of_vachnamrut)),col = "blue",names.arg = Gam,las = 2 , cex.names = 1.2,cex.axis = 1,xlab = "Village",ylab = "No_of_Vachnamrut")
    
    #plot_ly(
    #  x = Gam,
    #  y = No_of_vachnamrut,
    #  name = "Gam",
    #  type = "bar"
    #)
    
  })
  
  output$E_yearwisevachnamrut <- renderPlot({
    Vtable <- Vtable %>% filter(Id != 263)
    tarikh <- Vtable$Tarikh
    for(i in 1:length(tarikh)){
      tarikh[i] = strsplit(tarikh[i],'-')[[1]][1]
    }
    
    Vtable$Tarikh <- tarikh
    groupedData <- Vtable %>% group_by(Tarikh) %>% count(Tarikh)
    
    No_of_vachnamrut <- groupedData$n
    G_Year <-  groupedData$Tarikh
    
    df <- data.frame(G_Year,No_of_vachnamrut)
    write_excel_csv(df,"Generated Files\\E_YearWise.csv")
    
    barplot(No_of_vachnamrut,ylim = c(0,max(No_of_vachnamrut)),col = "blue",names.arg = G_Year,las = 2 , cex.names = 1.2,cex.axis = 1,xlab = "Samvat",ylab = "No_of_Vachnamrut")
    
  })
  
  output$dateAnalysis <- renderPlotly({
    dates <- Vtable$Tarikh
    for(i in 1:length(dates)){
      dates[i] = strsplit(dates[i],' ')[[1]][1]
    }
    
    Vtable$Tarikh <- dates
    
    Vtable <- Vtable %>% mutate(WordCount = (str_count(AmrutGuj," ") + str_count(VarnanGuj," ") + 2), date_vach = as.character(Tarikh), VachCount = 1) %>% filter(Id != 263)
    
    
    date_vach <- Vtable$date_vach
    WordCount<- Vtable$WordCount
    df <- data.frame(date_vach = Vtable$date_vach, WordCount= Vtable$WordCount, VachCount = Vtable$VachCount)
    df <- df %>% arrange(date_vach)
    
    date_vach <- as.character(date_vach)
    
    list_of_dates <- seq(as.Date(df$date_vach[1]),as.Date(df$date_vach[length(df$date_vach)]),by = 1)
    list_of_dates <- as.character(list_of_dates)
    
    
    timeline = data.frame(date_vach=list_of_dates) %>% left_join(df, by="date_vach") 
    timeline[is.na(timeline)] = 0
    
    tdf <- data.frame(timeline$date_vach,timeline$WordCount,timeline$VachCount)
    Date_Of_Vachanamrut <- tdf$timeline.date_vach
    Vachanamurt_Count <- tdf$timeline.VachCount
    
    write_excel_csv(timeline,"Generated Files\\tarikh.csv")
    b <- plot_ly(tdf,x = ~Date_Of_Vachanamrut , y = ~Vachanamurt_Count,type = "scatter",mode = "markers", marker = list(size = ~(tdf$timeline.WordCount/500),opacity = 1 , color = 'rgb(255,65,54)')) %>%
      layout(title =  " Timeline")
    
    b
    
  })
  
  output$scatterGap <- renderPlotly({
    
    dates <- Vtable$Tarikh
    for(i in 1:length(dates)){
      dates[i] = strsplit(dates[i],' ')[[1]][1]
    }
    
    Vtable$Tarikh <- dates
    
    Vtable <- Vtable %>% mutate(WordCount = (str_count(AmrutGuj," ") + str_count(VarnanGuj," ") + 2), date_vach = as.character(Tarikh), VachCount = 1) %>% filter(Id != 263)
    
    
    date_vach <- Vtable$date_vach
    WordCount<- Vtable$WordCount
    df <- data.frame(date_vach = Vtable$date_vach, WordCount= Vtable$WordCount, VachCount = Vtable$VachCount)
    df <- df %>% arrange(date_vach)
    
    date_vach <- as.character(date_vach)
    
    
    list_of_dates <- seq(as.Date(df$date_vach[1]),as.Date(df$date_vach[length(df$date_vach)]),by = 1)
    list_of_dates <- as.character(list_of_dates)
    
    
    timeline = data.frame(date_vach=list_of_dates) %>% left_join(df, by="date_vach") 
    timeline[is.na(timeline)] = 0
    
    tdf <- data.frame(timeline$date_vach,timeline$WordCount,timeline$VachCount)
    write_excel_csv(timeline,"Generated Files\\tarikh.csv")
    
    p <- plot_ly(type = 'scatter', mode = 'markers') %>%
      add_trace(
        x = tdf$timeline.date_vach, 
        y = tdf$timeline.VachCount,
        marker = list("blue"),
        showlegend = F
      ) %>%
      layout(title= "Length of Vachanamrut",yaxis = list(title = "Vachnamrut_No"),xaxis = list(title = "Length of Vachnamrut(In words)"))
    
    p

  }) 
  
  output$gapAnalysis <- renderPlotly({
    dates<-Vtable$Tarikh
    amrut_no <- Vtable$AmrutNo
    
    for(i in 1:length(dates))
    {
      dates[i]<-strsplit(dates[i]," ")[[1]][1]
    }
    
    dates_d<-data.frame(dates,amrut_no)
    colnames(dates_d) = c('dates_vach','amrut_no')
    dates_d <- dates_d %>% arrange(dates_vach)
    
    amrut_no <- dates_d$amrut_no
    dates<-dates_d$dates_vach
    dates<-dates[-1]
    amrut_no <- amrut_no[-1]
    
    dates_finish<-dates[2:length(dates)]
    dates_start<-dates[1:length(dates)-1]
    
    amrut_finish<-amrut_no[2:length(amrut_no)]
    amrut_start<-amrut_no[1:length(amrut_no)-1]
    
    gap_amrut<- data.frame(dates_finish,dates_start,amrut_finish,amrut_start)
    
    
    for(i in 1:length(dates)-1)
    {
      gap_amrut$date_diff[i] <- as.Date(as.character(gap_amrut$dates_finish[i]), format="%Y-%m-%d")-
        as.Date(as.character(gap_amrut$dates_start[i]), format="%Y-%m-%d")
    }
    
    write_excel_csv(gap_amrut,"Generated Files\\gap_amrut.csv")
    
    plot_ly(x=gap_amrut$date_diff,type = "histogram")%>%
      layout(title = "Gap Analysis",yaxis = list(title = "No_of Vachnamrut"),xaxis = list(title = "Gap(In days)"))
    
  })
  
  output$summaryGap <- renderDataTable({
    sheet_data <- read_csv("Generated Files\\gap_amrut.csv",col_names = TRUE)
    sheet_data <- as.table(as.matrix(sheet_data))
    
  })
  
  output$paretoChart <- renderPlot({
    sheet_data <- read_csv("Resource Files\\Que_Paramahans.csv",col_names = TRUE)
    sheet_data <- sheet_data %>% filter(sheet_data$Count != 0)
    Paramhans_name <- sheet_data$Paramhans_Name
    No_of_question <- sheet_data$Count
    names(No_of_question) <- Paramhans_name
    pareto.chart(No_of_question, ylab = "Error frequency", col=heat.colors(length(No_of_question)))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)