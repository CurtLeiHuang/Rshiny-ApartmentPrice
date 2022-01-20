#######################################################################################################################################
#library packages, if package is no installed ,installed and then libray
#######################################################################################################################################

packages = c("proxy", "shiny","leaflet","tidyverse","DT","shinycssloaders","tidytext","shinydashboard","dashboardthemes","wordcloud","shinyWidgets")
      
## Now load or install&load all
package.check <- lapply( packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#######################################################################################################################################
#prepare some input for slidebar
#######################################################################################################################################

columnchoices=data.frame(selectedcolumns=c(1,6,7,8,10,11))
mylist <- as.list(columnchoices$selectedcolumns)

# rename
names(mylist) <- c("Bed/Bath/Parking","Apartment discription","Agent","Link","Predicted price from Model 1","Predicted price from  Model 2")
#for leaflet 
maplocation=list("adelaide-sa-5000"=c(138.6007,-34.9285),"perth-wa-6000"=c(115.8605,-31.9505),"sydney-nsw-2000"=c(151.2093,-33.8688),"brisbane-qld-8000"=c(153.0251,-27.4698))

selectednumbers=c("100K","300K","600K","900K","1.2M","1.6M","2M","2.5M","4M","6M")
truenumbers=c(100000,300000,600000,900000,1200000,1600000,2000000,2500000,4000000,6000000)


#######################################################################################################################################
#ui part 
#######################################################################################################################################

body<- dashboardBody(
    shinyDashboardThemes(theme = "onenote"),
    tabItems(tabItem(tabName = "a_map",
                     fluidRow(box(title="Map",status = "primary", collapsible = TRUE,width=8,leafletOutput("map")),
                              box(title="Histogram",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner(plotOutput(outputId = "my_histogram")),htmlOutput("text"))),
                     DTOutput("data")),
             tabItem(tabName = "word1",
                     fluidRow(box(title="Top 100 wordcloud",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner( plotOutput(outputId = "my_wordcloud1"))),
                              box(title="Top 10 captialised word",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner(plotOutput(outputId = "my_capitalisedword1"))),
                              box(title="Top 10 common word",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner(plotOutput(outputId = "my_commonword1")))),
                     box(title="groups based on property description similiarity",status = "primary", collapsible = TRUE,width=12,plotOutput(outputId = "hclustwords1")),h4(textOutput("text1")),
                     DTOutput("datawords1")),
             tabItem(tabName = "word2",
                     fluidRow(box(title="Top 100 wordcloud",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner( plotOutput(outputId = "my_wordcloud2"))),
                              box(title="Top 10 captialised word",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner(plotOutput(outputId = "my_capitalisedword2"))),
                              box(title="Top 10 common word",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner(plotOutput(outputId = "my_commonword2")))),
                     box(title="groups based on property description similiarity",status = "primary", collapsible = TRUE,width=12,plotOutput(outputId = "hclustwords2")),h4(textOutput("text2")),
                     DTOutput("datawords2")),
             tabItem(tabName = "word3",
                     fluidRow(box(title="Top 100 wordcloud",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner( plotOutput(outputId = "my_wordcloud3"))),
                              #not enough length of description for 3 words capitalised word analysis, mute from beginning
                              box(title="Top 10 captialised word",status = "primary", collapsible = TRUE,width=4,p("Not enough capitalised words for this analysis")),
                              box(title="Top 10 common word",status = "primary", collapsible = TRUE,width=4,shinycssloaders::withSpinner(plotOutput(outputId = "my_commonword3")))),
                     box(title="groups based on property description similiarity",status = "primary", collapsible = TRUE,width=12,plotOutput(outputId = "hclustwords3")),h4(textOutput("text3")),
                     DTOutput("datawords3"))
             )
)

sidebar<-  dashboardSidebar(
    width = 300,
    sidebarMenu(
        selectInput("city", "Choose a city:", list("Pre-load" = list("adelaide-sa-5000"),"Not load" = list("perth-wa-6000", "sydney-nsw-2000","melbourne-vic-3000"))),
        menuItem("Price prediction",tabName="prediction",icon=icon("analytics"),
                 menuSubItem("Map", tabName = "a_map", icon = icon("map")),
                 #checkbox for number of bedrooms
                 checkboxGroupInput(inputId = "beds", label = "Select number of bedroom", choices = c(1:5),inline=T),
                 #checkbox for number of bathrooms
                 checkboxGroupInput(inputId = "baths", label = "Select number of bathroom", choices =c(1:5),inline=T),
                 #checkbox for number of parking
                 checkboxGroupInput(inputId = "parkings", label = "Select number of parking", choices =c(0:5),inline=T),
                 #slider of price ranges
                 sliderTextInput( inputId = "ranges", label = "Select price range",choices = selectednumbers,selected =  selectednumbers[c(1,10)],grid=TRUE),
                 checkboxGroupInput(inputId = "columns", label = "Select variables to display in table", choices = mylist,inline=T),
                 
                 downloadButton("downloadData", "Download data from selected city")),
        
        menuItem("Similarity analysis",tabName="description",
                 menuSubItem("1 word", tabName = "word1", icon = icon("charts")),
                 menuSubItem("2 words", tabName = "word2", icon = icon("charts")),
                 menuSubItem("3 words", tabName = "word3", icon = icon("charts")),
                  #checkbox for number of bedrooms in similiarity analysis
                 checkboxGroupInput(inputId = "bedsD", label = "Select number of bedroom", choices = c(1:5),inline=T)
                 )
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "Property analysis",titleWidth=300),
    sidebar,
    body)

#######################################################################################################################################
#server part 
#######################################################################################################################################

server <- function(input, output, session){
####################################################################
#set up demo file, if input city is adelaide, load processed data ,otherwise, load source code and do a real time collection 
####################################################################
    
    rundata<-reactive({
        if(input$city=="adelaide-sa-5000"){
            load("adelaide.rda")
            mydata=allinformation
            return(mydata)
        }else{
            tmpR<-paste0(input$city,".R")
            source(tmpR)
            mydata=allinformation
            return(mydata)
        }
    })
####################################################################
#for download file
####################################################################
       
    downloadcsv<-reactive(rundata()%>%select(c(1:12)))
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$downloaddata, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(downloadcsv(), file, row.names = FALSE)
        }
    )
####################################################################
# pass in collected data for prediction analysis part 
####################################################################
    
    vari <- reactive({
        if(is.null(input$beds) & is.null(input$baths) & is.null(input$parkings)){
            return(rundata())
        }else if(is.null(input$beds) & !is.null(input$baths) & !is.null(input$parkings)){
            filter(rundata(), bath%in%input$baths & parking%in%input$parkings )
        }else if(!is.null(input$beds) & is.null(input$baths) & !is.null(input$parkings)){
            filter(rundata(),bed%in%input$beds & parking%in%input$parkings)
        }else if( !is.null(input$beds) & !is.null(input$baths) & is.null(input$parkings)){
            filter(rundata(),bed%in%input$beds & bath%in%input$baths )
        }else if(is.null(input$beds) & is.null(input$baths) & !is.null(input$parkings)){
            filter(rundata(), parking%in%input$parkings )
        }else if(is.null(input$beds) & !is.null(input$baths) & is.null(input$parkings)){
            filter(rundata(), bath%in%input$baths )
        }else if(!is.null(input$beds) & is.null(input$baths) & is.null(input$parkings)){
            filter(rundata(),bed%in%input$beds )
        }else if(!is.null(input$beds) & !is.null(input$baths) & !is.null(input$parkings)){
            filter(rundata(),bed%in%input$beds & bath%in%input$baths & parking%in%input$parkings)
        }
    })
####################################################################
# pass in collected data for similiarity analysis part 
####################################################################

    vari1 <- reactive({
        if(is.null(input$bedsD)){
            return(rundata())
        }else if(!is.null(input$bedsD)) {
            filter(rundata(),bed%in%input$bedsD)
        }
    })
####################################################################
# summary of total properties and number selected of preporties in prediction analysis 
####################################################################

    output$text <- renderText({
        my_data <-vari()%>% filter(newpricemax >= truenumbers[which(selectednumbers==input$ranges[1])] & newpricemax <= truenumbers[which(selectednumbers==input$ranges[2])])
        paste("<font color=\"#FF0000\"><b>",nrow(my_data),"</b></font>","of", nrow(rundata()),"properties is/are displayed.")
    })
####################################################################
# histrogram in prediction analysis 
####################################################################

    output$my_histogram<- renderPlot({
        my_data <-vari()%>%filter(newpricemax >= truenumbers[which(selectednumbers==input$ranges[1])] & newpricemax <= truenumbers[which(selectednumbers==input$ranges[2])])
        if(nrow(my_data)==0 ){
            plot.new()
        }else{
         floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
         ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
#plot two histogram together, one for advertised price, one for predicted price 
            h1=hist(my_data$newpricemax)
            h2=hist(my_data$predictprice)
            if(max(h1$counts)>=max(h2$counts)){
            hist(my_data$newpricemax , main = "Histogram of advertised/predicted price (mean)",col=rgb(1,0,0,0.5), border = F,axes=F,ylab="Number of properties",xlab="Price range")
            hist(my_data$predictprice, main = "",col=rgb(0,0,1,0.5), border =F,add=T,ases=F)
                        startend=c(floor_dec(min(my_data$newpricemax),-5),ceiling_dec(max(my_data$newpricemax),-5))

            }else{
                 hist(my_data$predictprice , main = "Histogram of advertised/predicted price (mean)",col=rgb(0,0,1,0.5), border = F,axes=F,ylab="Number of properties",xlab="Price range")
                 hist(my_data$newpricemax, main = "",col=rgb(1,0,0,0.5), border =F,add=T,ases=F)
                        startend=c(floor_dec(min(my_data$predictprice),-5),ceiling_dec(max(my_data$predictprice),-5))

            }
 #make a special y and x axis        
            if(max(max(h1$counts),max(h2$counts))<3){
              yaxis=round(seq(0,max(max(h1$counts),max(h2$counts)),by=max(max(h1$counts),max(h2$counts))))

                }else{
                    yaxis=round(seq(0,max(max(h1$counts),max(h2$counts)),by=(max(max(h1$counts),max(h2$counts))/3)))
                }
            
            labelplace=seq(startend[1],startend[2],by=(startend[2]-startend[1])/4)
 #convert number  to user friendly format, e.g. 100000 to 100K          
            comprss <- function(tx) {
                div <- findInterval(as.numeric(gsub("\\,", "", tx)), c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
                paste0(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), c("","K","M","B","T")[div] )
            }

            customlabel=comprss(labelplace)
            axis(1,at=labelplace,labels=customlabel)
            axis(2,at=yaxis,labels=yaxis)
            legend("topright",legend=c("Advertised price","Predicted price"),text.col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),cex=0.8)
        }
    })
####################################################################
# table in prediction analysis 
####################################################################

    output$data <- renderDT({
        DT::datatable(
                vari()%>%filter(newpricemax >= truenumbers[which(selectednumbers==input$ranges[1])] & newpricemax <= truenumbers[which(selectednumbers==input$ranges[2])]) %>%
                select(c("advertised.price","predicted.price","address",as.numeric(unlist(input$columns)))),options = list(pageLength = 5,scrollX = TRUE,scrollY = TRUE),rownames= FALSE)
    })
####################################################################
# map in prediction analysis, if Longitude and Latitude avaiable, add markers in map
####################################################################
 
    output$map <- renderLeaflet({
        mypoints <-vari()%>% filter(newpricemax >= truenumbers[which(selectednumbers==input$ranges[1])] & newpricemax <= truenumbers[which(selectednumbers==input$ranges[2])])
        if(!is.null(mypoints$Longitude)){
            leaflet() %>% addTiles() %>% setView(lng= as.numeric(unlist(maplocation[names(maplocation)==input$city])[1]), lat=as.numeric(unlist(maplocation[names(maplocation)==input$city])[2]), zoom = 14)%>%
                addMarkers(lng=as.vector(mypoints[,"Longitude"]), lat=as.vector(mypoints[,"Latitude"]),popup=as.vector(paste0("<b>Advertised Price: </b>",mypoints[,"advertised.price"], "<br>","<b>Predicted Price: </b>",mypoints[,"predicted.price"], "<br>","<a href =",mypoints[,"link"] ," target=\"_blank\">", mypoints[,"address"], "</a>")),clusterOptions=markerClusterOptions(frezeAtZoom=5))
        }else{
            leaflet() %>% addTiles() %>% setView(lng= 138.6007, lat=-34.9285, zoom = 15)
        }
    })
####################################################################
# 1 word wordcloud in similiarity analysis 
####################################################################
    output$my_wordcloud1<- renderPlot({
        # if input is NULL, plot nothing
        if(nrow(vari1())==0 ){
            plot.new()
        }else{
        my_data <-vari1()%>%
            unnest_tokens( word,description) %>%
            filter(!word %in% stop_words$word,
                   !str_detect(word, pattern= "[[:digit:]]"),
                   !str_detect(word, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word, pattern = "\\b(.)\\b"))%>%count(word)%>%
            filter(n >= 3) %>% group_by(word) %>%arrange(desc(n))
# if it is empty word after filtering,  plot nothing
        if(length(my_data$word)<1){
            plot.new()
            }else{
        wordcloud(words = my_data$word , freq = my_data$n, min.freq = 1,
                  max.words=100, random.order=FALSE, rot.per=0.35,  scale=c(2,.4),
                  colors=brewer.pal(8, "Dark2"))
            }
        }
        
        })
####################################################################
# 1 word top 10 common word in similiarity analysis 
####################################################################
        
    output$my_commonword1<- renderPlot({
         if(nrow(vari1())==0 ){
            plot.new()
        }else{
        commonword <-vari1()%>%
            unnest_tokens( word,description) %>%
            filter(!word %in% stop_words$word,
                   !str_detect(word, pattern= "[[:digit:]]"),
                   !str_detect(word, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word, pattern = "\\b(.)\\b"))%>%count(word)%>%
            filter(n >= 3) %>% group_by(word) %>%arrange(desc(n))
     
        if(length(commonword$word)<1){
            plot.new()
            }else{
   
        barplot(commonword$n[1:10],main="Top 10 common words",ylab="Counts",names.arg=commonword$word[1:10],las=2,col="skyblue")
            }
        }
    })
####################################################################
# 1 word top 10 capitalised word in similiarity analysis 
####################################################################
    
    output$my_capitalisedword1<-renderPlot({
         if(nrow(vari1())==0 ){
            plot.new()
        }else{
        vocUpper <-vari1()%>%
            unnest_tokens(word, description,to_lower=FALSE) %>%
            anti_join(stop_words) %>%filter(!str_detect(word, pattern = "[[:digit:]]"), !str_detect(word, pattern = "[^\u0001-\u007F]+"), !str_detect(word, pattern = "\\b(.)\\b"))%>%count(word)  %>% group_by(word) %>%
            arrange(desc(n))%>%filter(str_detect(word,pattern="\\b[A-Z]+\\b"))
             if(length(vocUpper$word)<1){
            plot.new()
            }else{
  
                barplot(vocUpper$n[1:10],main="Top 10 capitalised words",ylab="Counts",names.arg=vocUpper$word[1:10],las=2,col="forestgreen")
                }
        }
    })
####################################################################
# 1 word hclust in similiarity analysis 
####################################################################

    output$hclustwords1 <- renderPlot({
         if(nrow(vari1())==0 ){
            plot.new()
        }else{
        df2 <-vari1()
        newdf2<-data.frame(df2,id=df2[,"trimedaddress"])
        commonword <-df2%>%dplyr::select(description) %>%
            unnest_tokens( word,description) %>%
            filter(!word %in% stop_words$word,
                   !str_detect(word, pattern= "[[:digit:]]"),
                   !str_detect(word, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word, pattern = "\\b(.)\\b"))%>%count(word)%>%
            filter(n >= 3) %>% group_by(word)  %>%arrange(desc(n))%>%pull(word)
        new<-newdf2%>%unnest_tokens( word,description)%>% filter(word %in% commonword) %>% count(id, word) %>% pivot_wider(id_cols = id, names_from = word, values_from = n) %>% map_df(replace_na, 0)%>%as.data.frame()
        rownames(new)=new[,1]

        df_dist <- parallelDist::parDist(as.matrix(new[, -1]), diag = TRUE)
        df_temp <- new[, -1] %>% transmute_all(function(x) if_else(x > 0, 1, 0))
        rownames(df_temp)=rownames(new)
        df_dist <- parallelDist::parDist(as.matrix(df_temp), method = "binary", diag = TRUE)
        if(nrow(as.matrix(df_dist))<2){
            plot.new()
        }else{
            h.c<-hclust(df_dist, method = "ward.D2")
            #cutoff, the lowest 5% of h, same cutoff for all 1,2,3 words
            cutoff=cutree(h.c,h=max(h.c$height)*0.05)
            subset=table(cutoff)[as.matrix(table(cutoff))>=2]
            toplot= rownames(as.matrix(df_dist))[cutoff%in%rownames(as.matrix(subset))]
            labels <- rownames(as.matrix(df_dist))
            labels[ !(labels %in% toplot) ] <- ""
            plot(h.c,  label=labels,xlab=NULL,ylab=("Distance between the clusters"),main=("Similiarities among descriptions from selected properties"))
            abline(h=max(h.c$height)*0.05,col="red",lty=2)
        }
        }
    })
####################################################################
# 1 word listed of properties with high similiar description in similiarity analysis 
####################################################################

    observe({
        df2 <-vari1()
        newdf2<-data.frame(df2,id=df2[,"trimedaddress"])
        commonword <-df2%>%  dplyr::select(description) %>%
            unnest_tokens( word,description) %>%
            filter(!word %in% stop_words$word,
                   !str_detect(word, pattern= "[[:digit:]]"),
                   !str_detect(word, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word, pattern = "\\b(.)\\b"))%>%count(word)%>%
            filter(n >= 3) %>% group_by(word)  %>%arrange(desc(n))%>%pull(word)

        new<-newdf2%>%unnest_tokens( word,description)%>% filter(word %in% commonword) %>% count(id, word) %>% pivot_wider(id_cols = id, names_from = word, values_from = n) %>% map_df(replace_na, 0)%>%as.data.frame()
        rownames(new)=new[,1]

        df_dist <- parallelDist::parDist(as.matrix(new[, -1]), diag = TRUE)
        df_temp <- new[, -1] %>% transmute_all(function(x) if_else(x > 0, 1, 0))
        rownames(df_temp)=rownames(new)
        df_dist <- parallelDist::parDist(as.matrix(df_temp), method = "binary", diag = TRUE)
        if(nrow(as.matrix(df_dist))<2){
            copied<-matrix(NA,0,0)
        }else{
            h.c<-hclust(df_dist, method = "ward.D2")
            #same cutoff as in hclust
            cutoff=cutree(h.c,h=max(h.c$height)*0.05)
            subset=table(cutoff)[as.matrix(table(cutoff))>=2]
            if(length(subset)==0){
                copied<-matrix(NA,0,0)
            }else{
                for(i in 1:length(subset)){
                    if(i==1){
                        tmp=rownames(as.matrix(cutoff))[cutoff==rownames(as.matrix(subset))[1]]
                        copied<-data.frame(df2[match(tmp,newdf2[,"id"]),c("address","description","agent")],group=paste("group","1",sep="-"))
                    }else{
                        tmp=rownames(as.matrix(cutoff))[cutoff==rownames(as.matrix(subset))[i]]
                        copied<-rbind(copied,data.frame(df2[match(tmp,newdf2[,"id"]),c("address","description","agent")],group=paste("group",i,sep="-")))
                    }
                }
            }
        }
####################################################################
# 1 word return text to explain what's items in table in similiarity analysis 
####################################################################
  
        output$text1 <- renderText({
            if(nrow(vari1())==0 ){
           "No property with selection criteria is available."
        }else{
            if(nrow(copied)>0){
                "Grouped property decriptions with high similiarities in cluster plot are listed below."
            }else{
                " These are no properities with highly similiar descriptions. Therefore, none of properites are listed below."
            }
            }
        })

        output$datawords1 <-renderDT({copied})
    })

####################################################################
# 2 words wordcloud in similiarity analysis 
####################################################################

    output$my_wordcloud2<- renderPlot({
    if(nrow(vari1())==0 ){
            plot.new()
    }else{
        my_data <-vari1()%>% dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>%
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b") )%>%
            unite("bigram", c(word1, word2), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%
            arrange(desc(n))
         if(length(my_data$word)<1){
            plot.new()
            }else{
        wordcloud(words = my_data$bigram , freq = my_data$n, min.freq = 1,
                  max.words=100, random.order=FALSE, rot.per=0.35, scale=c(2,.4),
                  colors=brewer.pal(8, "Dark2"))
            }
        }
    })
####################################################################
# 2 words top 10 common word in similiarity analysis 
####################################################################

    output$my_commonword2<- renderPlot({
            if(nrow(vari1())==0 ){
            plot.new()
        }else{
  
        commonword <-vari1()%>% dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b") )%>%
            unite("bigram", c(word1, word2), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))
                 if(length(commonword$n)<1){
            plot.new()
            }else{
   
        if(length(commonword$n)>10){
            barplot(commonword$n[1:10],main="Top 10 common words",ylab="Counts",axes=F,las=2,col="skyblue")
            text(seq(1,11.8,by=1.2), par("usr")[3] - 0.2, labels = commonword$bigram[1:10], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
            axis(2,at=seq(0,max(commonword$n),by=5))
        }else{
            barplot(commonword$n[1:length(commonword$n)],main="Top 10 common words",ylab="Counts",axes=F,las=2,col="skyblue")
            endpoint=1.15*length(commonword$n)
            text(seq(1,endpoint,by=1.2), par("usr")[3] - 0.2, labels = commonword$bigram[1:length(commonword$n)], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
            axis(2,at=seq(0,max(commonword$n),by=5))
            }
            }
            }
    })
####################################################################
# 2 words top 10 capitalised word in similiarity analysis 
####################################################################

    output$my_capitalisedword2<-renderPlot({
            if(nrow(vari1())==0 ){
            plot.new()
        }else{
  
        vocUpper <-vari1()%>% dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 2,to_lower=FALSE) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b") )%>%
            unite("bigram", c(word1, word2), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))%>%filter(str_detect(bigram,pattern="\\b[A-Z]+\\b"))
                  if(length(vocUpper$n)<1){
            plot.new()
            }else{
        if(length(vocUpper$n)>10){
            barplot(vocUpper$n[1:10],main="Top 10 capitalised words",ylab="Counts",axes=F,las=2,col="skyblue")
            text(seq(1,11.8,by=1.2), par("usr")[3] - 0.2, labels = vocUpper$bigram[1:10], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
            axis(2,at=seq(0,max(vocUpper$n),by=5))
        }else{
            barplot(vocUpper$n[1:length(vocUpper$n)],main="Top 10 common words",ylab="Counts",axes=F,las=2,col="skyblue")
            endpoint=1.2*length(vocUpper$n)
            text(seq(1,endpoint,by=1.2), par("usr")[3] - 0.2, labels = vocUpper$bigram[1:length(vocUpper$n)], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
            axis(2,at=seq(0,max(vocUpper$n),by=5))
        }
            }
            }
    })
####################################################################
# 2 words hclust in similiarity analysis 
####################################################################

    output$hclustwords2 <- renderPlot({
            if(nrow(vari1())==0 ){
            plot.new()
        }else{
  
        df2 <-vari1()
        newdf2<-data.frame(df2,id=df2[,"trimedaddress"])
        commonword <-df2%>%
            dplyr::select(description) %>%unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b") )%>%
            unite("bigram", c(word1, word2), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))%>%pull(bigram)
        new<-newdf2%>%unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
            filter(bigram %in% commonword) %>% count(id, bigram) %>% pivot_wider(id_cols = id, names_from = bigram, values_from = n) %>% map_df(replace_na, 0)%>%as.data.frame()
        rownames(new)=new[,1]
        
        df_dist <- parallelDist::parDist(as.matrix(new[, -1]), diag = TRUE)
        df_temp <- new[, -1] %>% transmute_all(function(x) if_else(x > 0, 1, 0))
        rownames(df_temp)=rownames(new)
        df_dist <- parallelDist::parDist(as.matrix(df_temp), method = "binary", diag = TRUE)

        if(nrow(as.matrix(df_dist))<2){
            plot.new()
        }else{
            h.c<-hclust(df_dist, method = "ward.D2")
            cutoff=cutree(h.c,h=max(h.c$height)*0.05)
            subset=table(cutoff)[as.matrix(table(cutoff))>=2]
            toplot= rownames(as.matrix(df_dist))[cutoff%in%rownames(as.matrix(subset))]
            labels <- rownames(as.matrix(df_dist))
            labels[ !(labels %in% toplot) ] <- ""
            plot(h.c,  label=labels,xlab=NULL,ylab=("Distance between the clusters"),main=("Similiarities among descriptions from selected properties"))
            abline(h=max(h.c$height)*0.05,col="red",lty=2)
        }
        }
            
    })
####################################################################
# 2 words listed of properties with high similiar description in similiarity analysis 
####################################################################
    observe({
        df2 <-vari1()
        newdf2<-data.frame(df2,id=df2[,"trimedaddress"])
        commonword <-df2%>%
            dplyr::select(description) %>%unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b") )%>%
            unite("bigram", c(word1, word2), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))%>%pull(bigram)

        new<-newdf2%>%unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
            filter(bigram %in% commonword) %>% count(id, bigram) %>% pivot_wider(id_cols = id, names_from = bigram, values_from = n) %>% map_df(replace_na, 0)%>%as.data.frame()
        rownames(new)=new[,1]

        df_dist <- parallelDist::parDist(as.matrix(new[, -1]), diag = TRUE)
        df_temp <- new[, -1] %>% transmute_all(function(x) if_else(x > 0, 1, 0))
        rownames(df_temp)=rownames(new)
        df_dist <- parallelDist::parDist(as.matrix(df_temp), method = "binary", diag = TRUE)
        if(nrow(as.matrix(df_dist))<2){
            copied<-matrix(NA,0,0)
        }else{
            h.c<-hclust(df_dist, method = "ward.D2")
            cutoff=cutree(h.c,h=max(h.c$height)*0.05)
            subset=table(cutoff)[as.matrix(table(cutoff))>=2]
            if(length(subset)==0){
                copied<-matrix(NA,0,0)
            }else{
                for(i in 1:length(subset)){
                    if(i==1){
                        tmp=rownames(as.matrix(cutoff))[cutoff==rownames(as.matrix(subset))[1]]
                        copied<-data.frame(df2[match(tmp,newdf2[,"id"]),c("address","description","agent")],group=paste("group","1",sep="-"))
                    }else{
                        tmp=rownames(as.matrix(cutoff))[cutoff==rownames(as.matrix(subset))[i]]
                        copied<-rbind(copied,data.frame(df2[match(tmp,newdf2[,"id"]),c("address","description","agent")],group=paste("group",i,sep="-")))
                    }
                }
                colnames(copied)<-c("address","description","agent","similarity group")
            }
        }
####################################################################
# 2 word return text to explain what's items in table in similiarity analysis 
####################################################################
        
        output$text2 <- renderText({
                if(nrow(vari1())==0 ){
             "No property with selection criteria is available."
        }else{
  
            if(nrow(copied)>0){
                "Grouped property decriptions with high similiarities in cluster plot are listed below."
            }else{
                " These are no properities with highly similiar descriptions. Therefore, none of properites are listed below."
            }
            }
        })
        output$datawords2 <-renderDT({copied})
    })
    
####################################################################
# 3 word wordcloud in similiarity analysis 
####################################################################

    output$my_wordcloud3<- renderPlot({
         if(nrow(vari1())==0 ){
            plot.new()
        }else{
  
        my_data <-vari1()%>% dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 3) %>%
            separate(bigram, c("word1", "word2","word3"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !word3 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word3, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word3, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b"),
                   !str_detect(word3, pattern = "\\b(.)\\b")
                   )%>%
            unite("bigram", c(word1, word2,word3), sep = " ")  %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%
            arrange(desc(n))
         if(length(my_data$n)<1){
            plot.new()
            }else{
       
        wordcloud(words = my_data$bigram , freq = my_data$n, min.freq = 1,
                  max.words=100, random.order=FALSE, rot.per=0.35, scale=c(2,.4),
                  colors=brewer.pal(8, "Dark2"))
            }
            }
    })
####################################################################
# 3 words top 10 common word in similiarity analysis 
####################################################################

    output$my_commonword3<- renderPlot({
         if(nrow(vari1())==0 ){
            plot.new()
        }else{
  
        commonword <-vari1()%>% dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 3) %>%
            separate(bigram, c("word1", "word2","word3"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !word3 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word3, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word3, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b"),
                   !str_detect(word3, pattern = "\\b(.)\\b")
                   )%>%
            unite("bigram", c(word1, word2,word3), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))
            if(length(commonword$n)<1){
            plot.new()
            }else{
       
        if(length(commonword$n)>10){
            barplot(commonword$n[1:10],main="Top 10 common words",ylab="Counts",axes=F,las=2,col="skyblue")
            text(seq(1,11.8,by=1.2), par("usr")[3] - 0.2, labels = commonword$bigram[1:10], srt = 45, pos = 2, xpd = TRUE,cex=0.8)
            axis(2,at=seq(0,max(commonword$n),by=5))
        }else{
            barplot(commonword$n[1:length(commonword$n)],main="Top 10 common words",ylab="Counts",axes=F,las=2,col="skyblue")
            endpoint=1.15*length(commonword$n)
            text(seq(1,endpoint,by=1.2), par("usr")[3] - 0.2, labels = commonword$bigram[1:length(commonword$n)], srt = 45, pos = 2, xpd = TRUE,cex=0.6)
            axis(2,at=seq(0,max(commonword$n),by=5))
        }
            }
            }
    })
####################################################################
# 3 words hclust in similiarity analysis 
####################################################################

    output$hclustwords3 <- renderPlot({
         if(nrow(vari1())==0 ){
            plot.new()
        }else{
  
        df2 <-vari1()
        newdf2<-data.frame(df2,id=df2[,"trimedaddress"])
        commonword <-df2%>%
            dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 3) %>%
            separate(bigram, c("word1", "word2","word3"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !word3 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word3, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word3, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b"),
                   !str_detect(word3, pattern = "\\b(.)\\b")
                   )%>%
            unite("bigram", c(word1, word2,word3), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))%>%pull(bigram)

        new<-newdf2%>%unnest_tokens(bigram, description, token = "ngrams", n = 3) %>%
            filter(bigram %in% commonword) %>% count(id, bigram) %>% pivot_wider(id_cols = id, names_from = bigram, values_from = n) %>% map_df(replace_na, 0)%>%as.data.frame()
        rownames(new)=new[,1]

        df_dist <- parallelDist::parDist(as.matrix(new[, -1]), diag = TRUE)
        df_temp <- new[, -1] %>% transmute_all(function(x) if_else(x > 0, 1, 0))
        rownames(df_temp)=rownames(new)
        df_dist <- parallelDist::parDist(as.matrix(df_temp), method = "binary", diag = TRUE)
        if(nrow(as.matrix(df_dist))<2){
            plot.new()
        }else{
            h.c<-hclust(df_dist, method = "ward.D2")
            cutoff=cutree(h.c,h=max(h.c$height)*0.05)
            subset=table(cutoff)[as.matrix(table(cutoff))>=2]
            toplot= rownames(as.matrix(df_dist))[cutoff%in%rownames(as.matrix(subset))]
            labels <- rownames(as.matrix(df_dist))
            labels[ !(labels %in% toplot) ] <- ""
            plot(h.c,  label=labels,xlab=NULL,ylab=("Distance between the clusters"),main=("Similiarities among descriptions from selected properties"))
            abline(h=max(h.c$height)*0.05,col="red",lty=2)
        }
            }
    })
####################################################################
# 3 words listed of properties with high similiar description in similiarity analysis 
####################################################################
    observe({
        df2 <-vari1()
        newdf2<-data.frame(df2,id=df2[,"trimedaddress"])
        commonword <-df2%>%
            dplyr::select(description) %>%
            unnest_tokens(bigram, description, token = "ngrams", n = 3) %>%
            separate(bigram, c("word1", "word2","word3"), sep = " ") %>% # split bi-grams into 2 words
            filter(!word1 %in% stop_words$word,
                   !word2 %in% stop_words$word,
                   !word3 %in% stop_words$word,
                   !str_detect(word1, pattern= "[[:digit:]]"),
                   !str_detect(word2, pattern = "[[:digit:]]"),
                   !str_detect(word3, pattern = "[[:digit:]]"),
                   !str_detect(word1, pattern =  "[^\u0001-\u007F]+"),
                   !str_detect(word2, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word3, pattern = "[^\u0001-\u007F]+"),
                   !str_detect(word1, pattern = "\\b(.)\\b"),
                   !str_detect(word2, pattern = "\\b(.)\\b"),
                   !str_detect(word3, pattern = "\\b(.)\\b")
                   )%>%
            unite("bigram", c(word1, word2,word3), sep = " ") %>% count(bigram) %>%
            filter(n >= 3) %>%group_by(bigram) %>%arrange(desc(n))%>%pull(bigram)

        new<-newdf2%>%unnest_tokens(bigram, description, token = "ngrams", n = 3) %>%
            filter(bigram %in% commonword) %>% count(id, bigram) %>% pivot_wider(id_cols = id, names_from = bigram, values_from = n) %>% map_df(replace_na, 0)%>%as.data.frame()
        rownames(new)=new[,1]

        df_dist <- parallelDist::parDist(as.matrix(new[, -1]), diag = TRUE)
        df_temp <- new[, -1] %>% transmute_all(function(x) if_else(x > 0, 1, 0))
        rownames(df_temp)=rownames(new)
        df_dist <- parallelDist::parDist(as.matrix(df_temp), method = "binary", diag = TRUE)
        if(nrow(as.matrix(df_dist))<2){
            copied<-matrix(NA,0,0)
        }else{
            h.c<-hclust(df_dist, method = "ward.D2")
            cutoff=cutree(h.c,h=max(h.c$height)*0.05)
            subset=table(cutoff)[as.matrix(table(cutoff))>=2]
            if(length(subset)==0){
                copied<-matrix(NA,0,0)
            }else{
                for(i in 1:length(subset)){
                    if(i==1){
                        tmp=rownames(as.matrix(cutoff))[cutoff==rownames(as.matrix(subset))[1]]
                        copied<-data.frame(df2[match(tmp,newdf2[,"id"]),c("address","description","agent")],group=paste("group","1",sep="-"))
                    }else{
                        tmp=rownames(as.matrix(cutoff))[cutoff==rownames(as.matrix(subset))[i]]
                        copied<-rbind(copied,data.frame(df2[match(tmp,newdf2[,"id"]),c("address","description","agent")],group=paste("group",i,sep="-")))
                    }
                }
                colnames(copied)<-c("address","description","agent","similarity group")
            }
        }
####################################################################
# 3 words return text to explain what's items in table in similiarity analysis 
####################################################################

        output$text3 <- renderText({
                    if(nrow(vari1())==0 ){
             "No property with selection criteria is available."
                    }else{
                        if( nrow(df_dist)==0){
                            "There are not enough data after filtering."
                            }else{
  
            if(nrow(copied)>0){
                "Grouped property decriptions with high similiarities in cluster plot are listed below."
            }else{
                " These are no properities with highly similiar descriptions. Therefore, none of properites are listed below."
            }
                            }
                        }
        })

        output$datawords3 <-renderDT({copied})
    })
}

shinyApp(ui = ui, server = server)

