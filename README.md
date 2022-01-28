# Rshiny-ApartmentPrice
This Rshiny dashboard is for scrapping real-time apartment information in major cities in Austrilia from public website (domain.com.au),listing apartment information, predicting apartment prices and analysing the similiaries between property descriptions. 

#TO run in R:

source("project.R")

shinyApp(ui = ui, server = server)

Note: this is dashboard runs slow because it need to scrape real time informaiton and using googld API. To explore the functions in advance, please feel free to see the video demo (mp4).
