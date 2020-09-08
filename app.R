library(shiny)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(memisc)
library(googlesheets4)
library(dplyr)

gs4_deauth()

muni <- read_sheet("1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4", sheet = "Municipality")
muni <- muni[c(1:39),]

# Longitude and latitude of Rhode Island cities
muni$Longitude <- c(-71.307991, -71.259804, -71.677200, -71.387459, -71.611969,
                    -71.5642, -71.4373, -71.4062, -71.5130, -71.3701,
                    -71.5351, -71.7581, -71.6911, -71.7776, -71.3712,
                    -71.5126, -71.4418, -71.1712, -71.2835, -71.4495,
                    -71.5576, -71.3128, -71.4537, -71.4677, -71.5495,
                    -71.3826, -71.2503, -71.4128, -71.6708, -71.6199,
                    -71.5495, -71.5247, -71.2134, -71.2825, -71.4162,
                    -71.6638, -71.5215, -71.8273, -71.5148)

muni$Latitude <- c(41.739891, 41.677071, 41.966221, 41.891701, 41.429138,
                   41.6886, 41.7798, 41.9722, 41.6343, 41.8137,
                   41.5751, 41.8537, 41.9043, 41.4613, 41.4963,
                   41.8205, 41.9110, 41.5101, 41.5218, 41.4501,
                   41.1721, 41.4901, 41.5568, 41.8536, 41.9668,
                   41.8787, 41.6023, 41.8240, 41.4945, 41.7964,
                   41.9220, 41.4476, 41.6259, 41.7304, 41.7001,
                   41.6285, 41.7037, 41.3776, 42.0029)

demo <- read_sheet("1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4", sheet = "Demographics")
demo <- demo[,-c(3,5,7,9)]
colnames(demo)[1:2] <- c("Category", "All People Tested")
sex <- demo[c(2:4),]
colnames(sex)[1] <- "Sex"
age <- demo[c(7:19),]
colnames(age)[1] <- "Age Group"
age$`Age Group`[2:3] <- c("5-9", "10-14")
race <- demo[c(22:28),]
colnames(race)[1] <- "Race Ethnicity"
race$`Race Ethnicity` <- c("Hispanic or Latino", "American Indian or Alaska Native", "Asian", 
                           "Black or African American", "White", "Other race", "Multiple race")
race <- race %>% mutate(Hospitalizations = ifelse(Hospitalizations == "<5", 0, Hospitalizations)) %>% mutate(Deaths = ifelse(Deaths == "<5", 0, Deaths))

trends <- read_sheet("1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4", sheet = "Trends")
colnames(trends)[2] <- "Daily number of positive tests"

ui <- fluidPage(
    
    theme = shinytheme("superhero"),
    
    # Page title
    titlePanel("RIDOH COVID-19 Data Visualizations"),
    
    # Data last updated
    helpText(paste("Updated:", trends$Date[nrow(trends)])),
    
    # Link to source data
    helpText( "Source data provided by the "
              , a("Rhode Island Department of Health", href = "https://health.ri.gov/covid/")
              , "."
    ),
    
    helpText("COVID-19 cases and COVID-19 associated deaths exclude out-of-state residents. 
             COVID-19 hospitalizations include all patients hospitalized in Rhode Island, 
             some of whom may not be Rhode Island residents. 
             For cases, a count and rate of zero indicates either zero cases or fewer than 
             five cases in that municipality. For hositalizations and deaths, counts of less 
             than five are shown as <5. Counts of less than five are suppressed in accordance 
             with RIDOH's Small Numbers Policy."),
    
    
    
    tabsetPanel(
        
        tabPanel("Cases", 
                 sidebarLayout( 
                     sidebarPanel(
                         radioButtons(inputId = "casesbuttons", 
                                      label = "Cases", 
                                      choices = c("Total cases", "Rate of cases per 100,000 population")),
                         helpText("Total cases in Rhode Island: 19,880"),
                         helpText("Rate of cases per 100,000 population in Rhode Island: 1,881")),
                     mainPanel(leafletOutput(outputId = "cases"))
                 ), 
        ),
        
        tabPanel("Hospitalizations",  
                 sidebarLayout( 
                     sidebarPanel( 
                         radioButtons(inputId = "hospbuttons", 
                                      label = "Hospitalizations", 
                                      choices = c("Total hospitalizations", "Rate of hospitalizations per 100,000 population")), 
                         helpText("Total hospitalizations in Rhode Island: 2,094"),
                         helpText("Rate of hospitalizations per 100,000 population in Rhode Island: 198")),
                     mainPanel(leafletOutput(outputId = "hosp"))),
        ),
        
        tabPanel("Deaths", 
                 sidebarLayout( 
                     sidebarPanel( 
                         radioButtons(inputId = "deathsbuttons", 
                                      label = "Deaths", 
                                      choices = c("Total deaths", "Rate of deaths per 100,000 population")), 
                         helpText("Total deaths in Rhode Island: 1,022"),
                         helpText("Rate of deaths per 100,000 population in Rhode Island: 97")),
                     mainPanel(leafletOutput(outputId = "deaths")))
        ),
        
        tabPanel("Demographics",
                 sidebarLayout( 
                     sidebarPanel( 
                         radioButtons(inputId = "demobuttons",
                                      label = "Demographics",
                                      choices = c("Sex", "Age", "Race"))),
                     mainPanel(plotOutput(outputId = "demographics"))),
        ),
        
        tabPanel("Trends", plotOutput(outputId = "trends"))
        
    )
    
)

server <- function(input, output, session) {
    
    output$cases <- renderLeaflet({
        
        casesbuttons <- switch(input$casesbuttons,
                               
                               `Total cases` = leaflet(muni) %>% 
                                   setView(lng = -71.418884, lat = 41.7, zoom = 9)  %>% #setting the view over RI
                                   addTiles() %>%
                                   addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = "red",
                                              radius = ~sqrt(as.numeric(as.character(`Total cases`))) * 50, 
                                              popup = ~paste(sep = "<br/>", 
                                                             `Municipality of residence`, 
                                                             paste("Total cases: ", `Total cases`))),
                               
                               `Rate of cases per 100,000 population` = leaflet(muni) %>% 
                                   setView(lng = -71.418884, lat = 41.7, zoom = 9)  %>%
                                   addTiles() %>%
                                   addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = "orange",
                                              radius = ~sqrt(as.numeric(as.character(`Rate of COVID-19 cases per 100,000 population`))) * 50, 
                                              popup = ~paste(sep = "<br/>", 
                                                             `Municipality of residence`, 
                                                             paste("Rate of cases per 100,000 population: ", `Rate of COVID-19 cases per 100,000 population`))))
        
        
    })
    
    output$hosp <- renderLeaflet({
        
        hospbuttons <- switch(input$hospbuttons,
                              
                              `Total hospitalizations` = leaflet(muni) %>% 
                                  setView(lng = -71.418884, lat = 41.7, zoom = 9)  %>%
                                  addTiles() %>%
                                  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = "red",
                                             radius = ~sqrt(as.numeric(as.character(`Total hospitalizations`))) * 50, 
                                             popup = ~paste(sep = "<br/>", 
                                                            `Municipality of residence`, 
                                                            paste("Total hospitalizations: ", `Total hospitalizations`))),
                              
                              `Rate of hospitalizations per 100,000 population` = leaflet(muni) %>% 
                                  setView(lng = -71.418884, lat = 41.7, zoom = 9)  %>%
                                  addTiles() %>%
                                  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = "orange",
                                             radius = ~sqrt(as.numeric(as.character(`Rate of hospitalizations per 100,000 population`))) * 50, 
                                             popup = ~paste(sep = "<br/>", 
                                                            `Municipality of residence`, 
                                                            paste("Rate of hospitalizations per 100,000 population: ", `Rate of hospitalizations per 100,000 population`))))
        
        
    })
    
    output$deaths <- renderLeaflet({
        
        deathsbuttons <- switch(input$deathsbuttons,
                                
                                `Total deaths` = leaflet(muni) %>% 
                                    setView(lng = -71.418884, lat = 41.7, zoom = 9)  %>%
                                    addTiles() %>%
                                    addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = "red",
                                               radius = ~sqrt(as.numeric(as.character(`Total deaths`))) * 50, 
                                               popup = ~paste(sep = "<br/>", 
                                                              `Municipality of residence`, 
                                                              paste("Total deaths: ", `Total deaths`))),
                                
                                `Rate of deaths per 100,000 population` = leaflet(muni) %>% 
                                    setView(lng = -71.418884, lat = 41.7, zoom = 9)  %>%
                                    addTiles() %>%
                                    addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, color = "orange",
                                               radius = ~sqrt(as.numeric(as.character(`Rate of deaths per 100,000 population`))) * 50, 
                                               popup = ~paste(sep = "<br/>", 
                                                              `Municipality of residence`, 
                                                              paste("Rate of deaths per 100,000 population: ", `Rate of deaths per 100,000 population`))))
        
        
    })
    
    output$demographics <- renderPlot({
        
        demobuttons <- switch(input$demobuttons,
                              
                              Sex = {par(mfrow=c(2,2))
                                  pie(main = "All People Tested", as.numeric(as.character(sex$`All People Tested`)), labels = paste(sex$Sex, round(as.numeric(as.character(sex$`All People Tested`))/sum(as.numeric(as.character(sex$`All People Tested`))), digits = 2) * 100, "%"))
                                  pie(main = "Cases", as.numeric(as.character(sex$Cases)), labels = paste(sex$Sex, round(as.numeric(as.character(sex$Cases))/sum(as.numeric(as.character(sex$Cases))), digits = 2) * 100, "%"))
                                  pie(main = "Hospitalizations", as.numeric(as.character(sex$Hospitalizations[1:2])), labels = paste(sex$Sex[1:2], round(as.numeric(as.character(sex$Hospitalizations[1:2]))/sum(as.numeric(as.character(sex$Hospitalizations[1:2]))), digits = 2) * 100, "%"))
                                  pie(main = "Deaths", as.numeric(as.character(sex$Deaths)), labels = paste(sex$Sex, round(as.numeric(as.character(sex$Deaths))/sum(as.numeric(as.character(sex$Deaths))), digits = 2) * 100, "%"))
                              },
                              
                              Age = {par(mfrow=c(2,2))
                                  age$`Age Group` <- factor(age$`Age Group`, levels = age$`Age Group`)
                                  barplot(main = "All People Tested", xlab = "Age Group", ylab = "# of People Tested", as.numeric(as.character(age$`All People Tested`)), names.arg = age$`Age Group`, las = 2, col = "lightsteelblue1")
                                  barplot(main = "Cases", xlab = "Age Group", ylab = "# of Cases", as.numeric(as.character(age$Cases)), names.arg = age$`Age Group`, las = 2, col = "lightsteelblue1")
                                  barplot(main = "Hospitalizations", xlab = "Age Group", ylab = "# of Hospitalizations", as.numeric(as.character(age$Hospitalizations)), names.arg = age$`Age Group`, las = 2, col = "lightsteelblue1")
                                  barplot(main = "Deaths", xlab = "Age Group", ylab = "# of Deaths", as.numeric(as.character(age$Deaths)), names.arg = age$`Age Group`, las = 2, col = "lightsteelblue1")
                              },
                              
                              Race = {par(mfrow=c(2,2))
                                  pie(main = "All People Tested", as.numeric(as.character(race$`All People Tested`)), labels = paste(race$`Race Ethnicity`, round(as.numeric(as.character(race$`All People Tested`))/sum(as.numeric(as.character(race$`All People Tested`))), digits = 2) * 100, "%"))
                                  pie(main = "Cases", as.numeric(as.character(race$Cases)), labels = paste(race$`Race Ethnicity`, round(as.numeric(as.character(race$Cases))/sum(as.numeric(as.character(race$Cases))), digits = 2) * 100, "%"))
                                  pie(main = "Hospitalizations", as.numeric(as.character(race$Hospitalizations)), labels = paste(race$`Race Ethnicity`, round(as.numeric(as.character(race$Hospitalizations))/sum(as.numeric(as.character(race$Hospitalizations))), digits = 2) * 100, "%"))
                                  pie(main = "Deaths", as.numeric(as.character(race$Deaths)), labels = paste(race$`Race Ethnicity`, round(as.numeric(as.character(race$Deaths))/sum(as.numeric(as.character(race$Deaths))), digits = 2) * 100, "%"))
                              }
                              
        )
        
    })
    
    output$trends <- renderPlot(
        
        barplot(main = "Daily Number of Positive Tests", xlab = "Date", ylab = "# of Positive Tests", trends$`Daily number of positive tests`, names.arg = trends$Date, las = 2, cex.names = 0.5, col = "skyblue")
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
