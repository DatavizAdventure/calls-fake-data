#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(bslib)
library(thematic)
library(dplyr)
#shinyWidgetsGallery()

calls <- read.csv("https://query.data.world/s/alswcm6adfxovl2arfijlst6hkhmxn", header=TRUE, stringsAsFactors=FALSE)
calls$call_timestamp<-as.Date(calls$call_timestamp,format="%m/%d/%Y")
# Define UI for application that draws a histogram

##################################        Wrangling     ###################################
#calls per center
callsPerCenter<-calls%>%
    group_by(call_center)%>%
    tally()%>%
    arrange(-n)

#Reasons
callsReason<-calls%>%
    group_by(reason)%>%
    tally()%>%
    arrange(-n)

#Response
callsResponse<-calls%>%
    group_by(response_time)%>%
    tally()%>%
    arrange(-n)


#Sentiment
callsSentiment<-calls%>%
    group_by(sentiment)%>%
    tally()%>%
    arrange(-n)


#Canal
callsChannel<-calls%>%
    group_by(channel)%>%
    tally()%>%
    arrange(-n)


#Day
callsDay<-calls%>%
    group_by(call_timestamp)%>%
    tally()


#Average minutes
callsAverage<-calls%>%
    group_by(call_timestamp)%>%
    mutate(totalDay = sum(call.duration.in.minutes))%>%
    mutate(averageTime = round(mean(call.duration.in.minutes), digits = 2))

trendAverage<-callsAverage%>%
    group_by(call_timestamp, averageTime)%>%
    tally()%>%
    rename(callsTotalDay = n)



# Cards
call_count<-format(calls%>%tally(),big.mark=",")

total_minutes<-format(calls%>%summarise(total_minutes=sum(call.duration.in.minutes)), big.mark=",")

avg_calls_duration<-round(calls%>%summarise(avg_calls_duration=mean(call.duration.in.minutes)),2)


#Filters

call_center_choices<-calls%>%select(call_center)%>%distinct()%>%arrange(call_center)



#############################################       Shiny        ##################################

#Body
body<-dashboardBody(
    tabItems(
        tabItem(tabName="Overview",
            fluidRow(
                    valueBox(
                            value = call_count,
                            subtitle = "Calls count", 
                            icon = icon("fire")
                        ),
                        valueBox(
                            value = total_minutes,
                            subtitle = "Total Minutes", 
                            icon = icon("fire")
                        ),
                    valueBox(
                        value = avg_calls_duration,
                        subtitle = "Calls duration average", 
                        icon = icon("fire")
                    )
       
                    ),
                    
            fluidRow(
                ### Calls by call center 
                column(width=12,
                       callsPerCenter %>%
                    hchart(type = "bar",
                           hcaes(x = call_center, y = n, color = c(
                               "#554089", 
                               "#4E7CBF", 
                               "#4BBEC5", 
                               "#FCBE78"
                           )),
                           name = "Calls") %>%
                    hc_yAxis(
                        title = list(text = ""),
                        style = list(
                            fontFamily = "Roboto",
                            fontSize = "12px",
                            color = "#666666"
                        )
                    ) %>%
                    hc_xAxis(title = list(text = ""), labels = list(
                        style = list(
                            fontFamily = "Roboto",
                            fontSize = "12px",
                            color = "#333333"
                        )
                    )) %>%  hc_legend(enabled = FALSE)%>% hc_title(
                        text = "Calls by call center", align = "left")
                     )
            )
        ),
        
        
    tabItem(tabName="Details",
            fluidRow(
                column(width=6,
                       callsDay%>%
                         hchart(type = "line",
                                hcaes(x = call_timestamp, y = n),
                                name = "Calls",
                                color = "#4E7CBF")%>%
                         hc_yAxis(title = list(text = ""),
                                  style = list(
                                      fontFamily = "Roboto",
                                      fontSize = "12px",
                                      color = "#666666"
                                  )
                         ) %>%
                         hc_xAxis(title = list(text = ""), labels = list(
                             style = list(
                                 fontFamily = "Roboto",
                                 fontSize = "12px",
                                 color = "#333333"
                             ) 
                         ))%>%hc_legend(enabled = FALSE)%>% hc_title(text = "Calls received", align = "left")
                       )
                     ,
                column(width=6,     
                        trendAverage%>%
                            hchart(type = "line",
                                   hcaes(x = call_timestamp, y = averageTime),
                                   name = "Average",
                                   color = "#388F94")%>%
                            hc_yAxis(title = list(text = ""),
                                     style = list(
                                         fontFamily = "Roboto",
                                         fontSize = "12px",
                                         color = "#666666"
                                     )
                            ) %>%
                            hc_xAxis(title = list(text = ""), labels = list(
                                style = list(
                                    fontFamily = "Roboto",
                                    fontSize = "12px",
                                    color = "#333333"
                                ) 
                            ))%>%hc_legend(enabled = FALSE)%>% hc_title(text = "Average calls duration", align = "left")
                )
                
                
            ),
            fluidRow(
                column(width=6,
                       callsResponse%>%
                           hchart(type = "bar",
                                  hcaes(x = response_time, y = n, color = c(
                                      "#554089", 
                                      "#4E7CBF", 
                                      "#4BBEC5"
                                  )),
                                  name = "Calls")%>%
                           hc_yAxis(title = list(text = ""),
                                    style = list(
                                        fontFamily = "Roboto",
                                        fontSize = "12px",
                                        color = "#666666"
                                    )
                           ) %>%
                           hc_xAxis(title = list(text = ""), labels = list(
                               style = list(
                                   fontFamily = "Roboto",
                                   fontSize = "12px",
                                   color = "#333333"
                               ) 
                           ))%>%  hc_legend(enabled = FALSE)%>% hc_title(text = "Response", align = "left")
                       
                       ),
                column(width=6,
                       callsReason%>%
                           hchart(type = "bar",
                                  hcaes(x = reason, y = n, color = c(
                                      "#554089", 
                                      "#4E7CBF", 
                                      "#388F94"
                                  )),
                                  name = "Answers")%>%
                           hc_yAxis(title = list(text = ""),
                                    style = list(
                                        fontFamily = "Roboto",
                                        fontSize = "12px",
                                        color = "#666666"
                                    )
                           ) %>%
                           hc_xAxis(title = list(text = ""), labels = list(
                               style = list(
                                   fontFamily = "Roboto",
                                   fontSize = "12px",
                                   color = "#333333"
                               ) 
                           ))%>%  hc_legend(enabled = FALSE)%>% hc_title(text = "Reasons", align = "left")
                    )
                ),
            fluidRow(
                column(width=6,
                       callsSentiment%>%
                           hchart(type = "bar",
                                  hcaes(x = sentiment, y = n, color = c(
                                      "#554089", 
                                      "#4E7CBF", 
                                      "#388F94", 
                                      "#4BBEC5",
                                      "#FCBE78"
                                  )),
                                  name = "Answers")%>%
                           hc_yAxis(title = list(text = ""),
                                    style = list(
                                        fontFamily = "Roboto",
                                        fontSize = "12px",
                                        color = "#666666"
                                    )
                           ) %>%
                           hc_xAxis(title = list(text = ""), labels = list(
                               style = list(
                                   fontFamily = "Roboto",
                                   fontSize = "12px",
                                   color = "#333333"
                               ) 
                           ))%>%  hc_legend(enabled = FALSE)%>% hc_title(text = "Sentiment", align = "left")
                       
                       ),
                column(width=6,
                       callsChannel%>%
                           hchart(type = "bar",
                                  hcaes(x = channel, y = n, color = c(
                                      "#554089", 
                                      "#4E7CBF", 
                                      "#388F94", 
                                      "#4BBEC5"
                                  )),
                                  name = "Calls")%>%
                           hc_yAxis(title = list(text = ""),
                                    style = list(
                                        fontFamily = "Roboto",
                                        fontSize = "12px",
                                        color = "#666666"
                                    )
                           ) %>%
                           hc_xAxis(title = list(text = ""), labels = list(
                               style = list(
                                   fontFamily = "Roboto",
                                   fontSize = "12px",
                                   color = "#333333"
                               ) 
                           ))%>%  hc_legend(enabled = FALSE)%>% hc_title(text = "Channel", align = "left")
                       
                       
                       )
      
            )
            
        )
    )
)
    
    





#Sidebar
sidebar<-dashboardSidebar(
                    sidebarMenu(
                        menuItem("Overview", tabName = "Overview")
                             ),
                    sidebarMenu(
                        menuItem("Details", tabName = "Details")
                                ),
                    pickerInput(
                        inputId = "Id081",
                        label = "Call center", 
                        choices = call_center_choices
                            ),
                    dateRangeInput("range", "Range:",
                               start = min(calls$call_timestamp), end = max(calls$call_timestamp),
                               min=min(calls$call_timestamp),max=max(calls$call_timestamp),
                               format("dd/mm/yyyy"),separator="-"
                            )
                    
                )



#Header

header<-dashboardHeader(
    title = "Call Center Report - October 2020",titleWidth  = 350
)


    
    
ui <- dashboardPage(skin="purple",
                    header=header,
                    sidebar=sidebar,
                    body=body)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
