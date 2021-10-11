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





#Average minutes
callsAverage<-calls%>%
  group_by(call_timestamp)%>%
  mutate(totalDay = sum(call.duration.in.minutes))%>%
  mutate(averageTime = round(mean(call.duration.in.minutes), digits = 2))





# Cards






#Filters

call_center_choices<-calls%>%select(call_center)%>%distinct()%>%arrange(call_center)



#############################################       Shiny        ##################################

#Body
body<-dashboardBody(
  tabItems(
    tabItem(tabName="Overview",
            fluidRow(
              valueBoxOutput("value_box_calls_count")
              ,
              valueBoxOutput("value_box_total_minutes")
              ,
              valueBoxOutput("value_box_avg_call_duration")
              
            ),
            
            fluidRow(
              ### Calls by call center 
              column(width=12,
                     box(status="info",solidHeader=TRUE,title="Calls by call center",
                         highchartOutput("calls_by_call_center"),width=12,height=500)
              )
            )
            
    ),
    
    
    ##Segundo dashboard Details  
    
    
    tabItem(tabName="Details",
            fluidRow(
              column(width=6,
                     box(status="info",solidHeader=TRUE,title="Calls received",
                         highchartOutput("Calls_received"),width="100%",height="500px")
              )
              ,    
              column(width=6,
                     
                     box(status="info",solidHeader=TRUE,title="Average calls duration",
                         highchartOutput("Average_calls_duration"),width="100%",height="500px")
              )
            )
            
            
            ,
            fluidRow(
              column(width=6,
                     box(status="info",solidHeader=TRUE,title="Response",
                         highchartOutput("response"),width="100%",height="500px")
                     
              ),  
              column(width=6,
                     box(status="info",solidHeader=TRUE,title="Reasons",
                         highchartOutput("reasons"),width="100%",height="500px")
              )
            ),
            fluidRow(
              column(width=6,
                     box(status="info",solidHeader=TRUE,title="Sentiments",
                         highchartOutput("sentiments"),width="100%",height="500px")
                     
              ),
              column(width=6,
                     box(status="info",solidHeader=TRUE,title="Channel",
                         highchartOutput("channels"),width="100%",height="500px")
                     
                     
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
    inputId = "Call_center",
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
  #Card call counts
  
  output$value_box_calls_count<- renderValueBox({
    call_count<-format(calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%tally(),big.mark=",")
    
    valueBox(
      value = call_count,
      subtitle = "Calls count", 
      icon = icon("phone-square-alt")
    )
    
  })
  
  
  
  output$value_box_total_minutes<- renderValueBox({
    
    total_minutes<-format(calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%
                            summarise(total_minutes=sum(call.duration.in.minutes)), big.mark=",")
    
    valueBox(
      value = total_minutes,
      subtitle = "Total Minutes", 
      icon = icon("clock")
    )
    
  })
  
  
  
  
  output$value_box_avg_call_duration<- renderValueBox({
    avg_calls_duration<-round(calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))
                              %>%summarise(avg_calls_duration=mean(call.duration.in.minutes)),2)
    
    valueBox(
      value = avg_calls_duration,
      subtitle = "Calls duration average", 
      icon = icon("user-clock")
    )
    
  })
  
  
  
  #Graph call center
  output$calls_by_call_center <-renderHighchart ({
    
    
    
    #calls per center
    callsPerCenter<-calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%
      group_by(call_center)%>%
      tally()%>%
      arrange(-n)
    
    callsPerCenter%>% 
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
    
  })   
  
  
  
  #Graph Calls received
  output$Calls_received <-renderHighchart ({
    
    
    #Day
    callsDay<-calls%>%filter(call_center==input$Call_center)%>%
      group_by(call_timestamp)%>%
      tally()
    
    
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
    
    
    
  })   
  
  
  
  #Graph average calls duration
  output$Average_calls_duration <-renderHighchart ({
    
    
    #Day
    trendAverage<-callsAverage%>%filter(call_center==input$Call_center)%>%
      group_by(call_timestamp, averageTime)%>%
      tally()%>%
      rename(callsTotalDay = n)
    
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
    
    
  })             
  
  
  
  
  
  
  
  
  #Response
  output$response <-renderHighchart ({
    
    
    #Response
    callsResponse<-calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%
      filter(call_center==input$Call_center)%>%
      group_by(response_time)%>%
      tally()%>%
      arrange(-n)
    
    
    
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
    
    
  })             
  
  
  
  
  #Reasons
  output$reasons <-renderHighchart ({
    
    
    
    
    #Reasons
    callsReason<-calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%
      filter(call_center==input$Call_center)%>%
      group_by(reason)%>%
      tally()%>%
      arrange(-n)
    
    
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
    
    
    
  })             
  
  
  
  
  #Sentiments
  output$sentiments <-renderHighchart ({
    
    
    
    
    #Sentiments
    callsSentiment<-calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%
      filter(call_center==input$Call_center)%>%
      group_by(sentiment)%>%
      tally()%>%
      arrange(-n)
    
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
    
    
    
  })             
  
  
  
  
  #Canal
  output$channels <-renderHighchart ({
    
    
    
    #Canal
    callsChannel<-calls%>%filter(call_timestamp<=as.Date(input$range[2]) & call_timestamp>=as.Date(input$range[1]))%>%
      filter(call_center==input$Call_center)%>%
      group_by(channel)%>%
      tally()%>%
      arrange(-n)
    
    
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
    
    
    
  })       
  
  
  
  
  
  
  
  
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)