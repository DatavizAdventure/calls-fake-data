library(dplyr)
library(ggplot2)
library(rpart)


df <- read.csv("https://query.data.world/s/alswcm6adfxovl2arfijlst6hkhmxn", header=TRUE, stringsAsFactors=FALSE)

                                            ########  EDA  #######
colnames(df)

str(df)



df%>%select(city)%>%distinct()  #462

df%>%select(state)%>%distinct() #51

df%>%select(channel)%>%distinct  #4

df%>%select(call_center)%>%distinct  #4

df%>%select(response_time)%>%distinct  #3

df%>%select(sentiment)%>%distinct  #5

df%>%select(reason)%>%distinct()  #3

df%>%select(call_timestamp)%>%summarise(max=max(call_timestamp),min=min(call_timestamp))

df%>%select(call_timestamp)%>%summarise(max=max(call_timestamp),min=min(call_timestamp))




ggplot(df,aes(sentiment,call.duration.in.minutes))+geom_violin()

#interesante
ggplot(df,aes(call.duration.in.minutes))+geom_histogram()+facet_grid(call_center~sentiment)

