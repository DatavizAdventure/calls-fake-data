library(dplyr)
library(ggplot2)
library(rpart)


df <- read.csv("https://query.data.world/s/alswcm6adfxovl2arfijlst6hkhmxn", header=TRUE, stringsAsFactors=FALSE)

                                            ########  EDA  #######
colnames(df)

str(df)

df%>%select(reason)%>%distinct()  #3

df%>%select(city)%>%distinct()  #462

df%>%select(state)%>%distinct() #51

df%>%select(channel)%>%distinct  #4

df%>%select(call_center)%>%distinct  #4

df%>%select(response_time)%>%distinct  #3

df%>%select(sentiment)%>%distinct  #5


#### Classification tree

data=sample(1,700,replace=TRUE)


df_train <-df[assign==1,]
dt_test <- df[assign==2,]


rpart(sentiment~call_center+reason+state+city+response_time+call.duration.in.minutes)





