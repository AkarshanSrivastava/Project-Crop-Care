library(plyr)
library(Amelia)
library(psych)
library(lubridate)
return_data <- read.csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//Returns.csv")
return_data <- return_data[,-c(4,5,6)]
sort(table(return_data$Dealer.ID))
##top 5 dealer in return
#Del1191 Del1397  Del114 Del1223  Del121 
# 20      20      22      26      35 
sort(table(return_data$Quantity))
##top 5 quantity in return
#1  10  50  20  40
#58  59  61  79 183 
sort(table(return_data$Products))
#top 5 product
#XPLODE - 250 ML (1X40)40
#KHAJANRBIO-SP 150 GM (1X40) 48
#EFFECTP- 250 ML (1X40)51 
#SHAKTI- 1 LTR (1X10)55
#MADALI-100GRX40 59 
### EDA
return_data$Date <- mdy(return_data$Date)
str(return_data)
## BM
describe.by(return_data)
colSums(is.na(return_data))
##so there is no na value 

returns_count <- ddply(return_data,.(Dealer.ID),summarize,sum(Value),frequency=length(Dealer.ID))
View(returns_count)

sort_value<- returns_count[order(returns_count$..1,decreasing = TRUE),]
View(sort_value)


sort_freq <- returns_count[order(returns_count$frequency,decreasing = TRUE),]
View(sort_freq)



returns_count_product <- ddply(return_data,.(Products),summarize,sum(Quantity),frequency=length(Products))
View(returns_count_product)

sort_product_value <- returns_count_product[order(returns_count_product$..1,decreasing = TRUE),]
View(sort_product_value)


sort_product_freq <- returns_count_product[order(returns_count_product$frequency,decreasing = TRUE),]
View(sort_product_freq)


attach(return_data)
library(rmarkdown)
library(tidyr)
library(knitr)
library(rmarkdown)
library(dplyr)

max_Date <- max(Date)

## now i am going to calculate the Recency
Recency <- return_data %>% 
  group_by(Dealer.ID) %>% 
  summarise(recency=as.numeric(as.Date("2018-03-30")-max(Date)))

View(Recency)
summary(Recency)


library(plyr)
frequency <- ddply(return_data,.(Dealer.ID,Date),summarize,sum_freq=sum(Value),frequency=length(Dealer.ID))

View(frequency)
str(frequency)
Frequency <- ddply(frequency,.(Dealer.ID),summarize,frequency=length(Dealer.ID))
View(Frequency)



###Monetary #######
#Sum the amount of money a customer spent and divide it by Frequency,
#to get the amount per transaction on average, that is the Monetary data.
sum_value <- ddply(return_data,.(Dealer.ID),summarize,sum_value=sum(Value))
View(sum_value)
## frequency with sum value 
sales_F_M<- merge(Frequency, sum_value, by="Dealer.ID")
View(sales_F_M)
monetary <- sales_F_M$sum_value/sales_F_M$frequency

sales_F_M$monetary <-sales_F_M$sum_value / sales_F_M$frequency

View(sales_F_M)
#We have calculated monetary.
#we remove the sum_value column
sales_F_M <- sales_F_M[,-3]
View(sales_F_M)
# We interested in Recency, Frequency and Monetary###
rfm_result <- cbind(Recency,sales_F_M)
View(rfm_result)
rfm_result <- rfm_result[,-c(3)]
View(rfm_result)
#setwd("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets")
#getwd()
#write.csv(rfm_result,file = "returnsrfm.csv")
