library(Amelia)
library(psych)
library(lubridate)
library(VIM)
library(e1071)
library(mice)
library(Hmisc)
library(data.table)
library(dplyr)
library(car)

#library(ggplot2)
#library(stringr)
#library(DT)
#library(tidyr)
#library(knitr)
#library(rmarkdown)

sales <- read.csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//finalsales.csv")
########EDA#########
View(sales)
## remove x
sales <- sales[,c(-1)]
str(sales)
## change the format of date
sales$Date <-mdy(sales$Date) 
## change the clm name
setnames(sales,c("Dealer.ID"),c("dealer"))
#sales$Date <- as.Date(sales$Date,format="%d/%m/%Y")
#order(sales$Date)
sum(is.na(sales$Date))
sum(is.na(sales))
table(is.na(sales))


colSums(is.na(sales))
## 5 missing valiue in value 
which(is.na(sales$Value))
#1138 2475 2476 2477 3675 these row contain NA in value data  
## all the BM
#business moment 
describe.by(sales)
## here we get the mean median sd range skew kurtosisi 
summary(sales)
## so there is to  much diffrence in mean and median so there is a possibility of outlier 
### check for the outlier 
boxplot(sales$Quantity, main="Quantity with outlier")
outlier_values<- as.data.frame(boxplot.stats(sales$Quantity)$out)  # outlier values.
hist(sales$Quantity)

## pattern of missing value 
md.pattern(sales)
windows()
##visualization odf missing value 
misisng_pttern <- aggr(sales, col=c('navyblue','yellow'),
                       numbers=TRUE, sortVars=TRUE,
                       labels=names(sales), cex.axis=.7,
                       gap=3, ylab=c("Missing data","Pattern"))


##impute missing value 
#impute_value <- mice(sales,m=5,method="pmm",maxit = 5)
##this is a unbalanced data
library(Hmisc)
sales$Value<- impute(sales$Value,fun = mean)
sum(is.imputed(sales$Value))
colSums(is.na(sales))
describe(sales$Value)
summary(sales$Value)
colSums(is.na(sales))
## so there is no na value 


library(tidyr)
library(knitr)
library(rmarkdown)
library(dplyr)

### RFM






###Recency, Frequency, & Monetary (RFM) 
##We first need to calculate the max transaction date to calculate the recency score
max_Date <- max(sales$Date)

## now i am going to calculate the Recency
Recency <- sales %>% 
  group_by(dealer) %>% 
  summarise(recency=as.numeric(as.Date("2018-03-30")-max(Date)))

View(Recency)
summary(Recency)


library(plyr)
frequency1 <- ddply(sales,.(dealer),summarize,sum_value=sum(Value),freq=length(dealer))

frequency <- ddply(sales,.(dealer,Date),summarize,sum_freq=sum(Value),frequency=length(dealer))

View(frequency)
str(frequency)
Frequency <- ddply(frequency,.(dealer),summarize,frequency=length(dealer))
View(Frequency)


###Monetary #######
#Sum the amount of money a customer spent and divide it by Frequency,
#to get the amount per transaction on average, that is the Monetary data.
sum_value <- ddply(sales,.(dealer),summarize,sum_value=sum(Value))
View(sum_value)
## frequency with sum value 
#sales_F_M<- merge(Frequency, sum_value, by="dealer")
#View(sales_F_M)
sales_F_M<- merge(frequency1, sum_value, by="dealer")
View(sales_F_M)
library(data.table)

setnames(sales_F_M,c("freq"),c("frequency"))
setnames(sales_F_M,c("sum_value.y"),c("sum_value"))


monetary <- sales_F_M$sum_value/sales_F_M$frequency




sales_F_M$monetary <-sales_F_M$sum_value / sales_F_M$frequency

View(sales_F_M)


#We have calculated monetary.
#we remove the sum_value column
sales_F_M <- sales_F_M[c(-2,-4)]
View(sales_F_M)
# We interested in Recency, Frequency and Monetary###
rfm_result <- cbind(Recency,sales_F_M)
View(rfm_result)
rfm_result <- rfm_result[,-c(3)]
View(rfm_result)
#setwd("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets")
#getwd()
#write.csv(rfm_result,file = "rfm.csv")
##graphical presentation
hist(rfm_result$recency)
hist(rfm_result$frequency)
hist(rfm_result$monetary)



#########clv

sales_data <- read.csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//rfm.csv")
normal=as.data.frame(scale(sales_data[,3:5]))
###We have used this formula of 0.105NR+0.258NM+0.637NF
normal$clv <- 0.105*normal$recency+0.258*normal$frequency+0.637*normal$monetary
normal=cbind(sales_data[,-c(1,2)],normal)
clv_value=normal[c(7)]


#################  go for Credit Score 
## Cliustering 

### Recency  70  && frequency 5 monetary 100000 cluster 1
### Recency  140  && frequency 12 monetary 300000 cluster 2
### Recency  220  && frequency 20 monetary 80000 cluster 3
### Recency  300  && frequency 35 monetary 140000 cluster 4
## then cluster 5


rfm_result$creditscorevalue <- ifelse(rfm_result$recency < 70 && rfm_result$frequency >25 
&& rfm_result$monetary >1200000, "Best",ifelse(70 < rfm_result$recency & rfm_result$recency >140 && 
25 > rfm_result$frequency &  rfm_result$frequency >15&& 1200000 > rfm_result$monetary & rfm_result$monetary < 8000000, "good",
ifelse(140 < rfm_result$recency & rfm_result$recency >210 && 
         15 > rfm_result$frequency &  rfm_result$frequency >10 && 800000 > 
         rfm_result$monetary & rfm_result$monetary < 500000, "Average",ifelse(210 < rfm_result$recency & rfm_result$recency >270 && 
        10 > rfm_result$frequency &  rfm_result$frequency >5 && 500000 > 
        rfm_result$monetary & rfm_result$monetary < 200000, "low","Verylow"))))
                                                                              
                               
                               
                               
rfm_result=rfm_result[c(-5)]                               
                               
#########not working for this data                                
                               
                               
                               
                               
normalized <- as.data.frame(scale(rfm_result[,c(-1)]))
data1 <-rfm_result[,c(-1)] 


#### so ow i will  take k =5 and build model

modelk <- kmeans(data1,5)
View(modelk)

cluster_k <- as.matrix(modelk$cluster)
View(cluster_k)


final_Result <- cbind(rfm_result,cluster_k)
View(final_Result)

aggregate(final_Result,by=list(final_Result$cluster_k),FUN = mean)

# recency frequency   monetary cluster_k
#124.22222 12.333333  676195.36         1
#36.66667 19.333333 1344310.88          2
#119.54545  7.123967  104252.29         3
#134.72745  5.239216   33959.22         4
#136.16129  8.548387  280904.74         5
                 


                                                                                
##### not good for this data 

##  we take some R F M value then on the basis of RFM value we will find the credit score 


## so first i remove that clm credit score 

rfm_result <- rfm_result[c(-8,-9)]
## go for r  f  m value 
max(rfm_result$recency) 
#i will take 90 as very good  5
## 180 as good  4
## 270 as average  3 
## 360 low  2 
#  360 + bad 1

rfm_result$R <- ifelse(rfm_result$recency <= 90,'5',
ifelse(90< rfm_result$recency &rfm_result$recency<=180,'4',
ifelse(180< rfm_result$recency & rfm_result$recency <=270,'3',ifelse(270 < rfm_result$recency & rfm_result$recency <= 360,"2","1"))))
max(rfm_result$frequency)
## #i will take 35+ as very good
## 25+ 35- as good
## 15+ 25- as average 
## 5+ 15-low 
#  below 5 bad 


rfm_result$f <- ifelse(rfm_result$frequency >=35,'5',
ifelse(35 > rfm_result$frequency &rfm_result$frequency >25,'4',
ifelse(25 > rfm_result$frequency & rfm_result$frequency >15,'3',
ifelse(15 > rfm_result$frequency & rfm_result$frequency >10,"2","1"))))

max(rfm_result$monetary)
## ## #i will take 1300000+ as very good
## 8lkh+ 1300000- as good
## 5+ 8- as average 
## 2+ 5-low 
#  below 2 bad 

rfm_result$M <- ifelse(rfm_result$monetary >=1300000,'5',
ifelse(1300000 > rfm_result$monetary &rfm_result$monetary >800000,'4',
  ifelse(800000 > rfm_result$monetary & rfm_result$monetary >500000,'3',
 ifelse(500000 > rfm_result$monetary & rfm_result$monetary >200000,"2","1"))))


str(rfm_result)
rfm_result$RFM<- with(rfm_result, paste0(rfm_result$R, rfm_result$f, rfm_result$M))


max(rfm_result$RFM)
min(rfm_result$RFM)

## if RFM score btween 450  to 551 "1" Very goood 
## if RFM score between 350 to 449 "2" good 
## if RFM score between 250 to 349 "3" Average 
## if RFM Score between 150 to 249 "4" low 
## if rfm less than 150 the "5"  Bad 


rfm_result$Creditscore <- ifelse(rfm_result$RFM>=450,'5',
     ifelse(450 > rfm_result$RFM &rfm_result$RFM >350,'4',
            ifelse(350 > rfm_result$RFM & rfm_result$RFM >250 ,'3',
         ifelse(250 > rfm_result$RFM & rfm_result$RFM >150,"2","1"))))





finaldata=cbind(rfm_result,clv_value)
View(finaldata)


#getwd()
#write.csv(finaldata,file ="finaldata.csv")
