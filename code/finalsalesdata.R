library(Amelia)
library(psych)
library(dplyr)
sales <- read.csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//Sales.csv")
View(sales)
##
sales <- sales[,-c(5,6,7)]
#print(sales$Quantity==sales$Value)

sort(table(sales$Area))
sample_data=subset(sales,sales$Area=="Samples")

### so we will remove these row from orig data set 
#as.numeric(rownames(sample_data))
sales1<- anti_join(sales,sample_data,by="Area")

same=subset(sales1,sales1$Quantity==sales1$Value)
as.numeric(rownames(same))
sales1 <- sales1[-c(449,1035,1242, 1484, 1485, 1497, 1498, 3389, 5026, 5027, 5028, 5029, 5030, 5096, 5097, 5098, 5099, 5100, 6601,
           6896, 6897, 8433, 8438, 8554, 8638, 8639, 8654),]

greater=subset(sales1,sales1$Quantity>sales1$Value)  
as.numeric(rownames(greater))

sales1 <- sales1[-c(128, 129,130,131,  261,  262,  384,  385,  402,  403, 1033, 1034, 1061, 1066, 1244, 1500 ,1501, 1502, 2135,
                    2136, 2798, 4215, 4517, 4961, 4962, 4963, 4964, 5521, 5524, 5525, 5526, 5842, 5843, 5844, 5933, 6285, 6286, 6287,
                    6589 ,6652, 6653, 6654, 6960, 7854, 7855, 8088, 8248, 8249, 8250, 8251, 8520, 8643, 8644, 8665, 8666, 8667),]

getwd()
setwd("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets")
getwd()
write.csv(greater,file="greaterthanvalue.csv")
write.csv(same,file="samevalue.csv")
write.csv(sample_data,file = "samplearea.csv")
write.csv(sales1,file = "finalsales.csv")
View(sales1)
str(sales1)
anyNA(sales1)
sum(is.na(sales1$Value))
missmap(sales1)
anyNA(sales1)
