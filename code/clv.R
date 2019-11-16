sales_data <- read.csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//rfm.csv")
return_data=read.csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//returnsrfm.csv")
## normalization
normal=as.data.frame(scale(sales_data[,3:5]))
normal=cbind(Dealer.ID=sales_data[,c(2)],normal)

### clv
###We have used this formula of 0.105NR+0.258NM+0.637NF
normal$clv <- 0.105*normal$recency+0.258*normal$frequency+0.637*normal$monetary
normal=cbind(sales_data[,-c(1,2)],normal)

########returns data
normal_return=as.data.frame(scale(return_data[,3:5]))
normal_return=cbind(Dealer.ID=return_data[,c(2)],normal_return)
### clv
###We have used this formula of 0.105NR+0.258NM+0.637NF
normal_return$clv <- 0.105*normal_return$recency+0.258*normal_return$frequency+0.637*normal_return$monetary
normal_return=cbind(return_data[,-c(1,2)],normal_return)

                            
