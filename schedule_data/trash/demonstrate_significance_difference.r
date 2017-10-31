setwd("~/Dropbox/thesis/data/results/schedule_data")

rm(list=ls(all=TRUE))

options(scipen=999)
options(digits=8)
options(stringsAsFactors = FALSE)
library(sandwich)
library(lmtest)

Data = read.csv(file="16-10-2017_schedule_2-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Useful variables
Data$ccr.size = Data$ccr*Data$size
Model_UpOCT_Size = lm(Data$up.oct_schedule ~ Data$size + Data$levels) 
Model_UpOCT_Edge = lm(Data$up.oct_schedule ~ Data$edges + Data$levels)

actual_size = Data$up.oct_schedule[order(Data$up.oct_schedule)]
fitted_size = fitted(Model_UpOCT_Stand)[order(Data$up.oct_schedule)]
regression_test <- data.frame(Actual = actual,Predicted = fitted)

tikz(file = "oct_size_compare", width = 5, height = 5)

plot <-ggplot(regression_test, aes(Actual,Predicted)) + geom_point(shape=1,colour='red') + geom_smooth(slope = 1,colour = 'gray',se=FALSE) +  theme_bw()
# + geom_point(shape=1) + scale_colour_manual(values=c("Insertion"="blue4"))  

print(plot)

plot(Data$up.oct_schedule[order(Data$up.oct_schedule)], fitted(Model_UpOCT_Stand)[order(Data$up.oct_schedule)],pch=16,cex=0.5,col="orange",
     main="",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
lines(c(0,5000),c(0,5000),col="grey50")

dev.copy2eps(file="up_oct_nodes.eps")
dev.off()
