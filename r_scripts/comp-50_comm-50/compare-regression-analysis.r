setwd("~/Dropbox/thesis/data/results/schedule_data")

rm(list=ls(all=TRUE))

options(scipen=999)
options(digits=8)
options(stringsAsFactors = FALSE)
library(tikzDevice)
library(sandwich)
library(lmtest)
library(compare)
library(ggplot2)
library(reshape2)

Data = read.csv(file="comp-50_comm-50/16-10-2017_schedule_2-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)

Model_UpOCT_Stand = lm(Data$up.oct_schedule ~ Data$size + Data$levels) # SIGN size is a lot more significant
mean(Data)
summary(Model_UpOCT_Stand)

#Plot the better regression
actual_size = Data$up.oct_schedule[order(Data$up.oct_schedule)]
fitted_size = fitted(Model_UpOCT_Stand)[order(Data$up.oct_schedule)]
regression_test <- data.frame(Actual = actual_size,Predicted = fitted_size)
tikz(file = "~/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/comparing_regressions/better_fit.tex", width = 5, height = 3)

prefcolor =           rgb(0,80,160, maxColorValue = 255)



plot <-ggplot(regression_test, aes(Actual,Predicted)) + geom_point(shape=1,colour=prefcolor) + geom_abline(slope = 1,colour = 'gray') +theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA,colour="black")) 
print(plot)
dev.off()

#Plot the worst regression
Model_UpOCT_Stand = lm(Data$up.oct_schedule ~ Data$edges + Data$levels) # SIGN size is a lot more significant

actual_size = Data$up.oct_schedule[order(Data$up.oct_schedule)]
fitted_size = fitted(Model_UpOCT_Stand)[order(Data$up.oct_schedule)]
regression_test <- data.frame(Actual = actual_size,Predicted = fitted_size)
tikz(file = "~/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/comparing_regressions/worse_fit.tex", width = 5, height = 3)

plot <-ggplot(regression_test, aes(Actual,Predicted)) + geom_point(shape=1,colour=prefcolor) + geom_abline(slope = 1,colour = 'gray')   +theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA,colour="black")) 
print(plot)
dev.off()
