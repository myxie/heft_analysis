setwd("~/Dropbox/thesis/data/results/schedule_data")

rm(list=ls(all=TRUE))

options(scipen=999)
options(digits=8)
options(stringsAsFactors = FALSE)
library(sandwich)
library(lmtest)
library(ggplot2)

Data = read.csv(file="comp-500_comm-50/22-10-2017_schedule_4-processors_comp500-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data = read.csv(file="comp-500_comm-50/20-10-2017_schedule_2-processors_comp500-comm50.csv",header=TRUE, stringsAsFactors=F)
# Useful variables
Data$edges.parallel = Data$edges*Data$parallel
Data$levels.parallel = Data$levels*Data$parallel

Data$edges.fork = Data$edges*Data$fork_join
Data$levels.fork = Data$levels*Data$fork_join
plot(Data$size, Data$oct.oct_schedule)
lines(Data$size, Data$oct.oct_schedule,type="p",col="blue")
mean(Data$seq/Data$up.insertion)/2
mean(Data$seq/Data$oct.oct_schedule)/2


################################################################################################################

#Upward + Insertion
Model_UpIns_Stand = lm(Data$up.insertion ~ Data$levels + Data$size) #
lines(Data$size,Data$up.insertion, type="p")
summary(Model_UpIns_Stand)
lines(Data$size, Data$up.oct_schedule,type='p')
coeftest(Model_UpIns_Stand,vcov.=(vcovHC(Model_UpIns_Stand, "HC3")))

################################################################################################################

#Upward + OCT Schedule
#use this as an example by mapping the fitted and predicted times on the graph, and show how one fits better than another (e.g. size has better fit than edges)
Model_UpOCT_Stand = lm(Data$up.oct_schedule ~ Data$size) # SIGN size is a lot more significant
summary(Model_UpOCT_Stand)
plot(Data$size, Data$up.oct_schedule)
coeftest(Model_UpOCT_Stand,vcov.=(vcovHC(Model_UpOCT_Stand,"HC3")))
################################################################################################################

#Upward + Greedy
Model_UpGreed_Stand = lm(Data$up.greedy ~ Data$levels + Data$edges)
plot(Data$size, Data$up.greedy)
summary(Model_UpGreed_Stand)

################################################################################################################


#OCT + Insertion
Model_OCTIns_Stand = lm(Data$oct.insertion ~  Data$size + Data$levels) 
summary(Model_OCTIns_Stand)

actual_size = Data$oct.insertion[order(Data$oct.insertion)]
fitted_size = fitted(Model_OCTIns_Stand)[order(Data$oct.insertion)]
regression_test <- data.frame(Actual = actual_size,Predicted = fitted_size)
plot <-ggplot(regression_test, aes(Actual,Predicted)) + geom_point(shape=1,colour='red') + geom_abline(slope = 1,colour = 'gray',se=FALSE) +  theme_bw()
# + geom_point(shape=1) + scale_colour_manual(values=c("Insertion"="blue4"))  

print(plot)


################################################################################################################


#OCT + OCT Schedule
Model_OCTOCT_Stand = lm(Data$oct.oct_schedule ~ Data$size) 
summary(Model_OCTOCT_Stand)

################################################################################################################

#OCT + Greedy
Model_OCTGreed_Stand = lm(Data$oct.greedy ~ Data$levels.parallel + Data$size)
summary(Model_OCTGreed_Stand)

##############################################################################################################################

#Rand + Insertion
Model_RandInsertion = lm(Data$random.insertion ~ Data$levels+ Data$size)
summary(Model_RandInsertion)

##############################################################################################################################

#Rand + Greedy
Model_RandGreedy = lm(Data$random.greedy ~ Data$edges + Data$levels) 
summary(Model_RandGreedy)

##############################################################################################################################

#Rand + OCT Schedule
Model_RandOCTSchedule = lm(Data$oct.oct_schedule ~ Data$size) 
summary(Model_RandOCTSchedule)

##############################################################################################################################
