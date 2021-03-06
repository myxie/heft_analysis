setwd("~/Dropbox/thesis/data/results/schedule_data")

rm(list=ls(all=TRUE))

options(scipen=999)
options(digits=8)
options(stringsAsFactors = FALSE)
library(sandwich)
library(lmtest)

Data = read.csv(file="20-10-2017_schedule_8-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Useful variables
Data$edges.parallel = Data$edges*Data$parallel
Data$levels.parallel = Data$levels*Data$parallel

Data$edges.fork = Data$edges*Data$fork_join
Data$levels.fork = Data$levels*Data$fork_join

################################################################################################################

#Upward + Insertion
Model_UpIns_Stand = lm(Data$up.insertion ~ Data$levels + Data$size
                       ) #

summary(Model_UpIns_Stand)

coeftest(Model_UpIns_Stand,vcov.=(vcovHC(Model_UpIns_Stand, "HC3")))

################################################################################################################

#Upward + OCT Schedule
#use this as an example by mapping the fitted and predicted times on the graph, and show how one fits better than another (e.g. size has better fit than edges)
Model_UpOCT_Stand = lm(Data$up.oct_schedule ~ Data$size + Data$levels) # SIGN size is a lot more significant
summary(Model_UpOCT_Stand)

################################################################################################################

#Upward + Greedy
Model_UpGreed_Stand = lm(Data$up.greedy ~ Data$edges + Data$levels)
summary(Model_UpGreed_Stand)

################################################################################################################


#OCT + Insertion
Model_OCTIns_Stand = lm(Data$oct.insertion ~  Data$edges + Data$levels) 

summary(Model_OCTIns_Stand)

################################################################################################################


#OCT + OCT Schedule
Model_OCTOCT_Stand = lm(Data$oct.oct_schedule ~ Data$size+Data$levels) 

summary(Model_OCTOCT_Stand)

################################################################################################################

#OCT + Greedy
Model_OCTGreed_Stand = lm(Data$oct.greedy ~ Data$levels + Data$edges)
summary(Model_OCTGreed_Stand)

##############################################################################################################################

#Rand + Insertion
Model_RandInsertion = lm(Data$random.insertion ~ Data$levels+ Data$size)
summary(Model_RandInsertion)
coeftest(Model_RandInsertion,vcov.=(vcovHC(Model_RandInsertion, "HC3")))
##############################################################################################################################

#Rand + Greedy
Model_RandGreedy = lm(Data$random.greedy ~ Data$edges + Data$levels) 
summary(Model_RandGreedy)

##############################################################################################################################

#Rand + OCT Schedule
Model_RandOCTSchedule = lm(Data$random.oct_schedule ~ Data$levels + Data$edges) 
summary(Model_RandOCTSchedule)

##############################################################################################################################
