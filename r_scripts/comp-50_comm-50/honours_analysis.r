setwd("~/Dropbox/thesis/data/results/schedule_data")

rm(list=ls(all=TRUE))

options(scipen=999)
options(digits=8)
options(stringsAsFactors = FALSE)
library(sandwich)
library(lmtest)

Data = read.csv(file="18-09-2017_schedule_3-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Useful variables
Data$edges = Data$BF*Data$size
Data$ccr.size = Data$ccr*Data$size
# Generate basic plot that maps makespan vs size for all heuristics

plot(Data$size,Data$oct.oct_schedule,main="Comparing Schedule length between HEFT heuristics",xlab="Nodes",ylab="Schedule Length",col='red',
     xlim=c(0,4500),ylim=c(0,100000))
lines(Data$size,Data$up.insertion,type="p",col='blue')
lines(Data$size,Data$up.oct_schedule,type="p",col='cyan')
lines(Data$size,Data$oct.greedy,type="p")
lines(Data$size,Data$up.greedy,type="p",col='orange')
lines(Data$size,Data$oct.insertion,type="p")



#Generate regressions for each separate heuristic pair

#Upward + Insertion
Model_UpIns_Stand = lm(Data$up.insertion ~ Data$levels + Data$edges) #SIGN
summary(Model_UpIns_Stand)

plot(Data$up.insertion[order(Data$up.insertion)], fitted(Model_UpIns_Stand)[order(Data$up.insertion)],pch=16,cex=0.5,col="orange",
main="Fitted and Predicted Scheduling time\nfor Up-Insertion",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
lines(c(0,5000),c(0,5000),col="grey50")


Model_UpIns_ccr = lm(Data$up.insertion ~ Data$levels + Data$ccr)

#Upward + OCT Schedule
Model_UpOCT_Stand = lm(Data$up.oct_schedule ~ Data$levels + Data$edges) # SIGN Edges are a lot more significant
summary(Model_UpOCT_Stand)

plot(Data$up.oct_schedule[order(Data$up.oct_schedule)], fitted(Model_UpOCT_Stand)[order(Data$up.oct_schedule)],pch=16,cex=0.5,col="orange",
     main="Fitted and Predicted Scheduling time\nfor Up-OCT",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
lines(c(0,5000),c(0,5000),col="grey50")

#Upward + Greedy
Model_UpGreed_Stand = lm(Data$up.greedy ~ Data$levels + Data$edges)
summary(Model_UpGreed_Stand)

plot(Data$up.greedy[order(Data$up.greedy)], fitted(Model_UpGreed_Stand)[order(Data$up.greedy)],pch=16,cex=0.5,col="orange",
     main="Fitted and Predicted Scheduling time\nfor Up-OCT",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
lines(c(0,5000),c(0,5000),col="grey50")

# Model_UpGreed_ccr = lm(Data$up.greedy ~ Data$levels + Data$ccr.size)

#OCT + Insertion
Model_OCTIns_Stand = lm(Data$oct.insertion ~  Data$edges + Data$levels) # SIGN
summary(Model_OCTIns_Stand)


#OCT + OCT Schedule
Model_OCTOCT_Stand = lm(Data$oct.oct_schedule~ Data$edges) # SIGNIFICANT - The OCT scheduling approach is heavily based purely on the number of edges
summary(Model_OCTOCT_Stand)

plot(Data$oct.oct_schedule[order(Data$oct.oct_schedule)], fitted(Model_OCTOCT_Stand)[order(Data$oct.oct_schedule)],pch=16,cex=0.5,col="orange",
     main="Fitted and Predicted Scheduling time\nfor Up-OCT",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
lines(c(0,5000),c(0,5000),col="grey50")

#OCT + Greedy
Model_OCTGreed_Stand = lm(Data$oct.greedy ~ Data$levels + Data$edges)
summary(Model_OCTGreed_Stand)

##############################################################################################################################


#Model_InsUp_Stand = lm(Data$up.insertion ~ Data$size + Data$BF)
#Model_InsUp_Stand = lm(Data$up.insertion ~ Data$size + Data$BF + Data$BF*Data$size)

# exclude size as we used it in the regressions and determined that edges were a better predictor of final values   

# plot(Data$up.insertion[order(Data$up.insertion)], fitted(Model_InsUp_Edges.Levels)[order(Data$up.insertion)],pch=16,cex=0.5,col="orange",
# main="Fitted and Predicted Scheduling time\nfor Up-Insertion",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
# lines(c(0,5000),c(0,5000),col="grey50")
# 
# points(Data$up.insertion[order(Data$up.insertion)], fitted(Model_InsUp_stand.het)[order(Data$up.insertion)],col="red",cex=0.5)
# #Useful for heteroskedacitity: https://onlinecourses.science.psu.edu/stat501/node/431
# 
# # WLS
# wts <- 1/fitted(lm(abs(residuals(Model_InsUp_Edges.Levels)) ~ Data$edges))^2
# Model_InsUp_stand.het = lm(up.insertion ~ edges + Data$levels, data = Data,weights=wts)
# plot(Data$up.insertion[order(Data$up.insertion)], fitted(Model_InsUp_stand.het)[order(Data$up.insertion)],pch=16,cex=0.5,col="orange",
#      main="Fitted and Predicted Scheduling time\nfor Up-Insertion, using WLS",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
# lines(c(0,5000),c(0,5000),col="grey50")
# 
# Data$edges = Data$BF*Data$size
# 
# #Error in the model (Actual vs Fitted, as % of actual)
# plot(Data$up.insertion[order(Data$up.insertion)], (residuals(Model_InsUp_stand.het)/Data$up.insertion)[order(Data$up.insertion)]*100)
# 
# 
# 
# Data$mult_edges.parralel = Data$edges*Data$parallel
# Data$mult_levels.parralel = Data$levels*Data$parallel
# Model_InsUp_stand_par = lm(up.insertion ~ edges + levels + mult_levels.parralel, data = Data)
# Model_InsUp_stand_par = lm(up.insertion ~ Edges + levels + mult_levels.parralel, data = Data)
# 
# Data$mult_edges.fork = Data$edges*Data$fork_join
# Data$mult_levels.fork = Data$levels*Data$fork_join
# Model_InsUp_stand_fork_levels = lm(up.insertion ~ edges + levels + mult_levels.fork, data = Data)
# Model_InsUp_stand_fork_edges = lm(up.insertion ~ edges + levels + mult_edges.fork, data = Data)
# 
# Data$size_data = Data$size
# Data$size_ccr = Data$size*Data$ccr
# Model_InsUp_stand = lm(up.insertion ~ edges + levels + ccr*size, data=Data)
# summary(Model_InsUp_stand)

