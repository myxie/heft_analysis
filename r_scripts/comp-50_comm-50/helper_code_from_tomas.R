#Model_InsUp_Stand = lm(Data$up.insertion ~ Data$size + Data$BF)
#Model_InsUp_Stand = lm(Data$up.insertion ~ Data$size + Data$BF + Data$BF*Data$size)

# exclude size as we used it in the regressions and determined that edges were a better predictor of final values   

# plot(Data$up.insertion[order(Data$up.insertion)], fitted(Model_InsUp_Edges.Levels)[order(Data$up.insertion)],pch=16,cex=0.5,col="orange",
# main="Fitted and Predicted Scheduling time\nfor Up-Insertion",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
# lines(c(0,5000),c(0,5000),col="grey50")
# 
points(Data$up.insertion[order(Data$up.insertion)], fitted(Model_InsUp_stand.het)[order(Data$up.insertion)],col="red",cex=0.5)
# #Useful for heteroskedacitity: https://onlinecourses.science.psu.edu/stat501/node/431
# 
# # WLS
wts <- 1/fitted(lm(abs(residuals(Model_UpIns_Stand)) ~ Data$edges))^2
Model_InsUp_stand.het = lm(up.insertion ~ edges + Data$levels, data = Data,weights=wts)
plot(Data$up.insertion[order(Data$up.insertion)], fitted(Model_InsUp_stand.het)[order(Data$up.insertion)],pch=16,cex=0.5,col="orange",
     main="Fitted and Predicted Scheduling time\nfor Up-Insertion, using WLS",xlab="Actual Sheduling Time",ylab="Predicted",xlim=c(0,4500),ylim=c(0,4500))
lines(c(0,5000),c(0,5000),col="grey50")
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
