setwd("~/Dropbox/thesis/data/results/schedule_data")

rm(list=ls(all=TRUE))

options(scipen=999)
options(digits=8)
options(stringsAsFactors = FALSE)
library(sandwich)
library(lmtest)
library(compare)
library(ggplot2)

Data2 = read.csv(file="16-10-2017_schedule_2-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)
Data3 = read.csv(file="16-10-2017_schedule_3-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data4 = read.csv(file="16-10-2017_schedule_4-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data5 = read.csv(file="16-10-2017_schedule_5-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  

small = Data2[Data2$size < 5,]
medium = Data2[which(Data2$size >= 5 & Data2$size < 100),]
large =  Data2[Data2$size >= 100,]

branch = Data2[Data2$edges/Data2$size >= 1,]
flat = Data2[Data2$levels <= 2,]

parallel = Data2[Data2$parallel>0.5,]
fork = Data2[Data2$fork_join>0.5,]
