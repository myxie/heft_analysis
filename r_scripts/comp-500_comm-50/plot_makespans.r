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

Data2 = read.csv(file="comp-500_comm-50/20-10-2017_schedule_2-processors_comp500-comm50.csv",header=TRUE, stringsAsFactors=F)
Data3 = read.csv(file="comp-500_comm-50/22-10-2017_schedule_3-processors_comp500-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data4 = read.csv(file="comp-500_comm-50/22-10-2017_schedule_4-processors_comp500-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Data5 = read.csv(file="16-10-2017_schedule_5-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Data5 = read.csv(file="16-10-2017_schedule_5-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# # Data5 = read.csv(file="20-10-2017_schedule_8-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Data8 = read.csv(file="20-10-2017_schedule_8-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mean(Data2$ccr)

xvar = Data2$size

up.insertion <- data.frame(Nodes = xvar,Makespan = Data2$up.insertion,Algorithm="Up.Insertion")
oct.oct_sched <- data.frame(Nodes = xvar, Makespan = Data2$oct.oct_schedule,Algorithm="OCT.OCT-Schedule")
rand.insertion <- data.frame(Nodes = xvar, Makespan = Data2$random.insertion,Algorithm="Rand.OCT-Schedule")
oct.insertion <-data.frame(Nodes = xvar, Makespan = Data2$oct.insertion,Algorithm="OCT.Insertion")
rand.greedy <- data.frame(Nodes = xvar, Makespan = Data2$random.greedy, Algorithm="Rand.Greedy")

zz <- melt(list(p1 = up.insertion, p2 = oct.oct_sched, p3 = rand.insertion,p4=rand.greedy), id.vars = c("Nodes","Makespan","Algorithm"))

plot<-ggplot(zz, aes(Nodes, Makespan, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(1,2,2,2))+theme()
scaleplot+theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_shape_manual(values=c(4, 0, 1,3)) +theme(legend.title=element_blank(),legend.position = c(0.25,0.8),legend.key = element_blank())

# +  xlim(0, 1000)+ylim(0,20000)

tikz(file = "/home/artichoke/Dropbox/thesis/data/results/r_plots/comp_500_makespan.tex", width = 4, height = 4)
dev.off()


########################################################################################################################

xvar3 = Data3$size

up.insertion <- data.frame(Nodes = xvar3,Makespan = Data3$up.insertion,Algorithm="Up.Insertion")
oct.oct_sched <- data.frame(Nodes = xvar3, Makespan = Data3$oct.oct_schedule,Algorithm="OCT.OCT-Schedule")
rand.insertion <- data.frame(Nodes = xvar3, Makespan = Data3$random.insertion,Algorithm="Rand.OCT-Schedule")
oct.insertion <-data.frame(Nodes = xvar3, Makespan = Data3$oct.insertion,Algorithm="OCT.Insertion")
rand.greedy <- data.frame(Nodes = xvar3, Makespan = Data3$random.greedy, Algorithm="Rand.Greedy")

zz <- melt(list(p1 = up.insertion, p2 = oct.oct_sched, p3 = rand.insertion,p4=rand.greedy), id.vars = c("Nodes","Makespan","Algorithm"))

plot<-ggplot(zz, aes(Nodes, Makespan, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(1,2,2,2))+theme()
tikz(file = "/home/artichoke/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/3-processor-makespan-comparison.tex", width = 4, height = 4)
scaleplot+theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_shape_manual(values=c(4, 0, 1,3)) +  theme(legend.title=element_blank(),legend.position = c(0.25,0.8),legend.key = element_blank())
dev.off()

########################################################################################################################
xvar4 = Data4$size

up.insertion <- data.frame(Nodes = xvar4,Makespan = Data4$up.insertion,Algorithm="Up.Insertion")
oct.oct_sched <- data.frame(Nodes = xvar4, Makespan = Data4$oct.oct_schedule,Algorithm="OCT.OCT-Schedule")
rand.insertion <- data.frame(Nodes = xvar4, Makespan = Data4$random.insertion,Algorithm="Rand.OCT-Schedule")
oct.insertion <-data.frame(Nodes = xvar4, Makespan = Data4$oct.insertion,Algorithm="OCT.Insertion")
rand.greedy <- data.frame(Nodes = xvar4, Makespan = Data4$random.greedy, Algorithm="Rand.Greedy")

zz <- melt(list(p1 = up.insertion, p2 = oct.oct_sched, p3 = rand.insertion,p4=rand.greedy), id.vars = c("Nodes","Makespan","Algorithm"))

plot<-ggplot(zz, aes(Nodes, Makespan, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(1,2,2,2))+theme()
tikz(file = "/home/artichoke/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/8-processor-makespan-comparison.tex", width = 4, height = 4)
scaleplot+theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_shape_manual(values=c(4, 0, 1,3))  +theme(legend.title=element_blank(),legend.position = c(0.25,0.8),legend.key = element_blank())
dev.off()

########################################################################################################################
