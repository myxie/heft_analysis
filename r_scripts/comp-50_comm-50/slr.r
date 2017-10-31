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

Data2 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_2-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)
Data3 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_3-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data4 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_4-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data5 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_5-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data5 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_5-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
# Data5 = read.csv(file="20-10-2017_schedule_8-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data8 = read.csv(file="comp-50_comm-50/20-10-2017_schedule_8-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#EE7722","#992288","#11AA99","#777777")

cbbPalette <- 
  c(
    rgb(50,140,0, maxColorValue = 255),
    rgb(255,180,0, maxColorValue = 255),
    rgb(0,80,160, maxColorValue = 255),
    rgb(86,200,255, maxColorValue = 255)
  )


xvar = Data2$size

up.insertion <- data.frame(Nodes = xvar,Makespan = Data2$up.insertion/Data2$cp,Algorithm="A 1")
oct.oct_sched <- data.frame(Nodes = xvar, Makespan = Data2$oct.oct_schedule/Data2$cp,Algorithm="B 2")
rand.insertion <- data.frame(Nodes = xvar, Makespan = Data2$random.insertion/Data2$cp,Algorithm="C 1")
oct.insertion <-data.frame(Nodes = xvar, Makespan = Data2$oct.insertion/Data2$cp,Algorithm="B 1")
rand.greedy <- data.frame(Nodes = xvar, Makespan = Data2$random.greedy/Data2$cp, Algorithm="C 3")
# ,p4=rand.greedy
zz <- melt(list(p1 = up.insertion, p2 = oct.oct_sched), id.vars = c("Nodes","Makespan","Algorithm"))

plot<-ggplot(zz, aes(Nodes, Makespan, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(1,2,2,2))+theme()
scaleplot
tikz(file = "/home/artichoke/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/50-comp/2-processor-makespan-comparison.tex", width = 4, height = 4)
scaleplot+theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_shape_manual(values=c(4, 0, 1,3)) +  xlim(0, 1000)+ylim(0,50) + theme(legend.title=element_blank(),legend.position = "bottom",legend.key = element_blank())
dev.off()

plot<-ggplot(zz, aes(Nodes, Makespan, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(1,2,2,2))+theme()
scaleplot
# tikz(file = "/home/artichoke/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/50-comp/2-processor-makespan-comparison.tex", width = 4, height = 4)
scaleplot+theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_shape_manual(values=c(20, 1, 1,3)) +  xlim(0, 1000)+ylim(0,50) +theme(legend.title=element_blank(),legend.position = c(0.25,0.8),legend.key = element_blank())
dev.off()



########################################################################################################################

xvar3 = Data3$size

up.insertion <- data.frame(Nodes = xvar3,SLR = Data3$up.insertion/Data3$cp,Algorithm="A 1")
oct.oct_sched <- data.frame(Nodes = xvar3, SLR = Data3$oct.oct_schedule/Data3$cp,Algorithm="B 2")
rand.insertion <- data.frame(Nodes = xvar3, SLR = Data3$random.insertion/Data3$cp,Algorithm="C 1")
oct.insertion <-data.frame(Nodes = xvar3, SLR = Data3$oct.insertion/Data3$cp,Algorithm="B 1")
rand.greedy <- data.frame(Nodes = xvar3, SLR = Data3$random.greedy/Data3$cp, Algorithm="C 3")

zz <- melt(list(p1 = up.insertion, p2 = oct.oct_sched, p3 = rand.insertion), id.vars = c("Nodes","SLR","Algorithm"))

plot<-ggplot(zz, aes(Nodes, SLR, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(2,3,3,3))+theme()
tikz(file = "~/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/50-comp/3-processor-slr-comparison.tex", width = 5, height = 3)
scaleplot + theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA,colour="black")) + scale_shape_manual(values=c(4, 20, 1,20)) +  xlim(0, 5000)+ylim(0,50) +theme(legend.title=element_blank(),legend.position ="bottom",legend.key = element_blank())
dev.off()

# +theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_shape_manual(values=c(4, 0, 1,3)) + theme(legend.title=element_blank(),legend.position = c(0.25,0.8),legend.key = element_blank())
########################################################################################################################
xvar8 = Data8$size

up.insertion <- data.frame(Nodes = xvar8,SLR = Data8$up.insertion/Data8$cp,Algorithm="A 1")
oct.oct_sched <- data.frame(Nodes = xvar8, SLR = Data8$oct.oct_schedule/Data8$cp,Algorithm="B 2")
rand.insertion <- data.frame(Nodes = xvar8, SLR = Data8$random.insertion/Data8$cp,Algorithm="C 1")
oct.insertion <-data.frame(Nodes = xvar8, SLR = Data8$oct.insertion/Data8$cp,Algorithm="B 1")
rand.greedy <- data.frame(Nodes = xvar8, SLR = Data8$random.greedy, Algorithm="C 3")

zz <- melt(list(p1 = up.insertion, p2 = oct.oct_sched, p3 = rand.insertion), id.vars = c("Nodes","SLR","Algorithm"))

plot<-ggplot(zz, aes(Nodes, SLR, color=Algorithm,size=Algorithm)) + geom_point(aes(shape=Algorithm,color=Algorithm))
scaleplot<-plot+ scale_color_manual(values=cbbPalette) +  scale_size_manual(values=c(2,3,3,3))+theme()
tikz(file = "~/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/50-comp/8-processor-slr-comparison.tex", width = 5, height = 3)
scaleplot + theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA,colour="black")) + scale_shape_manual(values=c(4, 20, 1,20)) +  xlim(0, 5000)+ylim(0,50) +theme(legend.title=element_blank(),legend.position ="bottom",legend.key = element_blank())
dev.off()

########################################################################################################################
