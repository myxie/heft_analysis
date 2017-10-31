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

Data2 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_2-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)
Data3 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_3-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data4 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_4-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data5 = read.csv(file="comp-50_comm-50/16-10-2017_schedule_5-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  
Data8 = read.csv(file="comp-50_comm-50/20-10-2017_schedule_8-processors_comp50-comm50.csv",header=TRUE, stringsAsFactors=F)  

cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Data2$seq/Data2
val = Data2$seq/Data2$up.insertion
newval = val[val > 1]
tmp = Data2[(Data2$seq/Data2$up.insertion),]


seqGreater2 = Data2[Data2$seq>Data2$up.insertion,]
median(seqGreater2$seq/seqGreater2$up.insertion)


# Speedup = makespan/seq
SpeedUpIns = c(mean(Data2$seq/Data2$up.insertion),mean(Data3$seq/Data3$up.insertion),
               mean(Data4$seq/Data4$up.insertion),mean(Data5$seq/Data5$up.insertion),
               mean(Data8$seq/Data8$up.insertion))

efficiencySpeedUpIns= SpeedUpIns/(c(2,3,4,5,8))

SpeedUpOct = c(mean(Data2$seq/Data2$up.oct_schedule),mean(Data3$seq/Data3$up.oct_schedule),
               mean(Data4$seq/Data4$up.oct_schedule),mean(Data5$seq/Data5$up.oct_schedule),
               mean(Data8$seq/Data8$up.oct_schedule))

efficiencySpeedUpOct= SpeedUpOct/(c(2,3,4,5,8))

SpeedOctIns = c(mean(Data2$seq/Data2$oct.insertion),mean(Data3$seq/Data3$oct.insertion),
                mean(Data4$seq/Data4$oct.insertion),mean(Data5$seq/Data5$oct.insertion),
                mean(Data8$seq/Data8$oct.insertion))

efficiencySpeedOctIns= SpeedOctIns/(c(2,3,4,5,8))

SpeedOctOct = c(mean(Data2$seq/Data2$oct.oct_schedule),mean(Data3$seq/Data3$oct.oct_schedule),
                mean(Data4$seq/Data4$oct.oct_schedule),mean(Data5$seq/Data5$oct.oct_schedule),
                mean(Data8$seq/Data8$oct.oct_schedule))

efficiencySpeedOctOct= SpeedOctOct/(c(2,3,4,5,8))

SpeedRandIns = c(mean(Data2$seq/Data2$random.insertion),mean(Data3$seq/Data3$random.insertion),
                 mean(Data4$seq/Data4$random.insertion),mean(Data5$seq/Data5$random.insertion),
                 mean(Data8$seq/Data8$random.insertion))

efficiencySpeedRandIns= SpeedRandIns/(c(2,3,4,5,8))

SpeedUpGreedy = c(mean(Data2$seq/Data2$up.greedy),mean(Data3$seq/Data3$up.greedy),
                  mean(Data4$seq/Data4$up.greedy),mean(Data5$seq/Data5$up.greedy),
                  mean(Data8$seq/Data8$up.greedy))

efficiencySpeedUpGreedy= SpeedUpGreedy/(c(2,3,4,5,8))

all_speedup = c(SpeedUpIns,SpeedUpOct,SpeedOctIns,SpeedOctOct,SpeedRandIns,SpeedUpGreedy)

d <- data.frame(Algorithms=rep(c("A 1","A 2","B 1","B 2","C 1","A 3"), each=5),
                Processors=c(2,3,4,5,6),
                Speedup = all_speedup)



tikz(file = "~/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/speedup_efficiency/speedup.tex", width = 5, height = 3)
tmplot <- ggplot(data=d, aes(x=Processors, y=Speedup, fill=Algorithms)) +
  geom_bar(stat="identity", position=position_dodge()) +  scale_fill_manual(values=cbbPalette) + theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA,colour="black")) + scale_x_discrete(limits=c("1","2","3","4","5","8"))
print(tmplot)
dev.off()

all_efficiency = c(efficiencySpeedUpIns,efficiencySpeedUpOct,efficiencySpeedOctIns,efficiencySpeedOctOct,efficiencySpeedRandIns,efficiencySpeedUpGreedy)

d <- data.frame(Algorithms=rep(c("A 1","A 2","B 1","B 2","C 1","A 3"), each=5),
                Processors=c(2,3,4,5,6),
                Efficiency = all_efficiency)


tikz(file = "~/Dropbox/Apps/ShareLaTeX/thesis/chapter_7/speedup_efficiency/efficiency.tex", width = 5, height = 3)
tmplot <- ggplot(data=d, aes(x=Processors, y=Efficiency, fill=Algorithms)) +
  geom_bar(stat="identity", position=position_dodge()) +  scale_fill_manual(values=cbbPalette) + theme(panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_rect(fill=NA,colour="black")) + scale_x_discrete(limits=c("1","2","3","4","5","8"))
print(tmplot)
dev.off()
