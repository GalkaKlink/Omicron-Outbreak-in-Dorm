library(tidyverse)
library("Hmisc")

data = read.table(file = "D:/RESULTS/Russia_MetadataInTree.Regions.BA11_DormDerived.tsv",header=TRUE)  #file with GISAID metadata of samples 
data$Other= data$BA11-data$DormDerived
colnames(data) = c("region","All_BA11","DormDerived","Other")
data = data[,c(1,4,3)]
colnames(data) = c("region","Other","DormDerived")
#data$DDpercent = (data$DormDerived/(data$Other +data$DormDerived))*100
data = data[order(-data$DormDerived/(data$Other +data$DormDerived)),]

names = read.csv(file = "D:/RESULTS/RegionsNames_Full_Short.tsv",sep="\t",header = TRUE)  #table for conversion of abbreviations of Russian regions from GISAID metadata into unified names 
data = merge(x=data,y=names,by.x = "region",by.y="short",all.x=TRUE,all.y=FALSE)

w_int = function(x) {
 n = data[x,2] + data[x,3]
 k = data[x,3]
 df = binconf(k, n, alpha=0.05,method="wilson")
 vect = as.vector(df[1,])
 return(vect)
 }
results = sapply(c(1:nrow(data)),w_int)
results = as.data.frame(t(results))
colnames(results) = c("DDpercent","LowerBound","UpperBound")
results$DDpercent = round(results$DDpercent*100,digits=1)
results$LowerBound = round(results$LowerBound*100,digits=1)
results$UpperBound = round(results$UpperBound*100,digits=1)
data=cbind(data,results)

data$lower = (data$Other+data$DormDerived)*(0.01*data$LowerBound)
data$upper = (data$Other+data$DormDerived)*(0.01*data$UpperBound)
data$lower = data$DormDerived-data$lower
data$upper = data$upper-data$DormDerived
data$lower = data$Other+data$lower
data$upper = data$Other-data$upper
data$all = data$DormDerived + data$Other

colnames(data)[1] = "short_reg_name"
colnames(data)[4] = "region"

levs = data$region
data1 <- data %>% gather(key = "group", value="samples", 2:3) 


common = theme_classic()+
  theme(axis.text.x = element_text(size =12,color = "black"), 
        axis.text.y = element_text(size =12,color = "black"),
        axis.title.x = element_text(size =12,color = "black"), 
        axis.title.y = element_text(size =12,color = "black"),
        legend.text = element_text(size =12,color = "black"),
        legend.title = element_blank())


data1$lower = ifelse(data1$group == "Other",data1$lower, "NA")
data1$upper = ifelse(data1$group == "Other",data1$upper, "NA")
data1$lower = as.numeric(data1$lower)
data1$upper = as.numeric(data1$upper)
group_levels = c("DormDerived","Other")

ggp = ggplot(data1, aes(fill=factor(group,levels = group_levels), y=samples, x=factor(region, levels = levs))) + 
       geom_bar(position="stack", stat="identity", alpha = 0.6) +
       scale_fill_manual(values = c("#B30000","#FC8D59"), labels = c("clade A","Other")) +
       common + ylim(0,360) +
       geom_text(data = data1 %>%
               filter(group == "Other"), 
               aes(label = paste(round(DDpercent, 1), "% ",sep=""), hjust = ifelse(short_reg_name=="SPE",-2,-1)),
              #aes(label = paste(round(DDpercent, 1), " (",LowerBound, ";",UpperBound,")","%",sep="")),
               vjust = 0.5, angle = 90) +
              geom_errorbar(aes(ymin = upper, ymax = lower), width = .2, col = "red") +
              theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
              xlab("region") 
            
ggsave("D:/PAPER/Figure5.FINAL.FromAlignmentInTree.DormDerivedSeqsInRussianRegions.png",plot= ggp,
       width = 20, height = 12, dpi = 300, units = "cm")


write.table(data,file = "D:/PAPER/FromAlignmentInTree.DormDerivedSeqsInRussianRegions.tsv",sep="\t",row.names=FALSE,quote=FALSE)
