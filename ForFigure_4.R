Sys.setlocale("LC_TIME", "English")
setwd("D:/RESULTS")
data=read.table(file = "Russia_MetadataInTree.Month_All_Omicron_BA11_DormDerived.After1stDec.woDormSamples",header=TRUE)
#data=read.table(file = "SPB_MetadataInTree.Month_All_Omicron_BA11_DormDerived.After1stDec.woDormSamples",header=TRUE)

data$month = paste(data$month,"-01",sep = "")
colnames(data) = c("month","all","Omicron","BA11","DormDerived")

k = 20
common = theme_classic()+
  theme(axis.text.x = element_text(size =k, color = "black"), 
        axis.text.y = element_text(size =k, color = "black"),
        axis.title.x = element_text(size =k, color = "black"), 
        axis.title.y = element_text(size =k, color = "black"),
        legend.text = element_text(size =k, color = "black"),
        legend.title = element_blank())

#colors <- c("all" = "lightgrey", "Omicron" = "darkgrey", "BA11" = "#FC8D59","DormDerived" = "#B30000")
colors <- c("clade A" = "#B30000","other BA.1.1" = "#FC8D59","other Omicron" = "darkgrey","non-Omicron" = "lightgrey")

ggp = ggplot(data = data) +
        #geom_point(aes(as.Date(month),all, color = "all")) +
        #  geom_point(aes(as.Date(month),Omicron,color = "Omicron")) + geom_point(aes(as.Date(month),BA11,color = "BA11")) #+
         geom_line(aes(as.Date(month),all,color = "non-Omicron")) +
         geom_line(aes(as.Date(month),Omicron,color = "other Omicron")) + geom_line(aes(as.Date(month),BA11,color = "other BA11")) +
         geom_line(aes(as.Date(month),DormDerived,color = "DormDerived")) +
         common + xlab("month") + ylab("samples") +
         geom_area(aes(x = as.Date(month), y = all, fill = "non-Omicron"), alpha=0.6) +
         geom_area(aes(x = as.Date(month), y = Omicron, fill = "other Omicron"), alpha=0.6) +
         geom_area(aes(x = as.Date(month), y = BA11, fill = "other BA.1.1"), alpha=0.6) +
         geom_area(aes(x = as.Date(month), y = DormDerived, fill = "clade A"), alpha=0.6) +
        scale_fill_manual(values = colors)   +
        scale_color_manual(values = colors)   +
        guides (color = FALSE) +
scale_x_date(
  date_labels = "%Y-%b") +
  theme(axis.text = element_text(angle = 90))
        

#ggsave("D:/PAPER/FromAlignmentInTree.All_Omicron_BA11_DD.InSPB.FINAL.png",plot= ggp, width = 20, height = 12, dpi = 300, units = "cm")

ggsave("D:/PAPER/FromAlignmentInTree.All_Omicron_BA11_DD.InRussia.FINAL.png",plot= ggp, width = 20, height = 12, dpi = 300, units = "cm")
