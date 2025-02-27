# 导入数据并添加分组信息
setwd("D:/R代码")
mRNA<-read.csv("Fun.csv",header=T,row.names=1)
library(RColorBrewer)
library(ggpubr)
library(ggplot2)
# 因子水平
mRNA$sam<-factor(mRNA$sam,levels=c("D100","D300","D500","D1000","D2000"))
# 颜色、分组比较设置
color <-c("#5CB85C","#337AB7","#F0AD4E","#D9534F","#5CB85C")
my_comparisons <- list(c("D300", "D1000"),
                       c("D300", "D2000"),
                       c("D500", "D1000"),
                       c("D500", "D2000"),
                       c("D1000", "D2000"))
# 提取需要循环绘制的基因名
gc <- colnames(mRNA)
#开始批量绘制
plist<-list()
for (i in 1:length(gc)){
  bar_tmp<-mRNA[,c(gc[i],"sam")]
  colnames(bar_tmp)<-c("Expression","sam")
  pb1<-ggboxplot(bar_tmp,
                 x="sam",
                 y="Expression",
                 color="sam",
                 fill=NULL,
                 add = "jitter",
                 bxp.errorbar.width = 0.6,
                 width = 0.4,
                 size=0.01,
                 font.label = list(size=30), 
                 palette = color)+
    theme(panel.background =element_blank())
  pb1<-pb1+theme(axis.line=element_line(colour="black"))+theme(axis.title.x = element_blank())
  pb1<-pb1+theme(axis.title.y = element_blank())+theme(axis.text.x = element_text(size = 15,angle = 45,vjust = 1,hjust = 1))
  pb1<-pb1+theme(axis.text.y = element_text(size = 15))+ggtitle(gc[i])+theme(plot.title = element_text(hjust = 0.5,size=15,face="bold"))
  pb1<-pb1+theme(legend.position = "NA")
  pb1<-pb1+stat_compare_means(method="t.test",hide.ns = F,comparisons =my_comparisons,label="p.signif")
  plist[[i]]<-pb1
} 
library(cowplot)
pall<-plot_grid(plist[[1]],plist[[2]],plist[[3]],
                plist[[4]],plist[[5]],plist[[6]],
                plist[[7]],plist[[8]],plist[[9]],
                plist[[10]],plist[[11]],plist[[12]],ncol=4)
pall
#输出：10*10
