rm(list = ls())#养成好习惯，确保有干净的R语言运行环境
setwd("D:/英文第三篇R文件")#加载工作目录，这里需要更换为你自己的工作路径
library(ggplot2)
library(palmerpenguins)
library(ggpubr)
library(vegan)

#清洗和加载数据

data1 <- read.csv("拟合11.csv", header = T)#加载数据
data2 <- read.csv("拟合22.csv", header = T)#加载数据
data3 <- read.csv("拟合33.csv", header = T)#加载数据
data4 <- read.csv("拟合55.csv", header = T)#加载数据
data1$Group <- factor(data1$Group)
data2$Group <- factor(data2$Group)
data3$Group <- factor(data3$Group)
data4$Group <- factor(data4$Group)
nature_colors <- c("#0072B2", "#D55E00") #设置颜色

#作图
p1 <- ggplot(data1 %>% na.omit(), aes(SLDM, Acid, shape = "21",fill=Group)) +
  geom_point(size =3,color = "black",stroke=0.3, alpha=0.9) +
  geom_smooth(aes(color = Group), method = lm, se = TRUE) +
  stat_cor(method = "spearman") +
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) + 
  scale_shape_manual(values = 21)

# 加上自己喜欢的主题
p1 <- p1 +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "right",
        axis.text.x.top = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black", size = 0.4),
        axis.line.x.top = element_line(color = "black", size = 0.4),
        axis.line.y.right = element_line(color = "black", size = 0.4)) +
  guides(x.sec = "axis", y.sec = "axis")
p1

#作图
p2 <- ggplot(data2 %>% na.omit(), aes(LCC, Acid, shape = "21", fill=Group)) +
  geom_point(size =3,color = "black",stroke=0.3, alpha=0.9) +
  geom_smooth(aes(color = Group), method = lm, se = TRUE) +
  stat_cor(method = "spearman") +
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) +  # 设置不同组的颜色
  scale_shape_manual(values = 21)  # 将所有组的点形状设置为实心圆

# 加上自己喜欢的主题
p2 <- p2 +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "right",
        axis.text.x.top = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black", size = 0.4),
        axis.line.x.top = element_line(color = "black", size = 0.4),
        axis.line.y.right = element_line(color = "black", size = 0.4)) +
  guides(x.sec = "axis", y.sec = "axis")
p2

#作图
p3 <- ggplot(data3 %>% na.omit(), aes(SLA, Acid, shape = "21", fill=Group)) +
  geom_point(size =3,color = "black",stroke=0.3, alpha=0.9) +
  geom_smooth(aes(color = Group), method = lm, se = TRUE) +
  stat_cor(method = "spearman") +
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) +  # 设置不同组的颜色
  scale_shape_manual(values = 21)  # 将所有组的点形状设置为实心圆

# 加上自己喜欢的主题
p3 <- p3 +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "right",
        axis.text.x.top = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black", size = 0.4),
        axis.line.x.top = element_line(color = "black", size = 0.4),
        axis.line.y.right = element_line(color = "black", size = 0.4)) +
  guides(x.sec = "axis", y.sec = "axis")
p3

#作图
p4 <- ggplot(data4 %>% na.omit(), aes(LPC, Acid, shape = "21", fill=Group)) +
  geom_point(size =3,color = "black",stroke=0.3, alpha=0.9) +
  geom_smooth(aes(color = Group), method = lm, se = TRUE) +
  stat_cor(method = "spearman") +
  scale_fill_manual(values = nature_colors) +
  scale_color_manual(values = nature_colors) +  # 设置不同组的颜色
  scale_shape_manual(values = 21)  # 将所有组的点形状设置为实心圆

# 加上自己喜欢的主题
p4 <- p4 +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "right",
        axis.text.x.top = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.line = element_line(colour = "black", size = 0.4),
        axis.line.x.top = element_line(color = "black", size = 0.4),
        axis.line.y.right = element_line(color = "black", size = 0.4)) +
  guides(x.sec = "axis", y.sec = "axis")
p4

#图合并
library(patchwork)

p1 <- p1 + theme(legend.position = 'none')
p2 <- p2 + theme(legend.position = 'none')
p3 <- p3 + theme(legend.position = 'none')
p1 + p2 + p3 + p4 + plot_layout(ncol = 2, widths = c(1, 1))
