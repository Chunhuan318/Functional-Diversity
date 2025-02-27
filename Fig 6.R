#读取数据
setwd("D:/英文第三篇R文件")
dat=read.csv("多样性1.csv",header = T,
             sep = ",")

#归一化所有变量，保证相同量纲
dat_select <- data.frame(scale(dat))

#拟合一个线性模型来确定物种丰富度、功能多样性，以及气候、土壤等多种非生物因子对生态系统稳定性的影响
#这里使用所有节选的变量拟合一个全模型
fit <- lm(S~., dat)
summary(fit)

library(MuMIn)
options(na.action = 'na.fail')

#模型选择（默认情况下，根据 AIC 值对所有模型子集进行排序），详情 ?dredge
fit_select <- dredge(fit)
head(fit_select)

#获取最好的模型（默认情况下选择第一个模型子集，即 AIC 值最小的）
fit_select1 <- summary(get.models(fit_select, 1)[[1]])
fit_select1

#提取回归系数（不含截距）、回归系数的标准误、P 值等
stat.lm <- fit_select1$coefficients[-1, ]
stat.lm <- data.frame(stat.lm, check.names = FALSE)
stat.lm$env <- rownames(stat.lm)
stat.lm$sig <- ifelse(stat.lm$'Pr(>|t|)'>0.05, '', ifelse(stat.lm$'Pr(>|t|)'>0.01, '*', ifelse(stat.lm$'Pr(>|t|)'>0.001, '**', '***')))
stat.lm$label <- paste(stat.lm$env, stat.lm$sig)
stat.lm

#为解释变量添加分组
#stat.lm[which(stat.lm$env %in% c('SpH', 'SNH', 'Sna', 'STP', 'Sca', 'SMg', 'SOC', 'SNO', 'SAP', 'SK')),'type'] <- 'Soil property'
#stat.lm[which(stat.lm$env %in% c('DS', 'Dna', 'Dca', 'DpH', 'DK', 'DMg')),'type'] <- 'Deposition property'
stat.lm[which(stat.lm$env %in% c('Fric', 'Feve', 'Fdis', 'Rao')),'type'] <- 'Deposition property'

stat.lm <- stat.lm[order(stat.lm$type, stat.lm$ Estimate), ]
stat.lm$env <- factor(stat.lm$env, levels = stat.lm$env)

#绘制森林图展示变量的标准化回归系数、回归系数的标准误、P 值等
library(ggplot2)

p1 <- ggplot(stat.lm, aes(x = env, y = Estimate, color = type)) +  #x 轴环境变量，y 轴标准化后的回归系数（最后再对 x、y 作个翻转）
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.1, size = 1, show.legend = FALSE) +  #误差线表示回归系数的标准误
  scale_color_manual(values = c('#FD4400', '#4775FE'), limits = c('Soil property', 'Deposition property')) +
  coord_flip() +  #横纵轴作个翻转
  geom_hline(yintercept = 0, linetype = 2) +  #0 位置处画一个虚线
  labs(x = '', y = 'Parameter estimate', color = '') +  
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        axis.line.x = element_line(), axis.ticks.y = element_blank(), 
        legend.key = element_blank()) +
  scale_x_discrete(breaks = stat.lm$env, labels = stat.lm$label, position = 'top') +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.25, 0.25))

p1
#可以使用 rdacca.hp 包中层次分割的方法，计算线性模型中解释变量的方差解释率
library(rdacca.hp)

iv <- list(
  Soil = dat_select[c( 'SpH', 'SNH', 'Sna', 'STP', 'Sca', 'SMg', 'SOC', 'SNO', 'SAP', 'SK', 'STN', 'SAP', 'SMg')], 
  Deposition = dat_select[c('DS', 'Dna', 'Dca', 'DpH', 'DK', 'DMg', 'DNO', 'DNH')]
)

hp <- rdacca.hp(dat_select['Rao'], iv, method = 'RDA', type = 'adjR2')
hp  #Individual effect 即为各因子的解释率（层次分割方法通过平均分配共同解释的 R2 部分，降低了共线性的影响）

#可以看到这儿的校正后的 R2 与上述回归中的校正后的 R2 是一样的
hp$Total_explained_variation
fit_select1$adj.r.squared
#提取 Individual effect，即为 relative effect of estimates
#Individual effect 的和等于校正后的 R2，本示例即 0.716
hp.ie <- data.frame(hp$Hier.part, check.names = FALSE)
hp.ie$env <- rownames(hp.ie)
hp.ie$env <- factor(hp.ie$env, levels = rev(hp.ie$env))
hp.ie$exp_var <- ''
hp.ie

#使用柱形图来可视化解释变量的方差解释率
p2 <- ggplot(hp.ie, aes(x = exp_var, y = Individual*100, fill = env)) + 
  geom_col(width = 0.6) +  #堆叠柱形图可视化 Individual effect，即 relative effect of estimates
  scale_fill_manual(values = c('#FD4400', '#4775FE')) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        axis.ticks.x = element_blank(), axis.line.y = element_line(color = 'gray30')) +
  labs(x = paste('adjR2 =', round(hp$Total_explained_variation, 3)), y = 'relative effect of estimates (%)', fill = '') +
  scale_x_discrete(position = 'top') +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

p2

#与上述标准化回归系数、回归系数的标准误、P 值等的森林图合并
library(patchwork)

p2 <- p2 + theme(legend.position = 'none')
p2 + p1 + plot_layout(ncol = 2, widths = c(1, 2))
#输出尺寸：6*8


