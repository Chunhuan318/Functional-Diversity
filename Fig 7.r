#下载linkET包
install.packages('devtools')
devtools::install_github('Hy4m/linkET')

library(linkET)
library(ggplot2)
library(dplyr)

#读取数据
setwd("D:/R代码")
div <- read.delim('功能多样性.csv',header = TRUE,
                  sep = ",")
env <- read.delim('环境变量x.csv',header = TRUE,
                  sep = ",")
#计算 Mantel 相关性
#通过 spec_select 指定数据组范围，例如这个示例数据中，微生物矩阵的第1-22列是物种丰度数据（指定名称 Taxonomy），第23-40列是基因丰度数据（指定名称 Function）
#默认情况下，对 spec 计算 Bray-Curtis 距离，对 env 计算 Euclidean 距离，然后计算二者 Mantel 相关
mantel <- mantel_test(spec = div, env = env, spec_select = list(FRic = 1, FEve = 2, FDis = 3, Rao = 4), 
                      mantel_fun = 'mantel')

#根据相关系数和显著性设置标签，以便作图时定义线宽和颜色
mantel <- mutate(mantel, 
                 rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf), labels = c('< 0.2', '0.2 - 0.4', '>= 0.4')),
                 pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c('< 0.01', '0.01 - 0.05', '>= 0.05'))
)
mantel

#绘制相关图
qcorrplot(correlate(env, method = 'spearman'), type = 'upper', diag = FALSE) +  #环境变量矩阵计算 Spearman 相关系数
  geom_square() +  #绘制 Spearman 相关系数热图
  geom_mark(sep = '\n', size = 2.5, sig.thres = 0.05) +  #显示 Spearman 相关系数和显著性
  geom_couple(aes(color = pd, size = rd), data = mantel, curvature = nice_curvature()) +  #环境和微生物的相关性展示为上述 Mantel 相关
  scale_fill_gradientn(colors = c("#9B2226", "#AE2021","#BB3E03","#CA6702","#FFFFFF",
                                  "#90e0ef", "#0077b6","#023e8a","#001219"), limits = c(-1, 1)) +  #根据 Spearman 相关指定热图颜色
  scale_size_manual(values = c(0.5, 1, 2)) +  #根据 Mantel 相关指定线条粗细
  scale_color_manual(values = c('#D95F02', '#1B9E77', '#E0E0E0')) +  #根据 Mantel 相关 p 值指定线条颜色
  guides(color = guide_legend(title = "Mantel's p", order = 1), #图例标题和排序
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "Spearman's r", order = 3)) +
  theme(legend.key = element_blank())



##ggcor法绘制相关热图
library(ggcor)
library(ggplot2)
library(dplyr)

#读取示例数据
setwd("D:/R代码")
div <- read.delim('多样性.csv',header = TRUE,
                  sep = ",")
env <- read.delim('环境变量.csv',header = TRUE,
                  sep = ",")
#计算 Mantel 相关性
#通过 spec.select 指定数据组范围，例如这个示例数据中，微生物矩阵的第1-22列是物种丰度数据（指定名称 Taxonomy），第23-40列是基因丰度数据（指定名称 Function）
#默认情况下，对 spec 计算 Bray-Curtis 距离，对 env 计算 Euclidean 距离，然后计算二者 Mantel 相关
mantel <- fortify_mantel(div, env, mantel.fun = 'mantel.randtest',
    div.dist.method = 'euclidean', env.dist.method = 'euclidean',
    spec.select = list(H = 1, D = 2, R = 3, E = 4) 
    )

#根据相关系数和显著性设置标签，以便作图时定义线宽和颜色
mantel <- mutate(mantel, 
    r = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
    p.value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE)
)
mantel

#绘制相关图，各参数和上述很像就随便设置了
quickcor(env, cor.test = TRUE, type = 'upper') +
geom_square(data = get_data(type = 'upper', show.diag = FALSE)) +
geom_mark(data = get_data(type = 'upper', show.diag = FALSE), sep = '\n', size = 2.5, sig.thres = 0.05) +
add_link(mantel, mapping = aes(color = p.value, size = r), diag.label = TRUE) +
scale_size_manual(values = c(0.1, 0.5, 1)) + 
scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +
scale_color_manual(values = c('#56B4E9', '#E69F00', '#999999')) + 
guides(color = guide_legend(title = "Mantel's p", order = 1), #图例标题和排序
    size = guide_legend(title = "Mantel's r", order = 2), 
    fill = guide_colorbar(title = "Pearson's r", order = 3)) +
add_diag_label(angle = 45) +  #如果想在对角线显示环境变量标签
remove_axis('all') +
theme(legend.key = element_blank())
