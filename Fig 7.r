#����linkET��
install.packages('devtools')
devtools::install_github('Hy4m/linkET')

library(linkET)
library(ggplot2)
library(dplyr)

#��ȡ����
setwd("D:/R����")
div <- read.delim('���ܶ�����.csv',header = TRUE,
                  sep = ",")
env <- read.delim('��������x.csv',header = TRUE,
                  sep = ",")
#���� Mantel �����
#ͨ�� spec_select ָ�������鷶Χ���������ʾ�������У�΢�������ĵ�1-22�������ַ�����ݣ�ָ������ Taxonomy������23-40���ǻ��������ݣ�ָ������ Function��
#Ĭ������£��� spec ���� Bray-Curtis ���룬�� env ���� Euclidean ���룬Ȼ�������� Mantel ���
mantel <- mantel_test(spec = div, env = env, spec_select = list(FRic = 1, FEve = 2, FDis = 3, Rao = 4), 
                      mantel_fun = 'mantel')

#�������ϵ�������������ñ�ǩ���Ա���ͼʱ�����߿����ɫ
mantel <- mutate(mantel, 
                 rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf), labels = c('< 0.2', '0.2 - 0.4', '>= 0.4')),
                 pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c('< 0.01', '0.01 - 0.05', '>= 0.05'))
)
mantel

#�������ͼ
qcorrplot(correlate(env, method = 'spearman'), type = 'upper', diag = FALSE) +  #��������������� Spearman ���ϵ��
  geom_square() +  #���� Spearman ���ϵ����ͼ
  geom_mark(sep = '\n', size = 2.5, sig.thres = 0.05) +  #��ʾ Spearman ���ϵ����������
  geom_couple(aes(color = pd, size = rd), data = mantel, curvature = nice_curvature()) +  #������΢����������չʾΪ���� Mantel ���
  scale_fill_gradientn(colors = c("#9B2226", "#AE2021","#BB3E03","#CA6702","#FFFFFF",
                                  "#90e0ef", "#0077b6","#023e8a","#001219"), limits = c(-1, 1)) +  #���� Spearman ���ָ����ͼ��ɫ
  scale_size_manual(values = c(0.5, 1, 2)) +  #���� Mantel ���ָ��������ϸ
  scale_color_manual(values = c('#D95F02', '#1B9E77', '#E0E0E0')) +  #���� Mantel ��� p ֵָ��������ɫ
  guides(color = guide_legend(title = "Mantel's p", order = 1), #ͼ�����������
         size = guide_legend(title = "Mantel's r", order = 2), 
         fill = guide_colorbar(title = "Spearman's r", order = 3)) +
  theme(legend.key = element_blank())



##ggcor�����������ͼ
library(ggcor)
library(ggplot2)
library(dplyr)

#��ȡʾ������
setwd("D:/R����")
div <- read.delim('������.csv',header = TRUE,
                  sep = ",")
env <- read.delim('��������.csv',header = TRUE,
                  sep = ",")
#���� Mantel �����
#ͨ�� spec.select ָ�������鷶Χ���������ʾ�������У�΢�������ĵ�1-22�������ַ�����ݣ�ָ������ Taxonomy������23-40���ǻ��������ݣ�ָ������ Function��
#Ĭ������£��� spec ���� Bray-Curtis ���룬�� env ���� Euclidean ���룬Ȼ�������� Mantel ���
mantel <- fortify_mantel(div, env, mantel.fun = 'mantel.randtest',
    div.dist.method = 'euclidean', env.dist.method = 'euclidean',
    spec.select = list(H = 1, D = 2, R = 3, E = 4) 
    )

#�������ϵ�������������ñ�ǩ���Ա���ͼʱ�����߿����ɫ
mantel <- mutate(mantel, 
    r = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
    p.value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE)
)
mantel

#�������ͼ����������������������������
quickcor(env, cor.test = TRUE, type = 'upper') +
geom_square(data = get_data(type = 'upper', show.diag = FALSE)) +
geom_mark(data = get_data(type = 'upper', show.diag = FALSE), sep = '\n', size = 2.5, sig.thres = 0.05) +
add_link(mantel, mapping = aes(color = p.value, size = r), diag.label = TRUE) +
scale_size_manual(values = c(0.1, 0.5, 1)) + 
scale_fill_gradientn(colors = c('#053061', '#68A8CF', 'white', '#F7B394', '#67001F'), limits = c(-1, 1)) +
scale_color_manual(values = c('#56B4E9', '#E69F00', '#999999')) + 
guides(color = guide_legend(title = "Mantel's p", order = 1), #ͼ�����������
    size = guide_legend(title = "Mantel's r", order = 2), 
    fill = guide_colorbar(title = "Pearson's r", order = 3)) +
add_diag_label(angle = 45) +  #������ڶԽ�����ʾ����������ǩ
remove_axis('all') +
theme(legend.key = element_blank())
