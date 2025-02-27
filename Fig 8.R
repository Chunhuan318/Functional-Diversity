library(lavaan)
setwd("D:/英文第三篇R文件")
a=read.csv("SEM2.csv",header = TRUE,
           sep = ",")
dat <- data.frame(scale(a))

sem.model1 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SOC ~ DN+DS+DBC
    SpH ~ DN+DS+DBC+DpH
    STP ~ DN+DS+DBC
    Fric ~ DN+DS+DBC+SOC+SpH+STP
    SOC ~~ SpH
    SOC ~~ STP
  ",
  data = dat)
summary(sem.model1, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model2 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    STP ~ DN+DS+DBC+DpH
    SOC ~ DN+DS+DBC+DpH
    Feve ~ DN+DS+DBC+SOC+STP
    SOC ~~ STP
  ",
  data = dat)
summary(sem.model2, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model3 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    AKP ~ DN+DS+DBC
    STP ~ DN+DS+DBC+AKP+DpH
    Fdis ~ DN+DS+DBC+AKP+STP+DpH
  ",
  data = dat)
summary(sem.model3, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model4 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    BG ~ DN+DS+DBC+DpH
    SOC ~ DN+DS+DBC+BG
    Rao ~ DN+DS+DBC+BG+SOC+DpH
  ",
  data = dat)
summary(sem.model4, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model5 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    SLA ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model5, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model6 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    SLDM ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model6, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model7 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    CHL ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model7, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model8 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    LT ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model8, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model9 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    LCC ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model9, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model10 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    LNC ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model10, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model11 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    LPC ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model11, standardized=TRUE,rsq=TRUE,modindices=TRUE)

sem.model12 <- sem(
  model = "
    DpH ~ DN+DS+DBC
    SEA ~ DN+DS+DBC+DpH
    SpH ~ DN+DS+DBC+DpH
    SAV ~ DN+DS+DBC+DpH
    LD ~ DN+DS+DBC+SEA+SpH+SAV
    SEA ~~  SAV
  ",
  data = a)
summary(sem.model12, standardized=TRUE,rsq=TRUE,modindices=TRUE)
