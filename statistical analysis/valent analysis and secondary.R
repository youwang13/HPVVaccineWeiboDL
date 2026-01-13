library(forecast)
library(dplyr)

setwd("C:/YOU/research/HPV AI/paper/manuscript/Results/分价分析")
vdata = read.csv("249 for R.csv")

chi_attitude = as.matrix(data.frame(row.names = c("male","female"),
  Attitude_0 = vdata$attitude_0,
  Attitude_1 = vdata$attitude_1))

chisq.test(chi_attitude)
fisher.test(chi_attitude, simulate.p.value = TRUE)
mcnemar.test(chi_attitude)


chi_behavior = as.matrix(data.frame(row.names = c("2","4","9"),
                                    behavior_0 = vdata$count-vdata$Behavior,
                                    behavior_1 = vdata$Behavior))
chi_behavior_24 = chi_behavior[c("2","4"),c("behavior_0","behavior_1")]

chisq.test(chi_behavior)
fisher.test(chi_behavior, simulate.p.value = TRUE)


chi_disease_risk = as.matrix(data.frame(row.names = c("2","4","9"),
                                    perceived_disease_risk_0 = vdata$count-vdata$Perceived.Disease.Risk,
                                    perceived_disease_risk_1 = vdata$Perceived.Disease.Risk))

chisq.test(chi_disease_risk)
fisher.test(chi_disease_risk, simulate.p.value = TRUE)


chi_benefits = as.matrix(data.frame(row.names = c("2","4","9"),
                                    benefits_0 = vdata$count-vdata$Perceived.benefits,
                                    benefits_1 = vdata$Perceived.benefits))

chisq.test(chi_benefits)
fisher.test(chi_benefits, simulate.p.value = TRUE)


chi_perceived_b = as.matrix(data.frame(row.names = c("2","4","9"),
                                    perceived_b_0 = vdata$count-vdata$Perceived.barriers,
                                    perceived_b_1 = vdata$Perceived.barriers))

chisq.test(chi_perceived_b)
fisher.test(chi_perceived_b, simulate.p.value = TRUE)


chi_practical_b = as.matrix(data.frame(row.names = c("2","4","9"),
                                    practical_b_0 = vdata$count-vdata$Practical.barriers,
                                    practical_b_1 = vdata$Practical.barriers))

chisq.test(chi_practical_b)
fisher.test(chi_practical_b, simulate.p.value = TRUE)


chi_mis = as.matrix(data.frame(row.names = c("2","4","9"),
                                    mis_0 = vdata$count-vdata$Misinformation,
                                    mis_1 = vdata$Misinformation))

chisq.test(chi_mis)
fisher.test(chi_mis, simulate.p.value = TRUE)


chi_social = as.matrix(data.frame(row.names = c("2","4","9"),
                                    social_0 = vdata$count-vdata$Social.norms,
                                    social_1 = vdata$Social.norms))

chisq.test(chi_social)
fisher.test(chi_social, simulate.p.value = TRUE)

table=as.matrix(data.frame(row.names = c("less4","more4"),
                     dead = c(21,4),
                     alive = c(2,8)))
fisher.test(table,alternative="greater", conf.int=TRUE)
