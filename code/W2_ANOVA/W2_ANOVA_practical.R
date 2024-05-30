####################Part 1
drug_trial_df<-read.csv("D:/桌面/ADS2/W2_ANOVA/drug_trial.csv")

#H0: There is no effect of drug A and drug B on pain
#HA: There is an effect of drug A and drug B on pain

#ANOVA model
model<-aov(pain~treatment,drug_trial_df)

#Normality of residuals
#Normal Q-Q
plot(model,2)
shapiro.test(resid(model))#p-value =0.3895 normal

#Equality of variances
plot(model, 1) 
library(car)
leveneTest(model)

summary(model)
#Df degree of freedom
#Sum Sq平方和 平方差的总会
#Mean Sq均方 平方和除以自由度

#ANOVA确认整体性 Post-host test 详细比较
#确定具体哪些组存在差异 保守性调整
TukeyHSD(model)
#diff均值差，lwr-upr置信区间，p adj矫正p值


################################非参方法
#非参ANOVA 非参数检验法 Kruskal-Wallis test
kruskal.test(weight ~ group, data = my_data)
#非参 Post-host test 如果用KW检验多重比较，
pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")

