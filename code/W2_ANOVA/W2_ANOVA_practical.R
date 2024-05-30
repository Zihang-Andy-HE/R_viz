####################Part 1
drug_trial_df<-read.csv("D:/����/ADS2/W2_ANOVA/drug_trial.csv")

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
#Sum Sqƽ���� ƽ������ܻ�
#Mean Sq���� ƽ���ͳ������ɶ�

#ANOVAȷ�������� Post-host test ��ϸ�Ƚ�
#ȷ��������Щ����ڲ��� �����Ե���
TukeyHSD(model)
#diff��ֵ�lwr-upr�������䣬p adj����pֵ


################################�ǲη���
#�ǲ�ANOVA �ǲ������鷨 Kruskal-Wallis test
kruskal.test(weight ~ group, data = my_data)
#�ǲ� Post-host test �����KW������رȽϣ�
pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")
