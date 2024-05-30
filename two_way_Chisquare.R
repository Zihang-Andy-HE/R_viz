library(tidyverse)
#two way ANOVA
# �������ݼ�
teeth <- read.csv("D:/����/ADS_provide/teeth.csv")


# ������ݽṹ
str(teeth)

# ȷ��������û��NAֵ
sum(is.na(teeth))

# ��supp��dose����ת��Ϊ���ӣ�����dose��������˳���ϵ
teeth$dose <- factor(teeth$dose, levels = c(0.5, 1, 2), ordered = TRUE)
teeth$supp <- as.factor(teeth$supp)

# ����������
teeth <- teeth %>% 
  relocate(supp, .before = len)

# ���������ط������
aov_result <- aov(len ~ supp * dose, data = teeth)
plot(aov_result,which=2)
plot(aov_result,which=3)

# �鿴ANOVA���
summary(aov_result)

# ���رȽ�
TukeyHSD(aov_result)


#################Chi_square
# ��������
genotype <- read.csv("D:/����/ADS_provide/genotype.csv")

# ������ݽṹ
str(genotype)

# ����һ���������ڿ�������
# ��������ֻ�����Ա�ͻ����ͣ�WT, het, mut��
table_genotype <- table(genotype$sex, genotype$genotype)
####!!!����ת��Ϊmatrix
table_genotype<-matrix(table_genotype)
# ��������ֵ�������ϵ¶��Ŵ����ɣ�Ԥ�ڱ���Ϊ�Ա�50/50��������25/50/25��
expected_counts <- matrix(c( 0.25, 0.125, 0.125,0.25, 0.125, 0.125), nrow = 2, byrow = TRUE)
colnames(expected_counts) <- c("het","mut","WT")
rownames(expected_counts) <- c("female", "male")

# ��������Ŷȼ���
chisq.test(table_genotype, p = expected_counts,correct=FALSE)
