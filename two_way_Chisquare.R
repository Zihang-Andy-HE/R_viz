library(tidyverse)
#two way ANOVA
# 加载数据集
teeth <- read.csv("D:/桌面/ADS_provide/teeth.csv")


# 检查数据结构
str(teeth)

# 确保数据中没有NA值
sum(is.na(teeth))

# 将supp和dose变量转换为因子，并对dose变量设置顺序关系
teeth$dose <- factor(teeth$dose, levels = c(0.5, 1, 2), ordered = TRUE)
teeth$supp <- as.factor(teeth$supp)

# 重新排列列
teeth <- teeth %>% 
  relocate(supp, .before = len)

# 进行两因素方差分析
aov_result <- aov(len ~ supp * dose, data = teeth)
plot(aov_result,which=2)
plot(aov_result,which=3)

# 查看ANOVA结果
summary(aov_result)

# 多重比较
TukeyHSD(aov_result)


#################Chi_square
# 导入数据
genotype <- read.csv("D:/桌面/ADS_provide/genotype.csv")

# 检查数据结构
str(genotype)

# 创建一个表格，用于卡方检验
# 假设我们只考虑性别和基因型（WT, het, mut）
table_genotype <- table(genotype$sex, genotype$genotype)
####!!!必须转化为matrix
table_genotype<-matrix(table_genotype)
# 计算期望值（根据孟德尔遗传定律，预期比例为性别50/50，基因型25/50/25）
expected_counts <- matrix(c( 0.25, 0.125, 0.125,0.25, 0.125, 0.125), nrow = 2, byrow = TRUE)
colnames(expected_counts) <- c("het","mut","WT")
rownames(expected_counts) <- c("female", "male")

# 卡方拟合优度检验
chisq.test(table_genotype, p = expected_counts,correct=FALSE)

