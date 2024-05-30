library(dplyr)
library(ggplot2)
Mice_WT<-read.csv("D:/桌面/ADS2/W6_linear_regressions/WT.csv")
Mice_KO<-read.csv("D:/桌面/ADS2/W6_linear_regressions/KO.csv")

Mice_WT$Genotype <- "WT"
Mice_KO$Genotype <- "KO"
mice <- rbind(Mice_WT, Mice_KO) %>%
  mutate(Sex = as.factor(Sex),    #分类便于作图，ANOVA，回归分析
         Genotype = as.factor(Genotype))

# 创建散点图并添加线性回归线  
ggplot(data = mice, aes(x = Age, y = Weight, color = Genotype)) +  
  geom_point(size = 3) +  # 绘制散点图  
  geom_smooth(method = "lm", se = T) +  # 添加线性回归线，se = FALSE表示不显示置信区间  
  scale_color_manual(values = c("WT" = "black", "KO" = "red")) +  # 设置WT和KO的颜色  
  labs(x = "Age, weeks", y = "Weight, g", color = "Genotype") +  # 添加轴标签和图例标签  
  theme_classic()  # 使用简洁的主题

