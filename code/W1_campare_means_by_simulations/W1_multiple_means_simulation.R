library(ggplot2)
trial <-
  read.csv("D:/桌面/ADS2/W1_campare_means_by_simulations/drug_trial.csv")
#H0: These groups are the same
#HA: These groups are different
##########sample 随便取两个看看
sample_index <- sample(1:nrow(trial), 2)
trial[sample_index, ]

###难点 具体如何给同组和异组取样===>>随机取样，根据其取样后的情况判断同/异
same_treatment <- c()
diff_treatment <- c()

for (i in 1:1000) {
  # choose 2 (different) rows from the total number of rows
  sample_index <- sample(1:nrow(trial), 2)
  # read out those two rows from trial. This is your sample
  # (maybe save it as a separate object, but it's not necessary)
  sample_row <- trial[sample_index, ]
  # For the two points in your sample, read out the pain indices
  sample1 <- sample_row[1, ]
  sample2 <- sample_row[2, ]
  # and determine their absolute difference
  sample_diff <- abs(sample1$pain - sample2$pain)
  # For the two points in your sample, decide whether they belong
  # to the same or to different treatment groups !!!!!!!取样完再判断
  if (sample1$treatment == sample2$treatment) {
    same_treatment <- c(same_treatment, sample_diff)
  } else{
    diff_treatment <- c(diff_treatment, sample_diff)
  }
}
allsample_df <- data.frame(value = same_treatment, group = "same")
allsample_df <-
  rbind(allsample_df,
        data.frame(value = diff_treatment, group = "different"))

ggplot(allsample_df, aes(x = group, y = value, fill = group)) +
  theme_classic() +
  geom_boxplot(width = 0.5,
               color = "black",
               alpha = 0.5) +
  scale_fill_manual(values = c("#B0E2FF", "#FFB5C5")) +
  ylab("Pain levels")

mean(diff_treatment)
mean(same_treatment)
abs(mean(diff_treatment) - mean(same_treatment))

shapiro.test(diff_treatment)#p-value = 4.454e-14<0.05 not normal distribution
shapiro.test(same_treatment)#p-value = 4.731e-12<0.05 not normal distribution
#var.test(diff_treatment, same_treatment)
#p-value =  <2.2e-16 variance not equal

wilcox.test(diff_treatment, same_treatment) #p-value < 2.2e-16

java_home_path <- "E:/JDK/jre" 
Sys.setenv(JAVA_HOME = java_home_path)
library(Deducer)
perm.t.test(diff_treatment, same_treatment)
#p-value < 2.2e-16



######################################Problem set1
jelly_beans<-read.csv("D:/桌面/ADS2/W1_campare_means_by_simulations/jellybeans.csv")
unique(jelly_beans$colour)


