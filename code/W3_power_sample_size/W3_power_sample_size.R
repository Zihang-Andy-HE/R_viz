#W4power sample size



########################one sample 题干中只说了另一组的均值
###10 simulation
#null: 175, alternative 
ps.175<-replicate(1e5,t.test(rnorm(10,178,10),mu=175,alternative = 'greater')$p.value)
power.175<-sum(ps.175<=0.05)/1e5 #~0.22


#If you measure 50 students instead of 10 students, then does the power change?
###50 simulation sample size bigger, power bigger
set.seed(88)
ps.175_50<-replicate(1e5,t.test(rnorm(50,178,10),mu=175,alternative = 'greater')$p.value)
power.175_50<-length(which(ps.175_50<=0.05))/1e5  #~0.67



increased_mean=178
national_mean=175
sd=10
power_values<-c()

###########sample size 代表不同的取样不同的返回
x_axis<-seq(from=5, to=100, by=5)
for (i in 1:length(x_axis)) {  
  n <- x_axis[i]  
  # 使用power.t.test函数计算功效  
  power_result <- power.t.test(  
    delta = increased_mean - national_mean,  # effect size 
    sd = sd,                                # sd
    n = n,                                 # sample size
    sig.level = 0.05,                     # significance
    alternative = "one.sided"                #Ha one/two
  )  
  power_values[i] <- power_result$power  #power
}  

#Plot the power versus N
plot(x_axis, power_values, type = "l", lwd = 2, col = "blue",  
     xlab = "Sample Size (N)", ylab = "Power",  
     main = "Power Calculation for Different Sample Sizes")  


########################two sample 两组数据都有

##If the drug is indeed effective as showed on the animal model, then what is the probability that they do
##not see a significant effect of the drug (p-value cutoff = 0.05)? Please perform a simulation as you did
##before to give an answer

ps.130<-replicate(1e5,t.test(rnorm(10,130,30),rnorm(10,117,30),alternative = 'greater')$p.value)
power.130<-length(which(ps.130<=0.05))/1e5

## Do you think the power is good enough? If the company truly believes the effect of the drug and want
##to be sure that they will not largely miss the effect in the trial (type II error rate < 0.2), then how
##many volunteers do they need to recruit?
##法一：type II error rate < 0.2，while循环power>0.8 break 
drug_weight<-117
placebo_weight<-130
sd=30

n <- 10
power_result <- NULL
while (is.null(power_result) || power_result$power <= 0.8) {
  n <- n + 1
  power_result <- power.t.test(
    delta = placebo_weight - drug_weight,  # effect size 
    sd = sd,                                # sd
    n = n,                                  # sample size
    sig.level = 0.05,                       # significance
    alternative = "one.sided"               # Ha one/two
  )
}
print(n)
##法二：                             #####加上power，不写n 生成结果n
power_result <- power.t.test(
  delta = placebo_weight - drug_weight,  # effect size 
  sd = sd,                                # sd
  n = NULL,                                  # sample size
  sig.level = 0.05,                       # significance
  power = 0.8,                             #power
  alternative = "one.sided"               # Ha one/two
)
power_result$n

## If the company changes their strategy, asking all the volunteers to take the pills and measuring their
##weights before and afterward, how many volunteers do they need?

power_result <- power.t.test(
  delta = placebo_weight - drug_weight,  # effect size 
  sd = sd,                                # sd
  n = NULL,                                  # sample size
  sig.level = 0.05,                       # significance
  power = 0.8,                             #!!! to calculate the sample size
  alternative = "one.sided",               # Ha one/two
  type = "paired" #!!!
)
power_result$n

as.data.frame(power_result$sig.level)


drug_weight<-117
placebo_weight<-130
sd=30




#############different  x =  p_values ->  y = N
x_axis<-seq(from=0.01, to=0.05, by=0.01)
samble_size<-c()
for (i in 1:length(x_axis)) {  
  p_value <- x_axis[i]  
  # 使用power.t.test函数计算功效  
  power_result <- power.t.test(  
    delta = placebo_weight - drug_weight,  # effect size 
    sd = 30,                                # sd
    n = NULL,                                 # sample size to be calculated
    power = 0.8, 
    sig.level = p_value,                     # significance
    alternative = "one.sided"                #Ha one/two
  )  
  samble_size[i] <- power_result$n  #n    !!!!勿忘修改
} 



plot(x_axis,samble_size,  type="b",xlab = "alpha", ylab = "Sample Size (N)",  col="blue", lwd = 4, 
     main = "Sample Sizes Calculation for Different Alpha")



#############different  x =  powers ->  y = N
x_axis<-seq(from=0.5, to=0.9, by=0.05)
samble_size<-c()
for (i in 1:length(x_axis)) {  
  powers <- x_axis[i]  
  # 使用power.t.test函数计算功效  
  power_result <- power.t.test(  
    delta = placebo_weight - drug_weight,  # effect size 
    sd = 30,                                # sd
    n = NULL,                                 # sample size to be calculated
    power = powers, 
    sig.level = 0.05,                     # significance
    alternative = "one.sided"                #Ha one/two
  )  
  samble_size[i] <- power_result$n  #n    !!!!勿忘修改
} 



plot(x_axis,samble_size,  type="b",xlab = "Powers", ylab = "Sample Size (N)",  col="blue", lwd = 4, 
     main = "Sample Sizes Calculation for Different Powers")
