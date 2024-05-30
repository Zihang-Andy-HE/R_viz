######################Lie detector problem

employee_num=10000
steal_rate=0.1
Nonsteal_rate=0.9
correct_rate=0.8
wrong_rate=0.2

##1. By setting up and running a simulation
#binomial distribution 二项分布
thieves<-rbinom(employee_num,1,steal_rate)

#核心 thief+correct & nonthief+incorrect
thieves_detected<-rbinom(employee_num,1,ifelse(thieves,correct_rate,wrong_rate))

prob<-sum(thieves+thieves_detected==2)/sum(thieves_detected)
prob
thieves_in_detected=50*prob
thieves_in_detected


##2. By using Bayes’ theorem
#P(T∣L)= P(L∣T)×P(T)/P(L)
#thief in detected thief
p_detected_thief=0.1*0.8+0.9*0.2 #P(L)
p_detected_in_thief=0.8 #P(L∣T)
p_thief=0.1 #P(T)

prob_thief_in_detected<-p_detected_in_thief*p_thief/p_detected_thief


######################Coin toss
# 设置参数
target_sequence <- c("H", "T", "T", "H")  # 目标序列
num_simulations <- 10000  # 模拟次数
results <- numeric(num_simulations)  # 存储每次模拟的结果

# 定义函数：模拟抛硬币直到出现目标序列并返回所需次数
simulate_until_target <- function() {
  tosses <- c()  # 存储每次抛硬币的结果
  while (TRUE) {
    # 抛硬币，结果为H或T
    toss <- sample(c("H", "T"), 1, replace = TRUE)
    tosses <- c(tosses, toss)
    
    # 检查是否出现了目标序列
    if (length(tosses) >= length(target_sequence) &&
        all(tosses[(length(tosses) - length(target_sequence) + 1):length(tosses)] == target_sequence)) {
      return(length(tosses))  # 返回抛硬币的次数 循环函数结束
    }
  }
}

# 进行多次模拟
for (i in 1:num_simulations) {
  results[i] <- simulate_until_target()
}

# 计算平均值
average_tosses <- mean(results)

# 打印结果
print(paste("平均抛硬币次数直到出现序列'H-T-T-H':", average_tosses))






