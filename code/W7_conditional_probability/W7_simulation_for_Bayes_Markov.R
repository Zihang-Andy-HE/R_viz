######################Lie detector problem

employee_num=10000
steal_rate=0.1
Nonsteal_rate=0.9
correct_rate=0.8
wrong_rate=0.2

##1. By setting up and running a simulation
#binomial distribution ����ֲ�
thieves<-rbinom(employee_num,1,steal_rate)

#���� thief+correct & nonthief+incorrect
thieves_detected<-rbinom(employee_num,1,ifelse(thieves,correct_rate,wrong_rate))

prob<-sum(thieves+thieves_detected==2)/sum(thieves_detected)
prob
thieves_in_detected=50*prob
thieves_in_detected


##2. By using Bayes�� theorem
#P(T�OL)= P(L�OT)��P(T)/P(L)
#thief in detected thief
p_detected_thief=0.1*0.8+0.9*0.2 #P(L)
p_detected_in_thief=0.8 #P(L�OT)
p_thief=0.1 #P(T)

prob_thief_in_detected<-p_detected_in_thief*p_thief/p_detected_thief


######################Coin toss
# ���ò���
target_sequence <- c("H", "T", "T", "H")  # Ŀ������
num_simulations <- 10000  # ģ�����
results <- numeric(num_simulations)  # �洢ÿ��ģ��Ľ��

# ���庯����ģ����Ӳ��ֱ������Ŀ�����в������������
simulate_until_target <- function() {
  tosses <- c()  # �洢ÿ����Ӳ�ҵĽ��
  while (TRUE) {
    # ��Ӳ�ң����ΪH��T
    toss <- sample(c("H", "T"), 1, replace = TRUE)
    tosses <- c(tosses, toss)
    
    # ����Ƿ������Ŀ������
    if (length(tosses) >= length(target_sequence) &&
        all(tosses[(length(tosses) - length(target_sequence) + 1):length(tosses)] == target_sequence)) {
      return(length(tosses))  # ������Ӳ�ҵĴ��� ѭ����������
    }
  }
}

# ���ж��ģ��
for (i in 1:num_simulations) {
  results[i] <- simulate_until_target()
}

# ����ƽ��ֵ
average_tosses <- mean(results)

# ��ӡ���
print(paste("ƽ����Ӳ�Ҵ���ֱ����������'H-T-T-H':", average_tosses))





