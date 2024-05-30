##chi square
##################################### 1. Simulation of the probability of goodness-of-fit test
# Define observed data
Poll_seasons <- data.frame(Spring = 40, Summer = 30, Autumn = 18, Winter = 28)
# Define expected proportions
equal_preferences <- sum(Poll_seasons) * 0.25

# Number of simulations
num_simulations <- 1000
# Initialize vector to store chi-square values
chi_square_values <- numeric(num_simulations)
# Generate a large population with expected proportions
population=rep(c("Spring", "Summer", "Autumn", "Winter"),500)  ###change population size

# Perform simulations
for (i in 1:num_simulations) {
  # sample from population
  simulated_data <- sample(population,
                           size = 100,       ###change sample size sample越大越平滑
                           replace = TRUE)
  
  # Calculate observed frequencies
  observed_frequencies <- table(simulated_data)

  #每个结果计算卡方 chi-square statistic
  chi_square_values[i] <- chisq.test(observed_frequencies, p = rep(0.25, 4), simulate.p.value = TRUE)$statistic
}

# observed data卡方
observed_chi_square <- chisq.test(Poll_seasons, p = rep(0.25, 4))$statistic

# p-value
p_value <- mean(chi_square_values >= observed_chi_square)

# Output results
cat("Observed chi-square value:", observed_chi_square, "\n")
cat("P-value:", p_value, "\n") #p=0.0384 模拟次数多更准确

###卡方检验 密度图
plot(density(chi_square_values))
#abline(v = observed_chi_square, col = "red", lty = 2)
#legend("topright", legend = c("Simulated Chi-square", "Observed Chi-square"),
#       col = c("black", "red"), lty = c(1, 2), cex = 0.8)


chisq.test(Poll_seasons,correct = F, p=rep(1/4,4)) #p=0.038


##################################### 2.  Chi-square distribution and degree of freedom

library(ggplot2)
dfs<-c(1,2,3,4,6,9)
color_vec<-rainbow(length(dfs))
chis<-c()
#plot(NULL,xlim=c(0,20),ylim=c(0,0.5))
for(i in 1:length(dfs)){
  chi<-rchisq(n=1000,dfs[i])
  lines(density(chi),lwd=2,col=color_vec[i])
}

####simon ggplot2写法
#rchisq
library(dplyr)
library(ggplot2)

lapply(1:10,function(i)    ##df=1:10批量逐个处理
  ##chisq列：1000个卡方检验的结果，df列因子结果不变
       data.frame(chisqr=rchisq(1000,df=i),df=factor(rep(i,1000))))%>% 
  bind_rows()%>% ###按照row bind上面的数据框  %>%复杂代码必备相当于在框外面
  ggplot(aes(chisqr,color=df))+theme_light()+
  geom_density()+labs(x="Simulated chi square values")



##################################### 3.  Chi-square test of homogeneity
M=matrix(c(5,1,1,9,8,5,2,5,9,8,3,9,18,16,12,5),byrow=T,nrow=4,
         dimnames = list(c("Severe allergies","Mild allergies","Sporadic allergies","Never allergies"),
                         c("Spring","Summer","Fall","Winter")))
mosaicplot(M,xlab = "Favorite season",ylab = "Observed Allergic Response",
           color = c("red","blue","green","yellow"),main = NA)

chisq.test(M,simulate.p.value = T)

##################################### 4.  Chi-square test and Fisher's exact test
M=matrix(c(7,2,3,7),nrow = 2,ncol = 2,byrow = T,
         dimnames = list(c("Alive","Dead"),c("WT","KO")))
chisq.test(M,correct = F);chisq.test(M,correct = T)



