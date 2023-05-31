## IMPORT DATA
heart <- read.csv("https://raw.githubusercontent.com/ProfNascimento/Bayes-Logistic/main/heart.csv")
str(heart)

DB=fastDummies::dummy_cols(heart,
      select_columns = c("Sex","ChestPainType","RestingECG","ExerciseAngina","ST_Slope"),
                           remove_selected_columns=TRUE)
names(DB)

library(brms) #Bayesian (multilevel) generalized linear modelling
Bayes_Model_Binary <- brm(formula = HeartDisease ~ .,  
                          data = DB, 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                          iter = 2000, 
                          chains = 1, 
                          inits= "0", 
                          cores=2,
                          seed = 123)
summary(Bayes_Model_Binary)
bayesplot::mcmc_trace(Bayes_Model_Binary, pars = c("b_Age"))
hist(bayesplot::mcmc_trace(Bayes_Model_Binary, pars = c("b_Age"))$data$value)
mean(bayesplot::mcmc_trace(Bayes_Model_Binary, pars = c("b_Age"))$data$value)

shinystan::launch_shinystan(Bayes_Model_Binary)

marginal_effects(Bayes_Model_Binary)
bsummary(Bayes_Model_Binary)
#use the `predict()` function to calculate the predicted probabilities of pupils in the original data from the fitted model
Pred <- predict(Bayes_Model_Binary, type = "response")

require(dplyr)
RES=data.frame(yhat=Pred[,1],y=DB$HeartDisease) %>% arrange(yhat)
plot(RES[,1],col=(RES[,2]+1),pch=14) #Back=0 & Red=1
abline(h=0.5,lty=2)

boxplot(RES[,1]~RES[,2],xlab="TYPE",ylab="P(Y=1|Xs)")
abline(h=0.5,col="red")

pPred <- ifelse(Pred[,1] > 0.5, 1, 0)
ConfusionMatrix <- table(pPred, DB$HeartDisease) #`pull` results in a vector
ConfusionMatrix

# ACCURACY (correct classification rate)
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)

caret::confusionMatrix(as.factor(pPred), as.factor(DB$HeartDisease))
