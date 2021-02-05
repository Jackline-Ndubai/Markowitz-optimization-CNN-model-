rm(list=ls())
setwd("~/Downloads")
results<-read.csv("UTK_final_results(1).csv")
confused<-read.csv("UTK_most_confused.csv")

#install.packages(c("fPortfolio","quantmod","PerformanceAnalytics","PortfolioAnalytics","plotly"))

library(plotly)
library(caTools)
library(dplyr)
library(ggplot2)
library(reshape)

#reshape dataset for f1 score class by cycle matrix
    data<-subset(results, select = c(cycle,class,f1.score))
    reshaped<-reshape(data, 
                      timevar = "cycle",
                      idvar = "class",
                      direction = "wide")
    colnames(reshaped)<-c("class","cycle_0", "cycle_1","cycle_2","cycle_3","cycle_4","cycle_5","cycle_6","cycle_7")
    
    reshaped<-subset(reshaped, select = -c(class))
    cycle_matrix<-as.matrix(reshaped)
    cycle_matrix

# Calculate weighted f1 score for each class in each cycle
    results$weighted_f1<-results$f1.score*results$train_composition
    scores<-subset(results, select = c(cycle,class,weighted_f1))
    cycle_scores<-aggregate(scores$weighted_f1, by=list(cycle=scores$cycle), FUN=sum)
    cycle_scores<-cycle_scores[,-1]
    cycle_scores

# Calculate mean accuracy score for each class 
    scores1<-subset(results, select = c(cycle,class,f1.score))
    scores1<-aggregate(scores1$f1.score, by=list(class=scores1$class), FUN=mean)

    scores1<-scores1[,-1]
    scores1

#Calculate deviation of each individual class f1 score from mean
    deviation<-cycle_matrix-scores1
    deviation

#Calculating covariance
    deviation_t<-t(deviation)
    deviation_t

    cov<-deviation_t*deviation
    cov

#Create class composition matrix for each cycle
    comp<-subset(results, select = c(cycle,train_composition))
    W0<-comp[comp$cycle== 0, ]
    W0<-as.matrix(W0[,-1])
    W1<-comp[comp$cycle== 1, ]
    W1<-as.matrix(W1[,-1])
    W2<-comp[comp$cycle== 2, ]
    W2<-as.matrix(W2[,-1])
    W3<-comp[comp$cycle== 3, ]
    W3<-as.matrix(W3[,-1])
    W4<-comp[comp$cycle== 4, ]
    W4<-as.matrix(W4[,-1])
    W5<-comp[comp$cycle== 5, ]
    W5<-as.matrix(W5[,-1])
    W6<-comp[comp$cycle== 6, ]
    W6<-as.matrix(W6[,-1])
    W7<-comp[comp$cycle== 7, ]
    W7<-as.matrix(W7[,-1])

#Calculate risk for each cycle
    risk_cycle_0<-sqrt(abs(t(W0) %*% cov %*% W0))
    risk_cycle_1<-sqrt(abs(t(W1) %*% cov %*% W1))
    risk_cycle_2<-sqrt(abs(t(W2) %*% cov %*% W2))
    risk_cycle_3<-sqrt(abs(t(W3) %*% cov %*% W3))
    risk_cycle_4<-sqrt(abs(t(W4) %*% cov %*% W4))
    risk_cycle_5<-sqrt(abs(t(W5) %*% cov %*% W5))
    risk_cycle_6<-sqrt(abs(t(W6) %*% cov %*% W6))
    risk_cycle_7<-sqrt(abs(t(W7) %*% cov %*% W7))

#Plot effecient frontier
    risk<-c(risk_cycle_0,risk_cycle_1,risk_cycle_2,risk_cycle_3,risk_cycle_4,risk_cycle_5,risk_cycle_6,risk_cycle_7)
    cycle<-c("cycle_0", "cycle_1","cycle_2","cycle_3","cycle_4","cycle_5","cycle_6","cycle_7")
    eff.frontier <- data.frame(cycle,cycle_scores,risk) 
    eff.frontier$Sharperatio <- eff.frontier$cycle_scores / eff.frontier$risk
    eff.frontier

    feasible.sr <- cycle_scores / risk
    feasible.sr
    p <- plot_ly() %>%
      add_trace(x = risk, y = cycle_scores, text = cycle,color = feasible.sr, 
                mode = "text+markers", type = "scattergl", showlegend = F,
                
                marker = list(size = 3, opacity = 0.5, 
                              colorbar = list(title = "Sharpe Ratio"))) %>%
      add_trace(data = eff.frontier, x = ~risk, y = ~cycle_scores,mode = "text+markers", type = "scattergl")%>% 
      layout(title = "Efficient Frontier",
             yaxis = list(title = "Cycle Accuracy Score", tickformat = ".2%"),
             xaxis = list(title = "Cycle Accuracy risk", tickformat = ".2%"))
    p


