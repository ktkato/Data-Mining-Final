library(mosaic)
library(data.table)
library(tidyverse)
library(randomForest)
library(foreach)

options(scipen = 999)

mlbcontracts1yr <- read.csv("~/University of Texas/Data Mining & Statistical Inference/Final Project/mlbcontracts.csv")
mlbcontracts3yr <- read.csv("~/University of Texas/Data Mining & Statistical Inference/Final Project/mlbcontracts3yr.csv")


# Split up indices, factors, contract info, and framing from main data
contractinfo <- subset(mlbcontracts1yr, select = c(player,yr,pos,agecontract,aav))
allstats1yr <- subset(mlbcontracts1yr, select = -c(index,player,pos,agecontract,ageprioryr,contractyrs,guarantee,aav,team,framing,playerid))
allstats3yr <- subset(mlbcontracts3yr, select = -c(index,player,pos,agecontract,ageprioryr,contractyrs,guarantee,aav,team,framing,playerid))
hist(contractinfo$aav)
# Fill in "fld" with 0 (for Jason Giambi 2014, Travis Hafner 2013, Jim Thome 2010, Jim Thome 2011, Hideki Matsui 2010, Jim Thome 2010)
allstats1yr[is.na(allstats1yr)] <- 0

# Principal Component Analysis (1-Year)

pc_1yr <- prcomp(allstats1yr, rank = 20, scale. = TRUE)  # PCA on 1-year stats
load1 = pc_1yr$rotation  # Characterizes each component
scores1 = pc_1yr$x  # Characterizes each player

# Principal Component Analysis (3-Year)

pc_3yr <- prcomp(allstats3yr, rank = 15, scale. = TRUE)  # PCA on 3-year stats
load3 = pc_3yr$rotation  # Characterizes each component
scores3 = pc_3yr$x  # Characterizes each player

# PCA Summaries
summary(pc_1yr)   # Can explain 85% of the variation with 15 principal components, 90% with 20
summary(pc_3yr)   # Can explain 85% of the variation with 11 principal components, 90% with 15

# What describes PC1? (Using 3-year dataset, though it's largely the same in the 1-year)
# Typical MLB job-seeker: weak hitters, primary trade is defense, probably journeymen & utility players

  # Top 8 features
  head(load3[order(load3[,1], decreasing = TRUE),1],8)
  
  # Bottom 8 features
  head(load3[order(load3[,1]),1],8)
  

# New data frames

mlb1yr <- cbind(contractinfo, scores1)
mlb1yr$nameyr <- paste(mlb1yr$player, mlb1yr$yr)

mlb3yr <- cbind(contractinfo, scores3)
mlb3yr$nameyr <- paste(mlb3yr$player, mlb3yr$yr)


# Illustrating nonlinearity between talent and money

simplelm <- lm(aav ~ poly(PC1,2), data = mlb1yr)
quadpred <- predict(simplelm)

p <-
  ggplot(data = mlb1yr) +
    geom_point(mapping = aes(x = PC1, y = aav)) +
    xlab("Principal Component 1") +
    ylab("Average Annual Value") +
    geom_line(aes(x = PC1, y = quadpred)) +
    ggtitle("Figure 1. Mapping PC1 to AAV")
q <-
  ggplot(data = mlb1yr) +
    geom_point(mapping = aes(x = PC1, y = mlbcontracts1yr$war)) +
    xlab("Principal Component 1") +
    ylab("WAR") +
    ggtitle("Figure 2. Mapping PC1 to WAR")
  
gridExtra::grid.arrange(p,q,ncol=2)
  

###   Making predictions   ###

N = nrow(mlb1yr)

# split into a training and testing set
train_frac = 0.8
N_train = floor(train_frac*N)
N_test = N - N_train


# Create the function that will give us our table

rmse_df <- function(formulalm, formularf, splitnum, p, df) {
  
  fitlist <- list()
  
  for(i in 1:splitnum) {
  
    train_ind = sample.int(N, N_train, replace=FALSE) %>% sort
    mlbtrain = df[train_ind,]
    mlbtest = df[-train_ind,]
    
    # Linear Model!
    
    lm1_mlb <- lm(formulalm, data = mlbtrain)
    yhat_lm1_mlb <- predict(lm1_mlb, mlbtest)
    
    # Random Forests!
    
    forest1_mlb <- randomForest(formularf, data = mlbtrain, mtry = p, ntree = 500)
    yhat_forest1_mlb <- predict(forest1_mlb, mlbtest)
    
    fitlist[[i]] <- data.frame(rmse_lm = (mean((yhat_lm1_mlb - mlbtest$aav)^2) %>% sqrt),
                               rmse_rf = (mean((yhat_forest1_mlb - mlbtest$aav)^2) %>% sqrt))
  }  
  
  rbindlist(fitlist, idcol = T)
}
  
###   RMSE results   ###

# 1-year

table1yr <- rmse_df(formulalm =
                   aav ~ agecontract + poly(PC1,2) + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
                   PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20,
                 formularf =
                   aav ~ agecontract + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
                   PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20,
                 splitnum = 100,
                 p = 8,
                 df = mlb1yr)

# 3-year

table3yr <- rmse_df(formulalm =
                   aav ~ agecontract + poly(PC1,2) + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
                   PC11 + PC12 + PC13 + PC14 + PC15,
                 formularf =
                   aav ~ agecontract + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
                   PC11 + PC12 + PC13 + PC14 + PC15,
                 splitnum = 100,
                 p = 5,
                 df = mlb3yr)

# Who wins 1-year?
mean(table1yr$rmse_lm)  # Winner!
mean(table1yr$rmse_rf)

# Who wins 3-year?

mean(table3yr$rmse_lm)  # Winner!
mean(table3yr$rmse_rf)


# Sample Variance Importance Plot

set.seed(500)
train_index = sample.int(N, N_train, replace=FALSE) %>% sort
train_ex = mlb3yr[train_index,]
test_ex = mlb3yr[-train_index,]
forest_ex <- randomForest(aav ~ agecontract + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
                              PC11 + PC12 + PC13 + PC14 + PC15, data = train_ex, mtry = 5, ntree = 500)
yhat_forest_ex <- predict(forest_ex, test_ex)
varImpPlot(forest_ex)  # PC1 hugely important, which must be why accounting for its quadratic curvature improved the LM so much



# Appendix

load3[order(load3[,11], decreasing = TRUE),11] # probably hard-swinging uppercutters, inclined to hit HRs or strike out, but mostly strike out
loadings[order(loadings[,3], decreasing = TRUE),3] # seem to be players that were productive in limited playing time, possibly hurt, possibly a bit lucky
loadings[order(loadings[,4], decreasing = TRUE),4] # free-swingers who hit hard groundballs
loadings[order(loadings[,5], decreasing = TRUE),5] # old and slow, but they still know how to make contact - possibly situational hitters; "mature" contributors
loadings[order(loadings[,6], decreasing = TRUE),6] # non-elite regulars; patient at the plate, they walk, strike out, and hit groundballs
loadings[order(loadings[,7], decreasing = TRUE),7] # associated with a lot of soft groundballs and walks; not wholly unproductive hitters but nothing special
loadings[order(loadings[,8], decreasing = TRUE),8] # elite defense drives their value; appears to be more recent players
loadings[order(loadings[,9], decreasing = TRUE),9] # strong defense, high strikeouts, contributing hitters
loadings[order(loadings[,10], decreasing = TRUE),10] # no power but walks and launch angle help them get on base
loadings[order(loadings[,11], decreasing = TRUE),11]
loadings[order(loadings[,12], decreasing = TRUE),12]
loadings[order(loadings[,13], decreasing = TRUE),13]
loadings[order(loadings[,14], decreasing = TRUE),14]
loadings[order(loadings[,15], decreasing = TRUE),15]
loadings[order(loadings[,16], decreasing = TRUE),16]


