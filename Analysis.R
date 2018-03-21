set.seed(349)
library(dplyr)
library(tm)
library(topicmodels)
library(qdap)

setwd("/Users/marisolromero/OneDrive/Northwestern/2016-2017/Q3/EECS 349-0/Project/Data")

##### read in and format data #####

train <- read.csv(file = "train.csv", header = TRUE, stringsAsFactors = FALSE)
val   <- read.csv(file = "validation.csv", header = TRUE, stringsAsFactors = FALSE)

nutritioncols <- train %>%
  select(ALA_G:ZN) %>%
  colnames

train[,nutritioncols][is.na(train[,nutritioncols])] <- 0
val[,nutritioncols][is.na(val[,nutritioncols])] <- 0

ingredientcols <- train %>%
  select(acorn.squash.seeds.scooped.out:zucchinis) %>%
  colnames()

nutritioncols <- train %>%
  select(ALA_G:ZN) %>%
  colnames

##### dimension reduction #####

ingredient.matrix <- train %>%
  select(acorn.squash.seeds.scooped.out:zucchinis)
ingredient.frequency.matrix <- as.wfm(t(ingredient.matrix))
document.ingredient.matrix <- as.dtm(ingredient.frequency.matrix)

val.ingredient.matrix <- val %>%
  select(acorn.squash.seeds.scooped.out:zucchinis)
val.ingredient.frequency.matrix <- as.wfm(t(val.ingredient.matrix))
val.document.ingredient.matrix <- as.dtm(val.ingredient.frequency.matrix)

# this code is commented out - it was used to select 50 as the number of latent topics in LDA
# lda.trials     <- lapply(as.list(seq(10, 150, by = 20)), function(x) LDA(document.ingredient.matrix, x))
# lda.posteriors <- lapply(lda.trials, posterior)
# lda.dataset    <- lapply(lda.posteriors, function(x) data.frame(social_rank = train$social_rank, topic = x$topics[,1:(ncol(x$topics) -1)]))
# lda.lm         <- lapply(lda.dataset, function(x) lm(social_rank ~., data = x))
# lda.rmse       <- lapply(lda.lm, function(x) sqrt(mean(x$residuals^2)))
# lda.rmse       <- unlist(lda.rmse)
# 
# lda.val.posterior <- lapply(lda.trials, function(x) posterior(x, val.document.ingredient.matrix))
# lda.val.dataset   <- lapply(lda.val.posterior, function(x) data.frame(social_rank = val$social_rank, topic = x$topics[,1:(ncol(x$topics) -1)]))
# lda.rmspe         <- lapply(as.list(1:length(lda.trials)), function(x) sqrt(mean((predict(lda.lm[[x]], lda.val.dataset[[x]]) - val$social_rank)^2)))
# lda.rmspe         <- unlist(lda.rmspe)
# rmspe obtained
# 10       30       50       70       90       110      130      150
# 26.59381 25.66276 25.53604 25.92471 26.39056 25.12315 25.78366 25.96874

# reducing ingredients with Latent Dirichlet Allocation
lda               <- LDA(document.ingredient.matrix, 50)
lda.posterior     <- posterior(lda)
lda.val.posterior <- posterior(lda, val.document.ingredient.matrix)
lda.dataset       <- data.frame(social_rank = train$social_rank, topic = lda.posterior$topics[,1:(ncol(lda.posterior$topics) - 1)])
lda.val.dataset   <- data.frame(social_rank = val$social_rank,   topic = lda.val.posterior$topics[,1:(ncol(lda.posterior$topics) - 1)])

lda.train.dataset.towrite <- train %>%
  select(social_rank, flavors.piquant, flavors.sour, flavors.salty, flavors.sweet, flavors.bitter, flavors.meaty,
         totalTimeInSeconds, numberOfServings, publisher) %>%
  data.frame(topic = lda.posterior$topics)

lda.validation.dataset.towrite <- val %>%
  select(social_rank, flavors.piquant, flavors.sour, flavors.salty, flavors.sweet, flavors.bitter, flavors.meaty,
         totalTimeInSeconds, numberOfServings, publisher) %>%
  data.frame(topic = lda.val.posterior$topics)

write.csv(lda.train.dataset.towrite, file = "lda.train.dataset.csv", row.names = FALSE)
write.csv(lda.validation.dataset.towrite, file = "lda.validation.dataset.csv", row.names = FALSE)

# reducing ingredients with Principal Component Analysis
pca          <- prcomp(ingredient.matrix)
pca.vals     <- pca$x[,1:200]
pca.data     <- data.frame(social_rank = train$social_rank, pca.vals)
pca.val.data <- as.data.frame(predict(pca, val.ingredient.matrix))[,1:200]

# reducing nutrition variables with Principal Component Analysis
pca2          <- prcomp(train[,nutritioncols])
pca2.vals     <- pca2$x[,1:2]
pca2.data     <- data.frame(social_rank = train$social_rank, pca2.vals)
pca2.val.data <- as.data.frame(predict(pca2, val[,nutritioncols]))[,1:2]

lda.pca.train.dataset.towrite <- data.frame(lda.train.dataset.towrite, pca2.vals)
lda.pca.validation.dataset.towrite <- data.frame(lda.validation.dataset.towrite, as.data.frame(predict(pca2, val[,nutritioncols]))[,1:2])
write.csv(lda.pca.train.dataset.towrite, file = "lda.pca.train.dataset.csv", row.names = FALSE)
write.csv(lda.pca.validation.dataset.towrite, file = "lda.pca.validation.dataset.csv", row.names = FALSE)
 
##### simple linear regression models #####

### using only publisher ###

pub.lm <- lm(social_rank ~ publisher, data = train)
pub.rmse <- sqrt(mean(pub.lm$residuals^2))
pub.lm$xlevels[["publisher"]] <- union(pub.lm$xlevels[["publisher"]], levels(as.factor(val$publisher)))
pub.rmspe <- sqrt(mean((predict(pub.lm, val) - val$social_rank)^2))

### using only flavor scores ###

flavors.lm <- lm(social_rank ~ flavors.piquant + flavors.sour + flavors.salty +
                   flavors.sweet + flavors.bitter + flavors.meaty,
                 data = train)
rmse.flavors <- sqrt(mean(flavors.lm$residuals^2))
rmspe.flavors <- sqrt(mean((predict(flavors.lm, val) - val$social_rank)^2, na.rm = TRUE))

### using only ingredients ###

ingredients.lm <- lm(as.formula(paste0("social_rank ~ ", paste(ingredientcols, collapse = "+"))),
                     data = train)
head(summary(ingredients.lm)$coefficients[unlist(sort(summary(ingredients.lm)$coefficients[,4], index.return = TRUE)),1], 100)
rmse.ingredients <- sqrt(mean(ingredients.lm$residuals^2))
rmspe.ingredients <- sqrt(mean((predict(ingredients.lm, val) - val$social_rank)^2))

## using ingredients reduced with LDA ##

lda.lm    <- lm(social_rank ~ ., data = lda.dataset)
lda.rmse  <- sqrt(mean(lda.lm$residuals^2))
lda.rmspe <- sqrt(mean((predict(lda.lm, lda.val.dataset) - val$social_rank)^2))

## using ingredients reduced with PCA ##

pca.lm    <- lm(social_rank ~ ., data = pca.data)
pca.rmse  <- sqrt(mean(pca.lm$residuals^2))
pca.rmspe <- sqrt(mean((predict(pca.lm, pca.val.data) - val$social_rank)^2))

## using nutrition variables ##

nutrition.lm <- lm(as.formula(paste0("social_rank ~ ", paste(nutritioncols, collapse = "+"))), data = train)
nutrition.rmse  <- sqrt(mean(nutrition.lm$residuals^2))
nutrition.rmspe <- sqrt(mean((predict(nutrition.lm, val) - val$social_rank)^2))

## using nutrition variables reduced with PCA ##

pca2.lm       <- lm(social_rank ~ ., data = pca2.data)
pca2.rmse     <- sqrt(mean(pca2.lm$residuals^2))
pca2.rmspe    <- sqrt(mean((predict(pca2.lm, pca2.val.data) - val$social_rank)^2))

pairs(select(data.frame(train)))

other.vars.lm <- lm(social_rank ~ totalTimeInSeconds + numberOfServings + publisher, data = train)
other.vars.lm$xlevels[["publisher"]] <- union(other.vars.lm$xlevels[["publisher"]], levels(as.factor(val$publisher)))
other.vars.rmse <- sqrt(mean(other.vars.lm$residuals^2))
other.vars.rmspe <- sqrt(mean((predict(other.vars.lm, val) - val$social_rank)^2))

other.vars.flavors.lm <- lm(social_rank ~ totalTimeInSeconds + numberOfServings + publisher + flavors.piquant + 
                              flavors.sour + flavors.salty + flavors.sweet + flavors.bitter + flavors.meaty,
                            data = train)
other.vars.flavors.lm$xlevels[["publisher"]] <- union(other.vars.flavors.lm$xlevels[["publisher"]], levels(as.factor(val$publisher)))
other.vars.flavors.rmse <- sqrt(mean(other.vars.flavors.lm$residuals^2))
other.vars.flavors.rmspe <- sqrt(mean((predict(other.vars.flavors.lm, val) - val$social_rank)^2, na.rm = TRUE))

other.vars.lda.dataset <- train %>%
  select(social_rank, totalTimeInSeconds, numberOfServings, publisher) %>%
  data.frame(topic = lda.posterior$topics[,1:(ncol(lda.posterior$topics) - 1)])
other.vars.lda.val.dataset <- val %>%
  select(social_rank, totalTimeInSeconds, numberOfServings, publisher) %>%
  data.frame(topic = lda.val.posterior$topics[,1:(ncol(lda.posterior$topics) - 1)])

other.vars.lda.lm <- lm(social_rank ~ ., other.vars.lda.dataset)
other.vars.lda.lm$xlevels[["publisher"]] <- union(other.vars.lda.lm$xlevels[["publisher"]], levels(as.factor(val$publisher)))
other.vars.lda.rmse <- sqrt(mean(other.vars.lda.lm$residuals^2))
other.vars.lda.rmspe <- sqrt(mean((predict(other.vars.lda.lm, other.vars.lda.val.dataset) - val$social_rank)^2))

other.vars.lda.pca.dataset <- data.frame(other.vars.lda.dataset, pca2.vals)
other.vars.lda.pca.val.dataset <- data.frame(other.vars.lda.val.dataset, pca2.val.data)

other.vars.lda.pca.lm <- lm(social_rank ~ ., other.vars.lda.pca.dataset)
other.vars.lda.pca.lm$xlevels[["publisher"]] <- union(other.vars.lda.pca.lm$xlevels[["publisher"]], levels(as.factor(val$publisher)))
other.vars.lda.pca.rmse <- sqrt(mean(other.vars.lda.pca.lm$residuals^2))
other.vars.lda.pca.rmspe <- sqrt(mean((predict(other.vars.lda.pca.lm, other.vars.lda.pca.val.dataset) - val$social_rank)^2))

rmse.flavors
rmse.ingredients
  lda.rmse
  pca.rmse
nutrition.rmse
  pca2.rmse
pub.rmspe
other.vars.rmse
other.vars.flavors.rmse
other.vars.lda.rmse
other.vars.lda.pca.rmse

rmspe.flavors
rmspe.ingredients
  lda.rmspe
  pca.rmspe
nutrition.rmspe
  pca2.rmspe
pub.rmspe
other.vars.rmspe
other.vars.flavors.rmspe
other.vars.lda.rmspe
other.vars.lda.pca.rmspe

## saving topics and terms from LDA ##
write.csv(lda.posterior$terms, file = "terms.csv", row.names = FALSE)
write.csv(lda.posterior$topics, file = "topics.csv", row.names = FALSE)

##### test dataset formatting #####
test <- read.csv(file = "test.csv", header = TRUE, stringsAsFactors = FALSE)
test[,nutritioncols][is.na(test[,nutritioncols])] <- 0

test.ingredient.matrix <- test %>%
  select(acorn.squash.seeds.scooped.out:zucchinis)
test.ingredient.frequency.matrix <- as.wfm(t(test.ingredient.matrix))
test.document.ingredient.matrix <- as.dtm(test.ingredient.frequency.matrix)

lda.test.posterior <- posterior(lda, test.document.ingredient.matrix)
lda.test.dataset   <- data.frame(social_rank = test$social_rank,   topic = lda.test.posterior$topics[,1:(ncol(lda.test.posterior$topics) - 1)])

lda.test.dataset.towrite <- test %>%
  select(social_rank, flavors.piquant, flavors.sour, flavors.salty, flavors.sweet, flavors.bitter, flavors.meaty,
         totalTimeInSeconds, numberOfServings, publisher) %>%
  data.frame(topic = lda.test.posterior$topics)

lda.pca.test.dataset.towrite <- data.frame(lda.test.dataset.towrite, as.data.frame(predict(pca2, test[,nutritioncols]))[,1:2])
write.csv(lda.pca.test.dataset.towrite, file = "lda.pca.test.dataset.csv", row.names = FALSE)

##### test final model and obtain RMSE #####

final.dataset <- lda.pca.test.dataset.towrite %>%
  select(-c(flavors.piquant, flavors.sour, flavors.salty, flavors.sweet, flavors.bitter, flavors.meaty, topic.50))

final.rmste <- sqrt(mean((predict(other.vars.lda.pca.lm, final.dataset) - test$social_rank)^2))

test.and.prediction <- data.frame(test, prediction = predict(other.vars.lda.pca.lm, final.dataset))

predicted.vs.actual <- select(test.and.prediction, title, id, prediction, social_rank)
write.csv(predicted.vs.actual, file = "test_predictions_linreg.csv", row.names = FALSE)

library(ggplot2)
library(reshape2)

##### compare results from regression and Weka models #####

results.df <- data.frame(technique = c("Linear Regression", "KNN-35", "Gaussian Process", "MLP", "Random Forest"),
                         validation.error = c(other.vars.lda.pca.rmspe, 11.96, 12.05, 15.68, 14.43),
                         test.error = c(final.rmste, 14.76, 14.79, 16.9, 16.78))

test.df.melt <- melt(results.df)

ggplot(test.df.melt, aes(x = technique, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Validation and Test RMSE") +
  xlab("Technique") + ylab("RMSE")+
  coord_cartesian(ylim=c(10,17))

##### plot stepwise regression results #####

regression.results <- data.frame(covariates = c("Base model", "Base model +\nFlavor Score", "Base model +\n LDA ingredients", "Base model +\n LDA ingredients+\nPCA nutrition"),
                                 validation.error = c(other.vars.rmspe, other.vars.flavors.rmspe, other.vars.lda.rmspe, other.vars.lda.pca.rmspe))
regression.results$covariates <- factor(regression.results$covariates, levels = c("Base model", "Base model +\nFlavor Score", "Base model +\n LDA ingredients", "Base model +\n LDA ingredients+\nPCA nutrition"),
                                           ordered = TRUE)
ggplot(regression.results, aes(x = covariates, y = validation.error))+geom_bar(stat = "identity")+
  coord_cartesian(ylim=c(12,13.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("RMSE") + xlab("Variables Inluded in Model")+
  ggtitle("Linear Regression Validaton Accuracy")
