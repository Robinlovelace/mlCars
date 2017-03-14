library(dplyr)
#devtools::install_github("cran/glmnet")
library(glmnet)
library(sp)
library(ggplot2)
library(MASS)
#devtools::install_github("cran/plotmo")
library(gamlss)
# Model is tied to variable names used to fit it, so write custom predict function for convenience

# computeResidual = function(xobs, yobs){
#   ypred = MeanmodPredict(xobs)
#   return(yobs-ypred)
# }

traindf = readRDS("data/training_set.Rds")

# NOTE THAT WITHIN-SINGLE-MSOA COMMUTES ARE INCLUDED, AND FOR THESE THE DISTANCE IS ZERO. SO REMOVE THEM AS THEY ARE NOT MEANINGFUL
traindf@data = traindf@data %>% filter(distance > 0)
traindf@data = traindf@data %>% filter(npeople >= 10)

# Note the dataset is skewed towards car fractions of 1
# we cannot arbitrarily remove these as we would then fall prey to sampling bias:
#   https://medium.com/machine-intelligence-report/new-to-machine-learning-avoid-these-three-mistakes-73258b3848a4#.m8rwu44aw
# and we would also lose a lot of perfectly valid data
ggplot(traindf@data, aes(x=distance, y=car)) + geom_point(alpha = 0.05)
nrow(traindf@data %>% filter(car==1))
nrow(traindf@data %>% filter(car==0))
nrow(traindf@data %>% filter(car>0 & car<1))

# ONCE WE REQUIRE 10 PEOPLE PER FLOW THE DATA BECOMES ONE-INFLATED ONLY, WITH ONLY A HANDFUL OF POINTS AT ZERO, SO WE REMOVE THESE
traindf@data = traindf@data %>% filter(car > 0)

# THESE ARE ALL ATTEMPTS TO FIT THE CAR PROPENSITY AS A FUNCTION OF DISTANCE TO GET A BASELINE MODEL
# THE FIT IN ALL OF THEM IS PRETTY TERRIBLE

# REGULARISED (ELASTIC NET) POISSON FIT
x = traindf@data[,"distance"]
xmat = model.matrix(~distance, data=x)
yglmnet = as.matrix(traindf@data$car, ncol=1)
mod_glmnet = glmnet(x=xmat, y=yglmnet, family="poisson", alpha=0.5, intercept=F)
#plotmo::plot_glmnet(mod)
cvmod_glmnet = cv.glmnet(xmat, yglmnet, family="poisson", type.measure = "deviance", alpha=0.5, nfolds=10)
plot(cvmod_glmnet)
coef(cvmod_glmnet, s="lambda.min")
ypred = predict(mod_glmnet, xmat, s=cvmod_glmnet$lambda.min, type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
mean((obsvspred$Observed - obsvspred$Predicted)^2)
# THIS DROPS WITH DISTANCE, WHICH DOES NOT MAKE ANY SENSE

# NEGATIVE BINOMIAL FIT
mod_nb = glm.nb(car~distance, data=traindf@data, weights = npeople)
ypred = predict(mod_nb, as.data.frame(xmat), type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
mean((obsvspred$Observed - obsvspred$Predicted)^2)
# THIS ONE IS ALSO NON-SENSICAL (>1 AT POINTS)

# # LOGISTIC REGRESSION
# mod_logit = glm(car~distance, family=binomial(link="logit"), data=traindf@data)
# ypred = predict(mod_logit, as.data.frame(xmat), type="response")
# obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
# names(obsvspred) = c("Distance","Observed","Predicted")
# ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
# mean((obsvspred$Observed - obsvspred$Predicted)^2)

# LOGISTIC REGRESSION - weighted by population
mod_logit = glm(car~distance, family=binomial(link="logit"), data=traindf@data, weights = npeople)
ypred = predict(mod_logit, traindf@data, type="response")
obsvspred = cbind(traindf@data, Observed = traindf@data$car, Predicted = ypred)
ggplot(obsvspred, aes(x=distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) +
  geom_line(aes(y=Predicted), col="blue", size=1)
mean((obsvspred$Observed - obsvspred$Predicted)^2)
# LOGISTIC FUNCTION CANNOT GENERATE PROPORTION OF 1 FOR ANY FINITE INPUT, THEREFORE THIS MODEL DOES NOT MAKE SENSE

# LOGISTIC REGRESSION - weighted by population. Log of distance used. 
mod_log_logit = glm(car~log(distance), family=binomial(link="logit"),
                data=traindf@data[, c("car","distance", "npeople")], weights = npeople)
ypred = predict(mod_log_logit, traindf@data, type="response")
obsvspred = cbind(traindf@data, Observed = traindf@data$car, Predicted = ypred)
ggplot(obsvspred, aes(x=distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) +
  geom_line(aes(y=Predicted), col="blue", size=1)
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# ZERO/ONE INFLATED BETA REGRESSION # best from theoretical perspective
#?gamlss.family, BEINF=0 and 1 inflated, BEOI=1 inflated, BEZI=0 inflated beta regression
mod_infl = gamlss(car~distance, family=BEOI, data=traindf@data, weights = npeople)
ypred = predict(mod_infl, newdata=traindf@data[,"distance"], type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# FRACTIONAL LOGIT MODEL
# DIDN'T WORK BECAUSE THERE SEEMS TO BE NO PREDICT() FUNCTION FOR THIS MODEL
# library(frm)
# mod = frm(traindf@data$car, as.matrix(traindf@data[,"distance"]), linkbin="logit", linkfrac = "logit" , type="2P", inf=1)
# ypred = predict(mod, traindf@data[,"distance"], type="response")
# obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
# names(obsvspred) = c("Distance","Observed","Predicted")
# ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
# mean((obsvspred$Observed - obsvspred$Predicted)^2)


# SINCE WE THE RAW DATA IS QUITE HARD TO FIT WITH A SINGLE VARIABLE, LET'S TRY TO MODEL THE AVERAGE OF THAT VARIABLE INSTEAD
distbands = traindf@data
distbands$dband = cut(x = distbands$distance, 0:30, labels=as.character(0:29))
dband = distbands %>%
  group_by(dband) %>%
  filter(n() > 10) %>%
  summarise(pdrive = mean(car), npeople = sum(npeople))
dband$dband = as.numeric(dband$dband)
plot(dband$dband, dband$pdrive)

mod_mean = glm(pdrive~dband+log(dband), family=binomial(link="logit"), data=dband, weights = npeople)
ypred = predict(mod_mean, dband[,"dband"], type="response")
obsvspred = as.data.frame(cbind(dband$dband,dband$pdrive, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.5) + geom_line(aes(y=Predicted), col="blue", size=1)
#mean((obsvspred$Observed - obsvspred$Predicted)^2) # Not comparable to previous MSE as we are modelling the averages here

coef(mod_mean)
MeanmodPredict = function(x, mod){
  y = boot::inv.logit(coef(mod)[["(Intercept)"]] + coef(mod)[["dband"]]*x + coef(mod)[["log(dband)"]]*log(x))
  return(y)
}
# validation of function: it indeed returns what it should
predict(mod_mean, data.frame(dband=10), type="response")
MeanmodPredict(10, mod_mean)

# check how well it fits the actual data, not just the means: not bad, better than the others
ypred = MeanmodPredict(traindf@data$distance, mod_mean)
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# FOR REFERENCE, WHAT WOULD BE THE MSE OF A MODEL WHICH ALWAYS PREDICTS 1
mean((obsvspred$Observed - 1)^2)
# OR A MODEL WHICH ALWAYS PREDICTS THE MEAN
mean((obsvspred$Observed - mean(obsvspred$Observed))^2)

# randomly assign data points to a fold
k = 10
pointsperfold = floor(nrow(traindf@data)/k)
set.seed(5)
foldsidx = sample(rep(1:k, length.out=pointsperfold))
set.seed(NULL)
cvoutput = data.frame(model=character(0), MSE=numeric(0))

# we will compare three model by cross-validation: logistic with log(distance), 1-inflated beta, and log(distance) regression of mean
# logistic with log(distance)
kfolds_metric = c()
for(i in 1:k){
  testidx = which(foldsidx == i)
  trainset = traindf@data[-testidx,]
  testset = traindf@data[testidx,]
  # fit the model
  mod = glm(car~log(distance), family=binomial(link="logit"),
                      data=trainset, weights = npeople)
  #predict
  ypred = predict(mod, testset, type="response")
  #evaluate the metric
  mse = mean((testset$car-ypred)^2)
  kfolds_metric = c(kfolds_metric, mse)
}
cvoutput[nrow(cvoutput)+1,] = c("Log logit", round(mean(kfolds_metric), 4))

#one-inflated beta regression
kfolds_metric = c()
for(i in 1:k){
  testidx = which(foldsidx == i)
  trainset = traindf@data[-testidx,]
  testset = traindf@data[testidx,]
  # fit the model
  mod = gamlss(car~distance, family=BEOI, data=trainset, weights = npeople)
  #predict
  ypred = predict(mod, newdata=testset, type="response")
  #evaluate the metric
  mse = mean((testset$car-ypred)^2)
  kfolds_metric = c(kfolds_metric, mse)
}
cvoutput[nrow(cvoutput)+1,] = c("One-inflated Beta", round(mean(kfolds_metric), 4))

# logistic log(distance) fit on average distances in 1km distance bands
kfolds_metric = c()
for(i in 1:k){
  testidx = which(foldsidx == i)
  trainset = traindf@data[-testidx,]
  testset = traindf@data[testidx,]
  # fit the model
  trainset$dband = cut(x = trainset$distance, 0:30, labels=as.character(0:29))
  dband = trainset %>%
    group_by(dband) %>%
    filter(n() > 10) %>%
    summarise(pdrive = mean(car), npeople = sum(npeople))
  dband$dband = as.numeric(dband$dband)
  mod = glm(pdrive~dband+log(dband), family=binomial(link="logit"), data=dband, weights = npeople)
  #predict
  ypred = MeanmodPredict(testset$distance, mod)
  #evaluate the metric
  mse = mean((testset$car-ypred)^2)
  kfolds_metric = c(kfolds_metric, mse)
}
cvoutput[nrow(cvoutput)+1,] = c("Log logit of average", round(mean(kfolds_metric), 4))


cvoutput

# WE HAVE A CLEAR WINNER: THE MODEL FIT ON THE AVERAGES OF THE CAR PROPORTIONS

# WE WILL THEREFORE USE THIS MODEL TO SUBTRACT THE BASELINE DISTANCE-DEPENDENCE FROM THE RESPONSE VARIABLE (CAR PROPENSITY)

# WE HAVE NOW APPROXIMATELY SUBTRACTED OFF THE BASELINE DISTANCE-DEPENDENCE OF THE RESPONSE VARIABLE
# THESE RESIDUALS WILL NOW BE OUR RESPONSE FUNCTION
# INTERPRETATION OF RESPONSE FUNCTION: POSITIVE MEANS CAR USAGE HIGHER THAN EXPECTED FROM DISTANCE ALONE, NEGATIVE MEANS LOWER
traindf$response = traindf$car - MeanmodPredict(traindf@data$distance, mod_mean)

# IF THE SUBTRACTION OF DISTANCE HAS WORKED PROPERLY THE RESPONSE VARIABLE SHOULD NOW HAVE LITTLE TO NO DISTANCE-DEPENDENCE
#   I.E. BE FLAT AS A FUNCTION OF DISTANCE
# THIS IS INDEED APPROXIMATELY THE CASE
ggplot(traindf@data, aes(x=distance, y=response)) + geom_point(alpha=0.1)

saveRDS(traindf, "data/training_set_dist_subtracted.Rds")
readr::write_csv(traindf@data, "data/training_set_dist_subtracted.csv")
# save for use in future work
# saveRDS(traindf, "data/wyflows.Rds")

wyflows = readRDS("data/wyflows.Rds")
wyflows$response = wyflows$car - MeanmodPredict(wyflows@data$distance, mod_mean)
saveRDS(wyflows, "data/wyflows_w_response.Rds")

ggplot(wyflows@data, aes(x=distance, y=response)) + geom_point(alpha=0.02)





# out-takes
# re-run with 0s and 1s removed
# tdf_nozero = filter(traindf@data, car < 1, car > 0)
# dband = tdf_nozero %>%
#   group_by(dbands) %>%
#   filter(n() > 10) %>%
#   summarise(pdrive = mean(car), npeople = sum(npeople))
# dband$dbands = as.numeric(dband$dbands)
# mod = glm(pdrive~dbands+log(dbands), family=binomial(link="logit"), data=dband, weights = npeople)
# ypred = predict(mod, dband[,"dbands"], type="response")
# obsvspred = as.data.frame(cbind(dband$dbands,dband$pdrive, ypred))
# names(obsvspred) = c("Distance","Observed","Predicted")
# ypred = MeanmodPredict(tdf_nozero$distance)
# obsvspred = as.data.frame(cbind(tdf_nozero$distance,tdf_nozero$car, ypred))
# names(obsvspred) = c("Distance","Observed","Predicted")
# ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red", alpha=0.05) + geom_line(aes(y=Predicted), col="blue", size=1)
# # WE WILL THEREFORE USE THIS MODEL TO SUBTRACT THE BASELINE DISTANCE-DEPENDENCE FROM THE RESPONSE VARIABLE (CAR PROPENSITY)
# ypred_logit = data_frame(Distance = tdf_nozero$distance,  Predicted = predict(mod_logit, traindf@data, type="response"))
# ggplot(obsvspred, aes(x=Distance)) +
#   geom_point(aes(y=Observed), col="red", alpha=0.05) +
#   geom_line(aes(y=Predicted), col="blue", size=1) +
#   geom_point(data = ypred_logit, aes(y=Predicted), col="green")
# ypred_logit = data_frame(Distance = traindf$distance,  Predicted = predict(mod_mean, traindf@data, type="response"))
# ggplot(obsvspred, aes(x=Distance)) +
#   geom_point(aes(y=Observed), col="red", alpha=0.05) +
#   geom_line(aes(y=Predicted), col="blue", size=1) +
#   geom_point(data = ypred_logit, aes(y=Predicted), col="green")

