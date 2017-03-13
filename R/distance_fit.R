library(dplyr)
#devtools::install_github("cran/glmnet")
library(glmnet)
library(sp)
library(ggplot2)
library(MASS)
#devtools::install_github("cran/plotmo")
library(gamlss)
# Model is tied to variable names used to fit it, so write custom predict function for convenience
coef(mod)
modPredict = function(x){
  y = boot::inv.logit(-0.25047325 -0.03825625*x + 0.93772354*log(x))
  return(y)
}
computeResidual = function(xobs, yobs){
  ypred = modPredict(xobs)
  return(yobs-ypred)
}

# traindf = wyflows # if running on all data
traindf = readRDS("data/training_set.Rds")

#traindf@data
#traindf$distance = rgeos::gLength(traindf, byid=T)/1000

ggplot(traindf@data, aes(x=distance, y=car)) + geom_point(alpha = 0.02)


# THESE ARE ALL ATTEMPTS TO FIT THE CAR PROPENSITY AS A FUNCTION OF DISTANCE TO GET A BASELINE MODEL
# THE FIT IN ALL OF THEM IS PRETTY TERRIBLE

# REGULARISED (ELASTIC NET) POISSON FIT
x = traindf@data[,"distance"]
xmat = model.matrix(~distance, data=x)
yglmnet = as.matrix(traindf@data$car, ncol=1)
mod = glmnet(x=xmat, y=yglmnet, family="poisson", alpha=0.5, intercept=F)
#plotmo::plot_glmnet(mod)
cvmod = cv.glmnet(xmat, yglmnet, family="poisson", type.measure = "deviance", alpha=0.5, nfolds=10)
plot(cvmod)
coef(cvmod, s="lambda.min")
ypred = predict(mod, xmat, s=cvmod$lambda.min, type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")

# NEGATIVE BINOMIAL FIT
mod = glm.nb(car~distance, data=traindf@data[,c("car","distance")])
ypred = predict(mod, as.data.frame(xmat), type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")

# LOGISTIC REGRESSION
mod = glm(car~distance, family=binomial(link="logit"), data=traindf@data[,c("car","distance", "npeople")])
ypred = predict(mod, as.data.frame(xmat), type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# LOGISTIC REGRESSION - weighted by population
mod_logit = glm(car~distance, family=binomial(link="logit"),
                data=traindf@data[,c("car","distance", "npeople")], weights = npeople)
ypred = predict(mod_logit, traindf@data, type="response")
obsvspred = cbind(traindf@data, Observed = traindf@data$car, Predicted = ypred)
ggplot(obsvspred, aes(x=distance)) + geom_point(aes(y=Observed), col="red") +
  geom_point(aes(y=Predicted), col="blue")
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# LOGISTIC REGRESSION - weighted by population and log
mod_log_logit = glm(car~log(distance), family=binomial(link="logit"),
                data=traindf@data[,c("car","distance", "npeople")], weights = npeople)
ypred = predict(mod_log_logit, traindf@data, type="response")
obsvspred = cbind(traindf@data, Observed = traindf@data$car, Predicted = ypred)
ggplot(obsvspred, aes(x=distance)) + geom_point(aes(y=Observed), col="red") +
  geom_point(aes(y=Predicted), col="blue")
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# ZERO ONE INFLATED BETA REGRESSION # best from theoretical perspective
mod = gamlss(car~distance, family=BEINF, data=traindf@data[,c("car","distance","npeople")], weights = npeople)
ypred = predict(mod, newdata=traindf@data[,"distance"], type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# SINCE WE CAN'T FIT THE RAW DATA, LET'S TRY TO MODEL THE AVERAGE INSTEAD
traindf$dbands = cut(x = traindf$distance, 0:60, labels=as.character(0:59))
dband = traindf@data %>%
  group_by(dbands) %>%
  filter(n() > 10) %>%
  summarise(pdrive = mean(car), npeople = sum(npeople))
dband$dbands = as.numeric(dband$dbands)
plot(dband[1:2])

mod = glm(pdrive~log(dbands), family=binomial(link="logit"), data=dband, weights = npeople)
ypred = predict(mod, dband[,"dbands"], type="response")
obsvspred = as.data.frame(cbind(dband$dbands,dband$pdrive, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")
mean((obsvspred$Observed - obsvspred$Predicted)^2)

# validation of function: it indeed returns what it should
#predict(mod, data.frame(dbands=10), type="response")
#modPredict(10)

# check how well it fits the actual data, not just the means: not bad, better than the others
ypred = modPredict(traindf@data$distance)
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$car, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")
# WE WILL THEREFORE USE THIS MODEL TO SUBTRACT THE BASELINE DISTANCE-DEPENDENCE FROM THE RESPONSE VARIABLE (CAR PROPENSITY)
ypred_logit = data_frame(Distance = traindf$distance,  Predicted = predict(mod_logit, traindf@data, type="response"))
ggplot(obsvspred, aes(x=Distance)) +
  geom_point(aes(y=Observed), col="red") +
  geom_point(aes(y=Predicted), col="blue") +
  geom_point(data = ypred_logit, aes(y=Predicted), col="green")

# WE HAVE NOW APPROXIMATELY SUBTRACTED OFF THE BASELINE DISTANCE-DEPENDENCE OF THE RESPONSE VARIABLE
# THESE RESIDUALS WILL NOW BE OUR RESPONSE FUNCTION
# INTERPRETATION OF RESPONSE FUNCTION: POSITIVE MEANS CAR USAGE HIGHER THAN EXPECTED FROM DISTANCE ALONE, NEGATIVE MEANS LOWER
traindf$response = traindf$car - predict(mod_log_logit, traindf@data, type="response")

# IF THE SUBTRACTION OF DISTANCE HAS WORKED PROPERLY THE RESPONSE VARIABLE SHOULD NOW HAVE LITTLE TO NO DISTANCE-DEPENDENCE
#   I.E. BE FLAT AS A FUNCTION OF DISTANCE
# THIS IS INDEED APPROXIMATELY THE CASE
ggplot(traindf@data, aes(x=distance, y=response)) + geom_point()

saveRDS(traindf, "data/training_set_dist_subtracted.Rds")
readr::write_csv(traindf@data, "data/training_set_dist_subtracted.csv")
# save for use in future work
# saveRDS(traindf, "data/wyflows.Rds")

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
# ypred = modPredict(tdf_nozero$distance)
# obsvspred = as.data.frame(cbind(tdf_nozero$distance,tdf_nozero$car, ypred))
# names(obsvspred) = c("Distance","Observed","Predicted")
# ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")
# # WE WILL THEREFORE USE THIS MODEL TO SUBTRACT THE BASELINE DISTANCE-DEPENDENCE FROM THE RESPONSE VARIABLE (CAR PROPENSITY)
# ypred_logit = data_frame(Distance = tdf_nozero$distance,  Predicted = predict(mod_logit, traindf@data, type="response"))
# ggplot(obsvspred, aes(x=Distance)) +
#   geom_point(aes(y=Observed), col="red") +
#   geom_point(aes(y=Predicted), col="blue") +
#   geom_point(data = ypred_logit, aes(y=Predicted), col="green")
