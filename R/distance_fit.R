library(dplyr)
#devtools::install_github("cran/glmnet")
library(glmnet)
library(sp)
library(ggplot2)
library(MASS)
#devtools::install_github("cran/plotmo")
library(gamlss)

traindf = readRDS("data/training_set.Rds")

#traindf@data
traindf$driveprop = traindf$`Driving a car or van`/traindf$`All categories: Method of travel to work`
traindf$distance = rgeos::gLength(traindf, byid=T)/1000

ggplot(traindf@data, aes(x=distance, y=driveprop)) + geom_point()


# THESE ARE ALL ATTEMPTS TO FIT THE CAR PROPENSITY AS A FUNCTION OF DISTANCE TO GET A BASELINE MODEL
# THE FIT IN ALL OF THEM IS PRETTY TERRIBLE

# REGULARISED (ELASTIC NET) POISSON FIT
x = traindf@data[,"distance"]
xmat = model.matrix(~distance, data=x)
yglmnet = as.matrix(traindf@data$driveprop, ncol=1)
mod = glmnet(x=xmat, y=yglmnet, family="poisson", alpha=0.5, intercept=F)
#plotmo::plot_glmnet(mod)
cvmod = cv.glmnet(xmat, yglmnet, family="poisson", type.measure = "deviance", alpha=0.5, nfolds=10)
plot(cvmod)
coef(cvmod, s="lambda.min")
ypred = predict(mod, xmat, s=cvmod$lambda.min, type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$driveprop, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")

# NEGATIVE BINOMIAL FIT
mod = glm.nb(driveprop~distance, data=traindf@data[,c("driveprop","distance")])
ypred = predict(mod, as.data.frame(xmat), type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$driveprop, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")

# LOGISTIC REGRESSION
mod = glm(driveprop~distance, family=binomial(link="logit"), data=traindf@data[,c("driveprop","distance")])
ypred = predict(mod, as.data.frame(xmat), type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$driveprop, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")

# ZERO ONE INFLATED BETA REGRESSION
mod = gamlss(driveprop~distance, family=BEINF, data=traindf@data[,c("driveprop","distance")])
ypred = predict(mod, newdata=traindf@data[,"distance"], type="response")
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$driveprop, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")



# SINCE WE CAN'T FIT THE RAW DATA, LET'S TRY TO MODEL THE AVERAGE INSTEAD
traindf$dbands = cut(x = traindf$distance, 0:60, labels=as.character(0:59))
dband = traindf@data %>% group_by(dbands) %>% filter(n() > 10) %>% summarise(pdrive = sum(`Driving a car or van`) / sum(`All categories: Method of travel to work`))
dband$dbands = as.numeric(dband$dbands)
plot(dband)

mod = glm(pdrive~dbands+log(dbands), family=binomial(link="logit"), data=dband)
ypred = predict(mod, dband[,"dbands"], type="response")

obsvspred = as.data.frame(cbind(dband$dbands,dband$pdrive, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")

# Model is tied to variable names used to fit it, so write custom predict function for convenience
coef(mod)
modPredict = function(x){
  y = boot::inv.logit(-0.5088964 - 0.0197722*x + 0.6721298*log(x))
  return(y)
}
# validation of function: it indeed returns what it should
#predict(mod, data.frame(dbands=10), type="response")
#modPredict(10)

# check how well it fits the actual data, not just the means: not bad, better than the others
ypred = modPredict(traindf@data$distance)
obsvspred = as.data.frame(cbind(traindf@data$distance,traindf@data$driveprop, ypred))
names(obsvspred) = c("Distance","Observed","Predicted")
ggplot(obsvspred, aes(x=Distance)) + geom_point(aes(y=Observed), col="red") + geom_point(aes(y=Predicted), col="blue")
# WE WILL THEREFORE USE THIS MODEL TO SUBTRACT THE BASELINE DISTANCE-DEPENDENCE FROM THE RESPONSE VARIABLE (CAR PROPENSITY)


computeResidual = function(xobs, yobs){
  ypred = modPredict(xobs)
  return(yobs-ypred)
}

# WE HAVE NOW APPROXIMATELY SUBTRACTED OFF THE BASELINE DISTANCE-DEPENDENCE OF THE RESPONSE VARIABLE
# THESE RESIDUALS WILL NOW BE OUR RESPONSE FUNCTION
# INTERPRETATION OF RESPONSE FUNCTION: POSITIVE MEANS CAR USAGE HIGHER THAN EXPECTED FROM DISTANCE ALONE, NEGATIVE MEANS LOWER
traindf@data$response = computeResidual(traindf@data$distance, traindf@data$driveprop)

# IF THE SUBTRACTION OF DISTANCE HAS WORKED PROPERLY THE RESPONSE VARIABLE SHOULD NOW HAVE LITTLE TO NO DISTANCE-DEPENDENCE
#   I.E. BE FLAT AS A FUNCTION OF DISTANCE
# THIS IS INDEED APPROXIMATELY THE CASE
ggplot(traindf@data, aes(x=distance, y=response)) + geom_point()
