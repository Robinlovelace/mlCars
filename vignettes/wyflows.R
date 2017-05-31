wyflows<- readRDS("../data/wyflows.Rds")
str(wyflows)
ls()
df.wyflows <- as.data.frame(wyflows)

head(df.wyflows)

write.csv(df.wyflows,"flows.csv")
# write.dta(df.wyflows,"flows.dta")

names(df.wyflows)

names(df.wyflows)[13] <- "q1624"
names(df.wyflows)[14] <- "q2534"
names(df.wyflows)[15] <- "q3549"
names(df.wyflows)[16] <- "q5064"
names(df.wyflows)[17] <- "q6574"
names(df.wyflows)[18] <- "q75plus"

# association with age. cut off at 25
#
df.wyflows$numYoung <- df.wyflows$q1624 
df.wyflows$numOld <- df.wyflows$q2534 + df.wyflows$q3549 + df.wyflows$q5064 + df.wyflows$q6574 + df.wyflows$q75plus


#cut off at 35
#df.wyflows$numYoung <- df.wyflows$q1624 + df.wyflows$q2534 
#df.wyflows$numOld <- df.wyflows$q3549 + df.wyflows$q5064 + df.wyflows$q6574 + df.wyflows$q75plus

# cut off at 49
#df.wyflows$numYoung <- df.wyflows$q1624 + df.wyflows$q2534 + df.wyflows$q3549
#df.wyflows$numOld <- df.wyflows$q5064+ df.wyflows$q6574+ df.wyflows$q75plus

# cut off at 65
#df.wyflows$numYoung <- df.wyflows$q1624 + df.wyflows$q2534 + df.wyflows$q3549 + df.wyflows$q5064
#df.wyflows$numOld <- df.wyflows$q6574+ df.wyflows$q75plus

#cut off at 75
#df.wyflows$numYoung <- df.wyflows$q1624 + df.wyflows$q2534 + df.wyflows$q3549 + df.wyflows$q5064 + df.wyflows$q6574
#df.wyflows$numOld <- df.wyflows$q75plus


df.wyflows$OldYoung <- df.wyflows$numOld + df.wyflows$numYoung

m0 <- lm(car ~ numYoung + OldYoung , data=df.wyflows)
summary(m0)

m1 <- lm(car ~ numOld + OldYoung , data=df.wyflows)
summary(m1)

m2 <- lm(car ~ numYoung + numOld, data=df.wyflows)
summary(m2)


m0 <- lm(car ~ q1624, data=df.wyflows)
summary(m0)

m1 <- lm(car ~ q1624+q2534+q3549+q6574+q75plus, data=df.wyflows)
summary(m1)


#ethnicity
df.wyflows$nonwhite <- df.wyflows$mixed + df.wyflows$asian + df.wyflows$black + df.wyflows$otherethn
df.wyflows$allEth <- df.wyflows$nonwhite + df.wyflows$white

m0 <- lm(car ~ white + allEth, data=df.wyflows)
summary(m0)

m1 <- lm(car ~ nonwhite + allEth, data=df.wyflows)
summary(m1)

m2 <- lm(car ~ white + nonwhite, data=df.wyflows)
summary(m2)

# male/female

mGender <- lm(car ~ male + female, data=df.wyflows)
summary(mGender)


#Number of cars in household
df.wyflows$havecar <- df.wyflows$housesw1car + df.wyflows$housesw2car + df.wyflows$housesw3car + df.wyflows$housesw4ormorecar
df.wyflows$allcars <- df.wyflows$housesw0car + df.wyflows$havecar

mCars00 <- lm(car ~ housesw0car + allcars + q1624 + q2534 + q3549 + q5064 + q6574 + q75plus, data=df.wyflows)
summary(mCars00)
 
mCars01 <- lm(car ~ havecar + allcars + q1624 + q2534 + q3549 + q5064 + q6574 + q75plus, data=df.wyflows)
summary(mCars01)

mCars02 <- lm(car ~ housesw0car + havecar + q1624 + q2534 + q3549 + q5064 + q6574 + q75plus, data=df.wyflows)
summary(mCars02)

mCars03 <- lm(car ~ housesw0car + havecar + I(housesw0car^2) + I(havecar^2) + q1624 + q2534 + q3549 + q5064 + q6574 + q75plus, data=df.wyflows)
summary(mCars03)

#Number of cars in household - cut off at 1
df.wyflows$have2pluscars <- df.wyflows$housesw2car + df.wyflows$housesw3car + df.wyflows$housesw4ormorecar
df.wyflows$have0or1cars <- df.wyflows$housesw0car + df.wyflows$housesw1car

df.wyflows$allcars <- df.wyflows$have2pluscars + df.wyflows$have0or1cars


mCars10 <- lm(car ~ have2pluscars + allcars , data=df.wyflows)
summary(mCars10)
#nothing found
 
mCars11 <- lm(car ~ have0or1cars + allcars , data=df.wyflows)
summary(mCars11)

mCars12 <- lm(car ~ housesw0car + havecar, data=df.wyflows)
summary(mCars12)

mCars13 <- lm(car ~ housesw0car + havecar + I(housesw0car^2) + I(havecar^2), data=df.wyflows)
summary(mCars13)

# health
# 
mHealth0 <- lm(car ~ vghealth + ghealth + fhealth + bhealth + vbhealth + q1624 + q2534 + q3549 + q5064 + q6574 + q75plus, data=df.wyflows)
summary(mHealth0)
#nothing found

# economic activity
# This doesn't appear in the DAG ! Arguably part of affluence ?
mEcon <- lm(car ~ econactiv + econinactiv , data=df.wyflows)
summary(mEcon )
#nothing found

#Qualifications
# This doesn't appear in the DAG ! Arguably part of affluence ?
mEcon <- lm(car ~ noqual + aptshpqual + lev1qual + lev2qual + lev3qual + lev4qual + otherqual, data=df.wyflows)
summary(mEcon)
#nothing found

#Students
# This doesn't appear in the DAG !
mStudents <- lm(car ~ schoolstudents + unistudents, data=df.wyflows)
summary(mStudents)
#nothing found

#centheat
# This doesn't appear in the DAG !
mCentheat <- lm(car ~ centheat, data=df.wyflows)
summary(mCentheat)
#nothing found

#nrooms
# This doesn't appear in the DAG ! Arguably part of affluence ?
mRooms <- lm(car ~ nrooms + white, data=df.wyflows)
summary(mRooms)
#nothing found


