# LR assignment - Sangram Jethy

## Loading required library

library(stringr)
library(MASS)
library("car")

## Loading the data set
carPrice <- read.csv("CarPrice_Assignment.csv")
#View(carPrice)
str(carPrice)

#############################################################################
## Data Preparation

## Spliting CarName to company name and model name and picking only company name
## toLower is used to convert all name to a common case example Nissan and nissan are same
carPrice$CarName <- str_split_fixed(carPrice$CarName, pattern = " ", n = 2)[,1] 
carPrice$CarName <- str_split_fixed(carPrice$CarName, pattern = "-", n = 2)[,1] 

## correcting spelling of company name
carPrice$CarName[carPrice$CarName == "Nissan"] <- "nissan"
carPrice$CarName[carPrice$CarName == "toyouta"] <- "toyota"
carPrice$CarName[carPrice$CarName == "vokswagen"] <- "volkswagen"
carPrice$CarName[carPrice$CarName == "vw"] <- "volkswagen"
carPrice$CarName[carPrice$CarName == "maxda"] <- "mazda"

str(carPrice$fueltype)
summary(factor(carPrice$fueltype))
View(carPrice)



## Convering categorical variable fueltype having 2 values to numeric dummy
levels(carPrice$fueltype)<-c(1,0)
carPrice$fueltype<- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]
summary(carPrice$fueltype)

## Convering categorical variable aspiration 2 values to numeric dummy
levels(carPrice$aspiration)<-c(1,0)
carPrice$aspiration<- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

## Convering categorical variable doornumber 2 values to numeric dummy
levels(carPrice$doornumber)<-c(1,0)
carPrice$doornumber<- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

## Convering categorical variable enginelocation 2 values to numeric dummy
levels(carPrice$enginelocation)<-c(1,0)
carPrice$enginelocation<- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

## Convering categorical variable carbody to  dummy
dummy_1 <- data.frame(model.matrix( ~carbody, data = carPrice))
#View(dummy_1)
dummy_1 <- dummy_1[,-1]
carPrice_1 <- cbind(carPrice[,-7], dummy_1)
#View(carPrice_1)

## Convering categorical variable drivewheel to  dummy
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = carPrice))
#View(dummy_2)
dummy_2 <- dummy_2[,-1]
carPrice_2 <- cbind(carPrice_1[,-7], dummy_2)
#View(carPrice_2)

## Convering categorical variable enginetype to  dummy
dummy_3 <- data.frame(model.matrix( ~enginetype, data = carPrice))
#View(dummy_3)
dummy_3 <- dummy_3[,-1]
carPrice_3 <- cbind(carPrice_2[,-13], dummy_3)
#View(carPrice_3)

## Convering categorical variable cylindernumber to  dummy
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = carPrice))
#View(dummy_4)
dummy_4 <- dummy_4[,-1]
carPrice_4 <- cbind(carPrice_3[,-13], dummy_4)
#View(carPrice_4)

## Convering categorical variable fuelsystem to  dummy
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = carPrice))
#View(dummy_5)
dummy_5 <- dummy_5[,-1]
carPrice_5 <- cbind(carPrice_4[,-14], dummy_5)
#View(carPrice_5)

## Convering categorical variable CarName to  dummy
dummy_6 <- data.frame(model.matrix( ~CarName, data = carPrice))
#View(dummy_6)
dummy_6 <- dummy_6[,-1]
carPrice_6 <- cbind(carPrice_5[,-3], dummy_6)
View(carPrice_6)

#############################################################################
## Preparing training and testing sets

set.seed(100)
trainindices= sample(1:nrow(carPrice_6), 0.7*nrow(carPrice_6))
train = carPrice_6[trainindices,]
test = carPrice_6[-trainindices,]
#View(train)


#############################################################################
## Model building

model_1 <-lm(price~.,data=train)
summary(model_1)
corrs = cor(carPrice_6)
#View(corrs)

#############################################################################

step <- stepAIC(model_1, direction="both")


# removing boreratio,carbodyhardtop,carbodyhatchback,carbodysedan,carbodywagon,carheight,carlength,
# CarNamebuick,CarNamejaguar,compressionratio,cylindernumberfour,cylindernumbersix,doornumber,
# drivewheelfwd,enginetypeohc,enginetypeohcv,fuelsystemidi,fuelsystemmfi,fuelsystemspdi,fueltype,
# highwaympg,horsepower,symboling,wheelbase

model_2 <- lm(formula = price ~ aspiration + car_ID + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNameporcshce + CarNamerenault + CarNamesaab +
                CarNametoyota  + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight + cylindernumberfive + cylindernumberthree +
                drivewheelrwd + enginelocation + enginesize + enginetypedohcv + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_2)
vif(model_2)


# removing car_ID due to high VIF

model_3 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu  + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNameporcshce + CarNamerenault + CarNamesaab +
                CarNametoyota  + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight + cylindernumberfive + cylindernumberthree +
                drivewheelrwd + enginelocation + enginesize + enginetypedohcv + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_3)
vif(model_3)


# removing enginetypedohcv due to high p value
# All rest variables with high VIF have significant p-value

model_4 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNameporcshce + CarNamerenault + CarNamesaab +
                CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight + cylindernumberfive + cylindernumberthree +
                drivewheelrwd + enginelocation + enginesize  + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_4)
vif(model_4)


# removing drivewheelrwd due to high p value
# All rest variables with high VIF have significant p-value

model_5 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNameporcshce + CarNamerenault + CarNamesaab +
                CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight + cylindernumberfive + cylindernumberthree +
                enginelocation + enginesize  + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_5)
vif(model_5)

# removing CarNameporcshce due to low p value

model_6 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu  + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab +
                CarNametoyota  + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight + cylindernumberfive + cylindernumberthree +
                enginelocation + enginesize  + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_6)
vif(model_6)

#removing cylindernumberfive due to low p value

model_7 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab +
                CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight + cylindernumberthree +
                enginelocation + enginesize  + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_7)
vif(model_7)

# removing cylindernumberthree due to low p value

model_8 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab +
                CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                carwidth + citympg + curbweight  +
                enginelocation + enginesize  + enginetypel + enginetypeohcf +
                enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
              data = train)

summary(model_8)
vif(model_8)




# removing curbweight due to high VIF and less P value

model_9 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                 CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab +
                 CarNametoyota  + CarNamevolvo +
                 carwidth + citympg  +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
               data = train)

summary(model_9)
vif(model_9)

# removing citympg due to low p value

model_10 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                 CarNameisuzu  + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab +
                 CarNametoyota  + CarNamevolvo +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + peakrpm + stroke, 
               data = train)

summary(model_10)
vif(model_10)

# removing peakrpm due to low p value

model_11 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                 CarNameisuzu + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault + CarNamesaab +
                 CarNametoyota + CarNamevolvo +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_11)
vif(model_11)

# removing CarNamesaab due to low p value

model_12 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge + CarNamehonda +
                 CarNameisuzu  + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault  +
                 CarNametoyota + CarNamevolvo +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_12)
vif(model_12)

# removing CarNamehonda due to low p value

model_12 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge  +
                 CarNameisuzu  + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault  +
                 CarNametoyota + CarNamevolvo +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_12)
vif(model_12)

# removing CarNamevolvo due to low p value

model_13 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge  +
                 CarNameisuzu  + CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault  +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_13)
vif(model_13)

# removing CarNameisuzu due to low p value

model_14 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge  +
                  CarNamemazda + CarNamemercury + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault  +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_14)
vif(model_14)

# removing CarNamemercury due to low p value

model_15 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge  +
                 CarNamemazda  + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth + CarNamerenault  +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_15)
vif(model_15)

# removing CarNamerenault due to low p value

model_16 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge  +
                 CarNamemazda  + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth   +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor + fuelsystem2bbl + stroke, 
               data = train)

summary(model_16)
vif(model_16)

# removing fuelsystem2bbl due to low p value and high vIF

model_17 <- lm(formula = price ~ aspiration + CarNamebmw + CarNamedodge  +
                 CarNamemazda  + CarNamemitsubishi +
                 CarNamenissan + CarNameplymouth   +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_17)
vif(model_17)

# removing CarNamedodge, CarNameplymouth due to low p value

model_18 <- lm(formula = price ~ aspiration + CarNamebmw   +
                 CarNamemazda  + CarNamemitsubishi +
                 CarNamenissan +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypel + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_18)
vif(model_18)

# removing enginetypel due to low p value

model_19 <- lm(formula = price ~ aspiration + CarNamebmw   +
                 CarNamemazda  + CarNamemitsubishi +
                 CarNamenissan +
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_19)
vif(model_19)

# removing CarNamenissan due to low p value

model_20 <- lm(formula = price ~ aspiration + CarNamebmw   +
                 CarNamemazda  + CarNamemitsubishi +
                 
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_20)
vif(model_20)

# removing CarNamemazda due to low p value

model_21 <- lm(formula = price ~ aspiration + CarNamebmw   +
                  CarNamemitsubishi +
                 
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_21)
vif(model_21)

# removing CarNamemitsubishi due to low p value

model_22 <- lm(formula = price ~ aspiration + CarNamebmw   +
                 
                 
                 CarNametoyota  +
                 carwidth   +
                 enginelocation + enginesize  + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_22)
vif(model_22)

# removing CarNametoyota due to low p value

model_23 <- lm(formula = price ~ aspiration + CarNamebmw   +
                 carwidth   +
                 enginelocation + enginesize  + enginetypeohcf +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_23)
vif(model_23)

# removing enginetypeohcf due to low p value

model_24 <- lm(formula = price ~ aspiration + CarNamebmw   +
                 carwidth   +
                 enginelocation + enginesize   +
                 enginetyperotor  + stroke, 
               data = train)

summary(model_24)
vif(model_24)

#########################################################################################################

# predicting the results in test dataset
Predict_1 <- predict(model_24,test[,-1])

test$test_price <- Predict_1

test[,c("price","test_price")]

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
