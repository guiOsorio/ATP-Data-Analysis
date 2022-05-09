rm(list = ls())
#### LOAD LIBRARIES
library(dplyr)
library(sqldf)
library(MASS)
library(ggplot2)
theme_set(theme_classic())

#### READ DATA
df_pinfo <- read.csv('../STAGE_1/player_info.csv', fileEncoding="UTF-8-BOM")
df_region <- read.csv('../STAGE_1/region.csv', fileEncoding="UTF-8-BOM")
df_serve <- read.csv('../STAGE_1/serve_stats.csv', fileEncoding="UTF-8-BOM")
df_return <- read.csv('../STAGE_1/return_stats.csv', fileEncoding="UTF-8-BOM")
df_pressure <- read.csv('../STAGE_1/pressure_stats.csv', fileEncoding="UTF-8-BOM")

View(df_pinfo)
View(df_region)
View(df_serve)
View(df_return)
View(df_pressure)


#### EXPLORATORY DATA ANALYSIS

## Join of region + player info
region_pinfoQ <- 
  "SELECT * FROM df_pinfo 
    JOIN df_region ON df_pinfo.Region_ID = df_region.Region_ID"
region_pinfo <- sqldf(region_pinfoQ)
region_pinfo <- region_pinfo[, -6]
region_pinfo$Region <- as.factor(region_pinfo$Region)
View(region_pinfo)

## Histogram of data by region
region_pinfo %>% ggplot(aes(Region, fill = Region)) +
  geom_histogram(stat = "count") +
  scale_fill_manual(values = c("#FCB131", "#0081C8", "#00A651", "#EE334E")) +
  labs(title = "Players by Region", tag = "1")

## Boxplots of age, weight, and height by region

# By age
region_pinfo %>% ggplot(aes(Region, Age, color = Region)) +
  geom_boxplot() +
  scale_color_manual(values = c("#FCB131", "#0081C8", "#00A651", "#EE334E")) +
  labs(title = "Age by Region", tag = "2")

# By weight
region_pinfo %>% ggplot(aes(Region, Weight.lbs., color = Region)) +
  geom_boxplot() +
  scale_color_manual(values = c("#FCB131", "#0081C8", "#00A651", "#EE334E")) +
  labs(title = "Weight by Region", y = "Weight (in lbs)", tag = "3")

# By height
region_pinfo %>% ggplot(aes(Region, Height.cms., color = Region)) +
  geom_boxplot() +
  scale_color_manual(values = c("#FCB131", "#0081C8", "#00A651", "#EE334E")) +
  labs(title = "Height by Region", y = "Height (in cms)", tag = "4")

## Plot weight against height
region_pinfo %>% ggplot(aes(Weight.lbs., Height.cms.)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "Weight VS Height", x = "Weight (in lbs)", y = "Height (in cms)", tag = "5")

## Join of all data
alldataQ <- 
  "SELECT * FROM df_region
    JOIN df_pinfo ON df_region.Region_ID = df_pinfo.Region_ID
      JOIN df_serve ON df_pinfo.ATP_Rank = df_serve.Player_ID
      JOIN df_return ON df_pinfo.ATP_Rank = df_return.Player_ID
      JOIN df_pressure ON df_pinfo.ATP_Rank = df_pressure.Player_ID"

alldata <- sqldf(alldataQ)
dim(alldata)
alldata <- alldata[, -31]
View(alldata)

## Plot age against pressure rating
alldata %>% ggplot(aes(Age, Rating.2)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "Age VS Pressure Rating", y = "Pressure Rank", tag = "6")

## Plot height against service rating
alldata %>% ggplot(aes(Height.cms., Rating)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "Height VS Service Rating", y = "Service Rank", tag = "7")

## Plot height against return rating
alldata %>% ggplot(aes(Height.cms., Rating.1)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "Height VS Return Rating", x = "Height (in cms)", y = "Return Rank", tag = "8")

## Plot first serve % against service games won %
## Does first serve percentage matter in terms of service games won?
alldata %>% ggplot(aes(First_perc, Games_won_perc)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "First Serve % VS Service Games Won %", x = "First Serve %", y = "Service Games Won %", tag = "9")

## Plot height against tiebreaks won %
## Does height matter in determining a tiebreak's winner?
alldata %>% ggplot(aes(Height.cms., Tiebreaks_won_perc)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "Height VS Tiebreaks Won %", x = "Height (in cms)", y = "Tiebreaks Won %", tag = "10")

## Plot service rating against return rating
alldata %>% ggplot(aes(Rating, Rating.1)) +
  geom_point() +
  geom_smooth(method = "rlm") +
  labs(title = "Service Rating VS Return Rating", x = "Service Rating", y = "Return Rating", tag = "11")

## Age Range column
range(alldata$Age)
alldata <- mutate(
  alldata,
  Age_Range = 
    if_else(Age > 35, "35+",
     if_else(Age > 30, "31-35",
      if_else(Age > 25, "26-30",
        if_else(Age > 20, "21-25", "18-20")
      )
    )
  )
)

## Bar chart of age range
alldata %>% ggplot(aes(Age_Range, fill = "red")) +
  geom_bar() +
  labs(title = "Frequency of Players By Age Range", x = "Age Range", y = "Frequency", tag = "12")

## Facet grid of bar charts by region
alldata %>% ggplot(aes(Age_Range, fill = Region)) +
  geom_bar() +
  scale_fill_manual(values = c("#FCB131", "#0081C8", "#00A651", "#EE334E")) +
  labs(title = "Frequency of Players By Age Range By Region", x = "Age Range", y = "Frequency", tag = "13") +
  facet_wrap(. ~ Region)


#### PRINCIPAL COMPONENT ANALYSIS

## Serve
servePCAdata <- alldata[, 9:16] # All serve related columns
servePr <- prcomp(servePCAdata[, 3:8], scale = TRUE) # Remove ranking and rating columns for clustering
servePr
summary(servePr) # 4 PCs (95%)
# The PCA successfully reduces the dimensionality of the data
plot(servePr, type = "l") # optimal -> 4 PCs
biplot(servePr, scale = 0)

# Extract PC scores
str(servePr)
servePr$x
servePCAdata2 <- cbind(servePCAdata, servePr$x[,1:4]) # Add PCs to the original serve data
head(servePCAdata2)

# Correlations between variables and principal components
cor(servePCAdata[,3:8], servePCAdata2[,9:12])

## Return
returnPCAdata <- alldata[, 18:23] # All return related columns
returnPr <- prcomp(returnPCAdata[, 3:6], scale = TRUE) # Remove ranking and rating columns for clustering
returnPr
summary(returnPr) # 3 PCs (95%)
plot(returnPr, type = "l") # optimal -> 2 PCs, but decided to go with 3 due to 95% threshold
biplot(returnPr, scale = 0)

# Extract PC scores
str(returnPr)
returnPr$x
returnPCAdata2 <- cbind(returnPCAdata, returnPr$x[,1:3])
head(returnPCAdata2)

# Correlations between variables and principal components
cor(returnPCAdata[,1:6], returnPCAdata2[,7:9])

## Pressure
pressurePCAdata <- alldata[, 25:30] # All pressure related columns
pressurePr <- prcomp(pressurePCAdata[, 3:6], scale = TRUE) # Remove ranking and rating columns for clustering
pressurePr
summary(pressurePr) # 4 PCs (99%)
# Since the PCA does not successfully reduce the dimensionality of the data, we don't really need it
plot(pressurePr, type = "l") # optimal -> 3 PCs
biplot(pressurePr, scale = 0)

# Extract PC scores
str(pressurePr)
pressurePr$x
pressurePCAdata2 <- cbind(pressurePCAdata, pressurePr$x[,1:4])
head(pressurePCAdata2)

# Correlations between variables and principal components
cor(pressurePCAdata[,1:6], pressurePCAdata2[,7:10])


#### CLUSTERING

# Checks the optimal number of clusters
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

## Serve k-means 
# Optimal # of clusters 
wssplot(servePCAdata2[,9:12]) # (3 clusters)
serveK <- kmeans(servePCAdata2[,9:12], 3)
serveK
str(serveK)
plot(servePCAdata, col = serveK$cluster)
# Analyze clusters
summary(servePCAdata[serveK$cluster == 1,]) # Best
summary(servePCAdata[serveK$cluster == 2,]) # Worst
summary(servePCAdata[serveK$cluster == 3,]) # Middle
# Serve clusters do not match rankings exactly

## Return k-means (3 clusters)
# Optimal # of clusters 
wssplot(returnPCAdata2[,7:9])
returnK <- kmeans(returnPCAdata2[,7:9], 3)
returnK
str(returnK)
plot(returnPCAdata, col = returnK$cluster)
# Analyze clusters
summary(returnPCAdata[returnK$cluster == 1,]) # Middle
summary(returnPCAdata[returnK$cluster == 2,]) # Worst
summary(returnPCAdata[returnK$cluster == 3,]) # Best
# Return clusters matches rankings exactly

## Pressure k-means (4 clusters)
# Optimal # of clusters 
wssplot(pressurePCAdata2[,7:10])
pressureK <- kmeans(pressurePCAdata2[,7:10], 4)
pressureK
str(pressureK)
plot(pressurePCAdata, col = pressureK$cluster)
# Analyze clusters
summary(pressurePCAdata[pressureK$cluster == 1,]) # Good
summary(pressurePCAdata[pressureK$cluster == 2,]) # Bad
summary(pressurePCAdata[pressureK$cluster == 3,]) # Good
summary(pressurePCAdata[pressureK$cluster == 4,]) # Bad
# Ranks based on median and average:
# BPs won - 3, 4, 1, 2 -> 1 and 2 very similar, 3 significantly better
# BPs saved - 2, 1, 3, 4 -> 1 and 2 very similar, 4 significantly worse
# Tiebreaks won - 3, 2, 1, 4 -> 1 and 2 similar, 4 extremely worse
# Deciding set won - 1, 3, 4, 2 -> 3 and 4 similar, 2 extremely worse
# Clusters don't divide data very well

## Group of best in everything
# With pressure
best <- alldata[serveK$cluster == 1 & returnK$cluster == 3 & (pressureK$cluster == 1 || pressureK$cluster == 3), 3:4] # 11 results
# Without pressure
best_nop <- alldata[serveK$cluster == 1 & returnK$cluster == 3, 3:4] # 11 results

## Group of worst in everything
worst <- alldata[serveK$cluster == 2 & returnK$cluster == 2 & (pressureK$cluster == 2 || pressureK$cluster == 4), 3:4] # 0 results
# Without pressure
worst_nop <- alldata[serveK$cluster == 2 & returnK$cluster == 2, 3:4] # 0 results

## Great servers with bad returns
gserve_breturn <- alldata[serveK$cluster == 1 & returnK$cluster == 2, 3:4] # 8 results

## Great returners with bad serves
greturn_bserve <- alldata[serveK$cluster == 2 & returnK$cluster == 3, 3:4] # 14 results

# Add binary variables to represent if the player belongs to each group created
alldata <-
  mutate(
    alldata,
    Returners = 
      ifelse(alldata$ATP_Rank %in% greturn_bserve$ATP_Rank, 1, 0),
    Servers = 
      ifelse(alldata$ATP_Rank %in% gserve_breturn$ATP_Rank, 1, 0),
    Worst_allaround =
      ifelse(alldata$ATP_Rank %in% worst$ATP_Rank, 1, 0),
    Worst_allaround_nop =
      ifelse(alldata$ATP_Rank %in% worst_nop$ATP_Rank, 1, 0),
    Best_allaround =
      ifelse(alldata$ATP_Rank %in% best$ATP_Rank, 1, 0),
    Best_allaround_nop =
      ifelse(alldata$ATP_Rank %in% best_nop$ATP_Rank, 1, 0)
    )
View(alldata)
#### VISUALIZATIONS PART 2

## Returner's distribution by height
# Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Returners))) +
  geom_bar() +
  labs(title = "Distribution By Height", subtitle = "Returners count", x = "Height (in cms)", fill = "Returner", tag = "14") +
  scale_fill_manual(values = c("#000000","#FFFF00"))
# Percent Stacked Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Returners))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution By Height", subtitle = "Returners proportion", x = "Height (in cms)", y = "proportion", fill = "Returner", tag = "15") +
  scale_fill_manual(values = c("#000000","#FFFF00"))

## Server's distribution by height
# Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Servers))) +
  geom_bar() +
  labs(title = "Distribution By Height", subtitle = "Servers count", x = "Height (in cms)", fill = "Server", tag = "16") +
  scale_fill_manual(values = c("#000000","#800080"))
# Percent Stacked Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Servers))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution By Height", subtitle = "Servers proportion", x = "Height (in cms)", y = "proportion", fill = "Server", tag = "17") +
  scale_fill_manual(values = c("#000000","#800080"))

## Best all around distribution by height (no pressure)
# Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Best_allaround_nop))) +
  geom_bar() +
  labs(title = "Distribution By Height", subtitle = "Best all around count", x = "Height (in cms)", fill = "Best", tag = "18") +
  scale_fill_manual(values = c("#000000","#00FF00"))
# Percent Stacked Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Best_allaround_nop))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution By Height", subtitle = "Best all around proportion", x = "Height (in cms)", y = "proportion", fill = "Best", tag = "19") +
  scale_fill_manual(values = c("#000000","#00FF00"))


## Worst all around distribution by height (no pressure)
# Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Worst_allaround_nop))) +
  geom_bar() +
  labs(title = "Distribution By Height", subtitle = "Worst all around count", x = "Height (in cms)", fill = "Worst", tag = "20") +
  scale_fill_manual(values = c("#000000", "#FF0000"))
# Percent Stacked Bar Plot
alldata %>%
  ggplot(aes(Height.cms., fill = factor(Worst_allaround_nop))) +
  geom_bar(position = "fill") +
  labs(title = "Distribution By Height", subtitle = "Worst all around proportion", x = "Height (in cms)", y = "proportion", fill = "Worst", tag = "21") +
  scale_fill_manual(values = c("#000000", "#FF0000"))


#### LINEAR REGRESSION

## Height VS Service rating
## How does height affect a player's serve?

# Do the variables seem related?
# Scatter plot of height against service rating
ggplot(alldata, aes(y=Rating, x=Height.cms.)) +
  geom_point() +
  labs(y="Service rating", x="Height (in cms)", title="Height (cms) VS Serve Rating")
# Correlation between variables
cor(alldata$Height.cms., alldata$Rating) # 0.61 -> medium positive correlation

# Linear regression model
height_serveLR <- lm(Rating ~ Height.cms., data = alldata)
summary(height_serveLR)

# Does the pattern of points reveal anything that might cause 
#us to question the assumptions underlying the appropriate 
#usage of regression analysis in this case?
# Residual plot
height_serveLR$residuals
ggplot(alldata, aes(x=Height.cms., y=height_serveLR$residuals)) +
  geom_point() +
  labs(x="Height (in cms)", y="Residuals", title="Residual Plot of Height") +
  geom_hline(yintercept = 0, color = "red")
# There are some outliers in the middle
###### Subset data using filter to eliminate residuals further than 20 from 0?

# Regression equation
height_serveLR # Service rating = 43.30 + 1.19 * Height (in cms)
# An increase of 1 cm in a player's height seems to increase the service
#rating by 1.19

# Confidence intervals
confint(height_serveLR, level=0.95) # The interval has 95% chance of containing the correct bi
confint(height_serveLR, level=0.99) # The interval has 99% chance of containing the correct bi

# R^2
summary(height_serveLR)
# R^2 = 0.3724 -> height explains 37.24% of variation in service rating

# p-value
summary(height_serveLR)
# p-value = 4.47e-13 -> less than 0.05, therefore it is statistically
#significant. Therefore, height does seem to be meaningful to our
#linear regression model

# By analyzing the R^2 and p-values, we can conclude that our model
#would benefit from adding more independent variables to help predict
#the service rating (because the p-value was low, meaning the
#independent variable was significant; but the r^2 value was weak)

# Predict values for heights of 170, 180, 190, 200
heights <- data.frame(Height.cms. <- c(170, 180, 190, 200))
predict(height_serveLR, heights)

# What are the predicted values for the service rating?
serverating_predicted <- fitted(height_serveLR)
alldata$serverating_predicted <- serverating_predicted
cor(alldata$serverating_predicted, alldata$Rating)
cor(alldata$serverating_predicted, alldata$Rating)^2 # value of r^2

# Check the fit of the regression line
ggplot(alldata, aes(x=Height.cms.,y=Rating)) +
  geom_point() +
  geom_line(color="red", aes(x=Height.cms., y=serverating_predicted)) +
  labs(x="Height (in cms)",y="Service Rating",title="Fit of regression line")



## Height VS Return rating
## How does height affect a player's return?

# Do the variables seem related?
# Scatter plot of height against return rating
ggplot(alldata, aes(y=Rating.1,x=Height.cms.)) +
  geom_point() +
  labs(y="Return rating", x="Height (in cms)", title="Height (cms) VS Return Rating")
# Correlation between variables
cor(alldata$Height.cms., alldata$Rating.1) # -0.38 -> weak negative correlation

# Linear regression model
height_returnLR <- lm(Rating.1 ~ Height.cms., data = alldata)
summary(height_returnLR)

# Does the pattern of points reveal anything that might cause 
#us to question the assumptions underlying the appropriate 
#usage of regression analysis in this case?
# Residual plot
ggplot(alldata, aes(x=Height.cms., y=height_returnLR$residuals)) +
  geom_point() +
  geom_hline(yintercept=0,color="red") +
  labs(x="Height (in cms)", y="Residuals", title="Residual plot")
# Some outliers in the middle
###### Subset data using filter to eliminate residuals further than 20 from 0?

# Regression equation
height_returnLR # Service rating = 257.88 - 0.63 * Height (in cms)
# An increase of 1 cm in a player's height seems to decrease the return
#rating by 0.63

# Confidence intervals
confint(height_returnLR, level=0.95) # The interval has 95% chance of containing the correct bi
confint(height_returnLR, level=0.99) # The interval has 99% chance of containing the correct bi

# R^2
summary(height_returnLR)
# R^2 = 0.1436 -> height explains 14.36% of variation in service rating

# p-value
summary(height_serveLR)
# p-value = 2.97e-05 -> less than 0.05, therefore it is statistically
#significant. Therefore, height does seem to be meaningful to our
#linear regression model

# The p-value and r^2 results tell us that, although height plays a
#role in predicting the return rating, we need more statistically
#significant independent variables to predict the return rating

# Predict values for heights of 170, 180, 190, 200
heights_2 <- data.frame(Height.cms. <- c(170, 180, 190, 200))
predict(height_returnLR, heights_2)

# What are the predicted values for the return rating?
returnrating_predicted <- fitted(height_returnLR)
alldata$returnrating_predicted <- returnrating_predicted
cor(alldata$returnrating_predicted, alldata$Rating.1)
cor(alldata$returnrating_predicted, alldata$Rating.1)^2 # value of r^2

# Check the fit of the regression line
ggplot(alldata, aes(x=Height.cms.,y=Rating.1)) +
  geom_point() +
  geom_line(color="red", aes(x=Height.cms., y=returnrating_predicted)) +
  labs(x="Height (in cms)",y="Return rating",title="Fit of regression line")



## First serve % VS Service games won %
## Does first serve percentage matter in terms of service games won?
# Do the variables seem related?
# Scatter plot of first serve against service games won
ggplot(alldata, aes(y=Games_won_perc,x=First_perc)) +
  geom_point() +
  labs(y="Service games won %", x="First serve %", title="First Serve % VS Service Games Won %")
# Correlation between variables
cor(alldata$Games_won_perc, alldata$First_perc) # 0.03 -> close to 0 = no correlation

# Linear regression model
fserve_gwonLR <- lm(Games_won_perc ~ First_perc, data = alldata)
summary(fserve_gwonLR)

# The r^2 of our model is extremely low (0.0009), and 
#the p-value of the independent variable used is
#too high (0.746), therefore, there is no point in
#using this model because these values tell us that
#first serve % does not matter when predicting the %
#of service games won



## Age VS Pressure rating
## Does age help predict pressure rating?

# Do the variables seem related?
# Scatter plot of age against pressure rating
ggplot(alldata, aes(y=Rating.2,x=Age)) +
  geom_point() +
  labs(y="Pressure rating", x="Age", title="Age VS Pressure Rating")
# Correlation between variables
cor(alldata$Age, alldata$Rating.2) # -0.11 -> very weak negative correlation

# Linear regression model
age_pressureLR <- lm(Rating.2 ~ Age, data = alldata)
summary(age_pressureLR)

# The r^2 of our model is extremely low (0.01291), and 
#the p-value of the independent variable used is
#too high (0.227), therefore, there is no point in
#using this model because these values tell us that
#age does not matter when predicting pressure rating



#### MULTIPLE LINEAR REGRESSION

## How will adding more variables to our model trying
##to predict service rating affect it?
## Let's add the ATP ranking and return rating variables

# Multi linear regression model
serve_multiLR <- lm(Rating ~ Height.cms. + ATP_Rank + Rating.1, data = alldata)
summary(height_serveLR)
summary(serve_multiLR)

# Original r^2 = 0.3724 -> New r^2 value = 0.5248 -> higher explanatory power
#The new model can explain 52.48% of the variation in service rating
# The p-value for all variables is below 0.05, so
#we should keep all of them since they are all 
#statistically significant (meaningful to the model)

# Regression equation
serve_multiLR 
# Service rating = 196.1628 + 0.7386 * Height (in cms) - 0.1604 * ATP ranking - 0.4172 * Return rating
# Taller players seem to have a better service rating,
#on the other hand, the higher (worst) a player's ranking
#is, the worse its serve stats seem to be, and the better
#the player's return rating, the worse its service rating

# Confidence intervals
confint(serve_multiLR, level=0.95) # The interval has 95% chance of containing the correct bi
confint(serve_multiLR, level=0.99) # The interval has 99% chance of containing the correct bi

# Predict serve rating based on different values for the
#independent variables
multi_predict_vals <- data.frame(Height.cms. <- c(170, 180, 190, 200), ATP_Rank <- c(10, 40, 70, 100), Rating.1 <- c(105, 125, 145, 165))
predict(serve_multiLR, multi_predict_vals)

# What are the predicted values for the serve rating?
serverating_predicted_multi <- fitted(serve_multiLR)
alldata$serverating_predicted_multi <- serverating_predicted_multi
cor(alldata$serverating_predicted_multi, alldata$Rating)
cor(alldata$serverating_predicted_multi, alldata$Rating)^2 # value of r^2


