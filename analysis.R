rm(list = ls())
#### LOAD LIBRARIES
library(dplyr)
library(sqldf)
library(MASS)
library(ggplot2)
theme_set(theme_classic())

#### READ DATA
df_pinfo <- read.csv('player_info.csv')
df_region <- read.csv('region.csv', fileEncoding="UTF-8-BOM")
df_serve <- read.csv('serve_stats.csv', fileEncoding="UTF-8-BOM")
df_return <- read.csv('return_stats.csv', fileEncoding="UTF-8-BOM")
df_pressure <- read.csv('pressure_stats.csv', fileEncoding="UTF-8-BOM")

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
summary(servePr) # 5 PCs (99%)
plot(servePr, type = "l") # optimal -> 3 clusters
biplot(servePr, scale = 0)

# Extract PC scores
str(servePr)
servePr$x
servePCAdata2 <- cbind(servePCAdata, servePr$x[,1:5]) 
head(servePCAdata2)

# Correlations between variables and principal components
cor(servePCAdata2[,1:8], servePCAdata2[,9:13])

## Return
returnPCAdata <- alldata[, 18:23] # All return related columns
returnPr <- prcomp(returnPCAdata[, 3:6], scale = TRUE) # Remove ranking and rating columns for clustering
returnPr
summary(returnPr) # 3 PCs (99%)
plot(returnPr, type = "l") # optimal -> 2 clusters
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
plot(pressurePr, type = "l") # optimal -> 3 clusters
biplot(pressurePr, scale = 0)

# Extract PC scores
str(pressurePr)
pressurePr$x
pressurePCAdata2 <- cbind(pressurePCAdata, pressurePr$x[,1:4])
head(pressurePCAdata2)

# Correlations between variables and principal components
cor(pressurePCAdata[,1:6], pressurePCAdata2[,7:10])


#### CLUSTERING

## Serve k-means (3 clusters)
serveK <- kmeans(servePCAdata2[,9:13], 3)
serveK
str(serveK)
plot(servePCAdata, col = serveK$cluster)
# Analyze clusters
summary(servePCAdata[serveK$cluster == 1,]) # Worst
summary(servePCAdata[serveK$cluster == 2,]) # Best
summary(servePCAdata[serveK$cluster == 3,]) # Middle
# Serve clusters do not match exactly

## Return k-means (2 clusters)
returnK <- kmeans(returnPCAdata2[,7:9], 2)
returnK
str(returnK)
plot(returnPCAdata, col = returnK$cluster)
# Analyze clusters
summary(returnPCAdata[returnK$cluster == 1,]) # Best
summary(returnPCAdata[returnK$cluster == 2,]) # Worst
# Return clusters matches rankings exactly

## Pressure k-means (3 clusters)
pressureK <- kmeans(pressurePCAdata2[,7:10], 3)
pressureK
str(pressureK)
plot(pressurePCAdata, col = pressureK$cluster)
# Analyze clusters
summary(pressurePCAdata[pressureK$cluster == 1,]) # Worst - good at BPs won and deciding set won
summary(pressurePCAdata[pressureK$cluster == 2,]) # Worst - good at BPs saved and tiebreaks won
summary(pressurePCAdata[pressureK$cluster == 3,]) # Best
# Clusters don't divide data very well

## Group of best in everything
# With pressure
alldata[serveK$cluster == 2 & returnK$cluster == 1 & pressureK$cluster == 3, 3:4] # 11 results
# Without pressure
alldata[serveK$cluster == 2 & returnK$cluster == 1, 3:4] # 12 results

## Group of worst in everything
alldata[serveK$cluster == 1 & returnK$cluster == 2 & (pressureK$cluster == 1 | pressureK$cluster == 2), 3:4] # 23 results
# Without pressure
alldata[serveK$cluster == 1 & returnK$cluster == 2, 3:4] # 28 results

## Great servers with bad returns
alldata[serveK$cluster == 2 & returnK$cluster == 2, 3:4] # 17 results

## Great returners with bad serves
alldata[serveK$cluster == 1 & returnK$cluster == 1, 3:4] # 30 results


#### LINEAR REGRESSION

## Height VS Service rating
## How does height affect a player's serve?
cor(alldata$Height.cms., alldata$Rating) # 0.61 -> medium positive correlation
height_serveLR <- lm(alldata$Rating ~ alldata$Height.cms.)

## Height VS Return rating
## How does height affect a player's return?
cor(alldata$Height.cms., alldata$Rating.1) # -0.38 -> weak negative correlation
height_returnLR <- lm(alldata$Rating.1 ~ alldata$Height.cms.)

## First serve % VS Service games won %
## Does first serve percentage matter in terms of service games won?
cor(alldata$First_perc, alldata$Games_won_perc) # 0.03 -> no correlation
fserve_gwonLR <- lm(alldata$Games_won_perc ~ alldata$First_perc)
