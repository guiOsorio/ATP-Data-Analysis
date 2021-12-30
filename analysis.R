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
servePCAdata <- alldata[, 9:16]
servePr <- prcomp(servePCAdata, scale = TRUE)
servePr
summary(servePr)
plot(servePr, type = "l") # 3 clusters
biplot(servePr, scale = 0)

# Extract PC scores
str(servePr)
servePr$x
servePCAdata2 <- cbind(servePCAdata, servePr$x[,1:5])
head(servePCAdata2)

# Correlations between variables and principal components
cor(servePCAdata2[,1:8], servePCAdata2[,9:13])

## Return
returnPCAdata <- alldata[, 18:23]
returnPr <- prcomp(returnPCAdata, scale = TRUE)
returnPr
summary(returnPr)
plot(returnPr, type = "l") # 2 clusters
biplot(returnPr, scale = 0)

# Extract PC scores
str(returnPr)
returnPr$x
returnPCAdata2 <- cbind(returnPCAdata, returnPr$x[,1:4])
head(returnPCAdata2)

# Correlations between variables and principal components
cor(returnPCAdata[,1:6], returnPCAdata2[,7:10])

## Pressure
pressurePCAdata <- alldata[, 25:30]
pressurePr <- prcomp(pressurePCAdata, scale = TRUE)
pressurePr
summary(pressurePr)
plot(pressurePr, type = "l") # 4 clusters
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

## Return k-means (2 clusters)
returnK <- kmeans(returnPCAdata2[,7:10], 3)
returnK
str(returnK)
plot(returnPCAdata, col = returnK$cluster)

## Pressure k-means (4 clusters)
pressureK <- kmeans(pressurePCAdata2[,7:10], 4)
pressureK
str(pressureK)
plot(pressurePCAdata, col = pressureK$cluster)


#### LINEAR REGRESSION





