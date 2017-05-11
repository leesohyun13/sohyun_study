---
#### title: "Sanfrancisco"
#### author: "Leesohyun"
#### date: '2017 5 11 '
#### output: html_document
---


```{r}
setwd("/Users/leesohyun/Desktop/data_study/san Fransisco crime")
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
```

```{r}
sf_train <- read_csv("train.csv")
sf_test <- read_csv("test.csv")
```

```{r}
head(sf_train)
summary(sf_train)
View(sf_train)
str(sf_train)
```

```{r}
sf_train$Descript <- NULL
sf_train$Resolution <- NULL
sf_train$Category <- factor(sf_train$Category)
sf_train$DayOfWeek <- factor(sf_train$DayOfWeek)
sf_train$PdDistrict <- factor(sf_train$PdDistrict)


sf_test$DayOfWeek <- factor(sf_test$DayOfWeek)
sf_test$PdDistrict <- factor(sf_test$PdDistrict)


levels(sf_train$Category) #38(one is NON-CRIMINAL)
table(sf_train$Category)  #bar-chart is hard to see 
```


## date_feature engineering
## 날짜 변수
```{r}
sf_train$Dates <- as.POSIXct(strptime(sf_train$Dates, format = "%Y-%m-%d %H:%M:%S"))
sf_train$ymd <- sapply(strsplit(as.character(sf_train$Dates), " "), "[", 1)
sf_train$times <- sapply(strsplit(as.character(sf_train$Dates), " "), "[", 2)
sf_train$year <- as.integer(sapply(strsplit(as.character(sf_train$ymd), "-"), "[", 1))
sf_train$month <- as.integer(sapply(strsplit(as.character(sf_train$ymd), "-"), "[", 2))
sf_train$day <- as.integer(sapply(strsplit(as.character(sf_train$ymd), "-"), "[", 3))
sf_train$hour <- as.integer(sapply(strsplit(as.character(sf_train$times), ":"), "[", 1))
sf_train$min <- as.integer(sapply(strsplit(as.character(sf_train$times), ":"), "[", 2))
sf_train$weekend_fri <- ifelse(sf_train$DayOfWeek=="Friday",1,0)
sf_train$weekend_sat <- ifelse(sf_train$DayOfWeek=="Saturday",1,0)
#hour_hot <- c(12, 13, 14, 15, 16, 17, 18, 19)
#sf_train$hot_time <- ifelse(hour_hot==sf_train$hour,1,0)
#sf_train %>%
#     ggplot(aes(factor(hour))) + 
#     geom_bar()

sf_train$ymd <- NULL
sf_train$Dates <- NULL


sf_test$Dates <- as.POSIXct(strptime(sf_test$Dates, format = "%Y-%m-%d %H:%M:%S"))
sf_test$ymd <- sapply(strsplit(as.character(sf_test$Dates), " "), "[", 1)
sf_test$times <- sapply(strsplit(as.character(sf_test$Dates), " "), "[", 2)
sf_test$year <- as.integer(sapply(strsplit(as.character(sf_test$ymd), "-"), "[", 1))
sf_test$month <- as.integer(sapply(strsplit(as.character(sf_test$ymd), "-"), "[", 2))
sf_test$day <- as.integer(sapply(strsplit(as.character(sf_test$ymd), "-"), "[", 3))
sf_test$hour <- as.integer(sapply(strsplit(as.character(sf_test$times), ":"), "[", 1))
sf_test$min <- as.integer(sapply(strsplit(as.character(sf_test$times), ":"), "[", 2))
sf_test$weekend_fri <- ifelse(sf_test$DayOfWeek=="Friday",1,0)
sf_test$weekend_sat <- ifelse(sf_test$DayOfWeek=="Saturday",1,0)
#hour_hot <- c(12, 13, 14, 15, 16, 17, 18, 19)
#sf_test$hot_time <- ifelse(hour_hot==sf_test$hour,1,0)

sf_test$ymd <- NULL
sf_test$Dates <- NULL
```


## Criminal count
```{r}
sf_train %>% 
  group_by(Category) %>%
  summarise(count=n()) %>%
  mutate(Category = reorder(Category, -count)) %>%
  ggplot(aes(x=Category, y = count, fill=Category)) + geom_col() + 
  coord_flip() + 
  theme(legend.position = "none")
```
### we consider to handle 'non-criminal' variable. (non-criminal is very high)   

### Criminal by year, month, hour
```{r}
sf_train %>%
  group_by(Year, Category) %>%
  summarise(count=n()) %>%
  top_n(10) %>%
  transform(Category = reorder(Category, - count)) %>%
  ggplot(aes(x=Category, y=count, color=Category, fill=Category)) + 
  geom_col() + 
  facet_wrap(~Year, scales = "free_y") + 
  coord_flip() + 
  theme(legend.position = "none")
```
### 달마다 차이는 없지만 년도별 범죄의 차이가 있다.

```{r}
sf_train %>%
  group_by(Month, Category) %>%
  summarise(count=n()) %>%
  top_n(10) %>%
  transform(Category = reorder(Category, - count)) %>%
  ggplot(aes(x=Category, y=count, color=Category, fill=Category)) + 
  geom_col() + 
  facet_wrap(~Month, scales = "free_y") + 
  coord_flip() + 
  theme(legend.position = "none")
```

```{r}
sf_train %>%
  group_by(hour, Category) %>%
  summarise(count=n()) %>%
  top_n(10) %>%
  transform(Category = reorder(Category, - count)) %>%
  ggplot(aes(x=Category, y=count, color=Category, fill=Category)) + 
  geom_col() + 
  facet_wrap(~hour, scales = "free_y") + 
  coord_flip() + 
  theme(legend.position = "none")
```


## Criminal by weekend, weekend
```{r}
weekend <- c("Friday","Saturday","Sunday")
ifelse(weekend == sf_train$DayOfWeek,1,0) -> sf_train$weekend_Or_Not
ifelse(weekend == sf_test$DayOfWeek,1,0) -> sf_test$weekend_Or_Not
```

```{r}
train %>%
group_by(weekend_Or_Not, Category) %>%
  summarise(count=n()) %>%
  top_n(10) %>%
  transform(Category = reorder(Category, - count)) %>%
  ggplot(aes(x=Category, y=count, color=Category, fill=Category)) + 
  geom_col() + 
  facet_wrap(~weekend_Or_Not, scales = "free_y") + 
  coord_flip() + 
  theme(legend.position = "none")
## 0 - not weekend, 1 - weekend
```
  
### PdDistrict - Criminal
```{r}
levels(train$PdDistrict)

train %>%
  group_by(PdDistrict, Category) %>%
  summarise(count=n()) %>%
  top_n(10) %>%
  transform(Category = reorder(Category, - count)) %>%
  ggplot(aes(x=Category, y=count, color=Category, fill=Category)) + 
  geom_col() + 
  facet_wrap(~PdDistrict, scales = "free_y") + 
  coord_flip() + 
  theme(legend.position = "none")
```

### map
```{r}
library(ggmap)
SF <- get_map(location = 'San Francisco',
              zoom = 13,
              maptype = "roadmap"
)

### Generate a heat map
ggmap(SF) + 
  geom_point(, aes(x = X, y = Y,
                               color = Category, fill = Category),
             alpha = .1)


### map_in_kaggle
sfMap <- qmap("San Francisco", zoom = 13, color = "bw")
saveRDS(sfMap, file = "sf_map_copyright_openstreetmap_contributors.rds")
map <- readRDS("sf_map_copyright_openstreetmap_contributors.rds")
counts <- summarise(group_by(sf_train, Category), Counts=length(Category))
counts
counts <- counts[order(-counts$Counts),]
counts
top12 <- train[sf_train$Category %in% counts$Category[c(1,3:13)],]
head(top12)
p <- map + geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Type of Crime")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("Top Crimes in San Francisco") +
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
p
```


### Address dummy variable 
```{r}
Intersection <- "/"
Block <- "Block"
sf_train$Intersection <- ifelse(str_detect(sf_train$Address, "/"), 1, 0)
sf_train$Block <- ifelse( str_detect(sf_train$Address, "Block"), 1, 0)


sf_test$Intersection <- ifelse(str_detect(sf_test$Address, "/"), 1, 0)
sf_test$Block <- ifelse( str_detect(sf_test$Address, "Block"), 1, 0)
```

### Address변수 나눠 더미 변수화
```{r}
sf_trainAddress <- strsplit(sf_train$Address, " ")
AddressName <- function(x){
  roadType <- rep("none", length(x))
  for(i in 1:length(x)){
    temp <- x[[i]]
    if("/" %in% temp){
      roadType[i] <- "Intersection"
    }  else if("ST" %in% temp){
      roadType[i] <- "Street"
    } else if("WY" %in% temp){
      roadType[i] <- "Way"
    }  else if("AV" %in% temp){
      roadType[i] <- "Avenue"
    }  else if("CT" %in% temp){
      roadType[i] <- "Court"
    }  else if("DR" %in% temp){
      roadType[i] <- "Drive"
    }  else if("PL" %in% temp){
      roadType[i] <- "Plaza"
    }  else if("TR" %in% temp){
      roadType[i] <- "Trail"
    } else if("BL" %in% temp){
      roadType[i] <- "Boulevard"
    } else if("LN" %in% temp){
      roadType[i] <- "Lane"
    }else{
      roadType[i] <- "others"
    }
  }
  address.df <- data.frame(Roadname = roadType)
  return(address.df)
}
sf_trainAddress.df <- AddressName(sf_trainAddress)



sf_testAddress <- strsplit(sf_test$Address, " ")
AddressName <- function(x){
  roadType <- rep("none", length(x))
  for(i in 1:length(x)){
    temp <- x[[i]]
    if("/" %in% temp){
      roadType[i] <- "Intersection"
    }  else if("ST" %in% temp){
      roadType[i] <- "Street"
    } else if("WY" %in% temp){
      roadType[i] <- "Way"
    }  else if("AV" %in% temp){
      roadType[i] <- "Avenue"
    }  else if("CT" %in% temp){
      roadType[i] <- "Court"
    }  else if("DR" %in% temp){
      roadType[i] <- "Drive"
    }  else if("PL" %in% temp){
      roadType[i] <- "Plaza"
    }  else if("TR" %in% temp){
      roadType[i] <- "Trail"
    } else if("BL" %in% temp){
      roadType[i] <- "Boulevard"
    } else if("LN" %in% temp){
      roadType[i] <- "Lane"
    }else{
      roadType[i] <- "others"
    }
  }
  address.df <- data.frame(Roadname = roadType)
  return(address.df)
}
sf_testAddress.df <- AddressName(sf_testAddress)


types <- c("Street", "Intersection", "Avenue", "Boulevard", "Drive", "Way", "Road", "Plaza", "Court", "Highway", "Lane", "Trail")
for(type in types){
  sf_train[, type] <- ifelse(sf_trainAddress.df$Roadname == type, 1, 0)
  sf_test[, type] <- ifelse(sf_testAddress.df$Roadname == type, 1, 0)
}

sf_train$Address <-NULL
sf_test$Address <- NULL
gc()
```


## XGBOOST
```{r}
library(xgboost)
trainLabel<- sf_train$Category
trainLabel.num <- as.numeric(as.factor(trainLabel)) -1

trainMat <- as.matrix(sf_train[, -1])
testMat <- as.matrix(sf_test[,-1])

trainMat <- as(trainMat, "dgCMatrix")
testMat <- as(testMat, "dgCMatrix")


param <- list(
  #nthread             = 4,
  booster             = 'gbtree',
  objective           = 'multi:softprob',
  num_class           = 39,
  eta                 = 0.3,
  #gamma               = 0,
  max_depth           = 6,
  #min_child_weigth    = 1,
  max_delta_step      = 1
  #subsample           = 1,
  #colsample_bytree    = 1,
  #early.stop.round    = 5
)

sf_xgb <- xgboost(
  params = param,
  data = trainMat,
  label = trainLabel.num,
  eval_metric  = 'mlogloss',
  nrounds = 15
)
```

```{r}
 sf_xgb_imp <- xgb.importance(colnames(trainMat), model = sf_xgb)
xgb.plot.importance(sf_xgb_imp)

sf_pred <- predict(sf_xgb, testMat)
sf_pred_mat <- matrix(sf_pred, ncol = 39, byrow = T)
rm(sf_pred)
gc()
```

```{r}
submit <- read_csv("sampleSubmission.csv")
submit[, 2:40] <- sf_pred_mat

write.csv(submit, "submission_2.csv", row.names = FALSE)
## score : 2.439
```


## random forest
```{r}
doParallel::registerDoParallel(4)
library(ranger)
set.seed(1)

sf_train$Category <- as.factor(sf_train$Category)

sf_rf <- ranger(Category ~ ., data = sf_train, num.trees = 100, 
                importance = 'impurity', write.forest = TRUE, probability = TRUE)
```

```{r}
sf_rf_pred <- predict(sf_rf, sf_test)
sf_rf_pred <- sf_rf_pred$predictions

submit <- read_csv("sampleSubmission.csv")
submit[, 2:40] <- sf_rf_pred

write.csv(submit, "submission_r.csv", row.names = FALSE)
```
#### score : 2.32





############################################################################################
####################################   Reference  ##########################################
############################################################################################
#1. https://www.kaggle.com/burakhmmtgl/sf-crime/feature-engineering-with-tidyverse/notebook
#2. https://www.kaggle.com/vivekyadav/sf-crime/sfo-rmd-kaggle
#3. https://www.kaggle.com/benhamner/sf-crime/san-francisco-top-crimes-map ---> map
