# sohyun_study
my first blog about data analysis and algorithm

---
title: "prudential final" 
author: "Leesohyun"
date: 2017 4 23 
output: html_document
---

## prudential insurance classification.   
 이 데이터는 prudential이라는 보험회의 데이터로, 사람들이 든 보험 정보다 그 사람의 개인적인 자료들을 통해 해당 가입 보험에 대해 얼마나 만족하는지(Response)를 예측하는 것이다.  
데이터는 모두 **정규화**되어 있으며, 일부의 데이터는 **na**값은 많이 가지고 있다. 그래서 이 데이터를 통해 정규화되어 있는 데이터를 어떻게 분석할 것인지, na를 어떤 방식으로 처리하는 것이 좋은지가 중요한 포인트라고 할 수 있다. 분석을 하면서 모든 데이터가 정규화 되어 있어 데이터 탐색 결과를 보아도 어떤 결과이거 앞으로 어떻게 전처리 해야 하는지 알 수 없어 이 점에서 특히 어려웠다. 또한 해당 데이터의 변수의 개수가 많아 데이터 탐색에서 많은 시간을 소요했다.  
탐색을 통해 전처리할 주요 변수 product_info_2와 na처리가 있었다. pruduct_Info_2는 고객들이 가입한 보험을 분류한 코드 일부였다. 우리는 이 코드를 통해 각 분류 번호마다 고객들의 나이와 몸무게, bmi가 다르다는 것을 발견하고 이것을 더미변수화 하였다. 그리고 na가 많은 변수들을 없애면 정보손실이 심하기 때문에 na를 없애는 대신에 대체되는 값을 넣어주었다.(-1) -1을 넣은 이유는 na가 아닌 값들과 겹치지 않게 하기 위함이었다. 여러가지 값을 대입해본 결과 -1이 적당하다고 판단하였다.  


### 평가 기준 : quadratic weighted kappa  
참고자료 : the Kappa statistic in reliability studies: use, Interpretation, and sample size #equirements
요약 : kappa는 categorical한 변수에 있어 알맞게 측정했는지를 측정하는 것이다. 여기에 잘못 분류한 것에 weight를 추가로 주는 것이 weighted kappa이다. kappa에서 고려해야 할 것은 true negative와 true positive 개수에 있어 많은 차이가 없어야 한다.(Prevalence : |a-d|/n) 둘째는 false positive와 false negative사이의 차이가 클수록 kappa의 값이 높아진다는 것이다. 왜냐면 kappa값은 얼마나 잘 예측했는지를 중요하게 생각하기 때문이다.  


### 분석 
```{r echo=F}
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(dummy)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
```

```{r}
head(train)
summary(train)

train$Response <- factor(train$Response)
```



### feature engineering  
#### dummy variables for Product_info_2  
```{r}
train %>% 
  transform(Product_Info_2_a1 = ifelse(Product_Info_2 == "A1",1,0),
            Product_Info_2_a2 = ifelse(Product_Info_2 == "A2",1,0),
            Product_Info_2_a3 = ifelse(Product_Info_2 == "A3",1,0),
            Product_Info_2_a4 = ifelse(Product_Info_2 == "A4",1,0),
            Product_Info_2_a5 = ifelse(Product_Info_2 == "A5",1,0),
            Product_Info_2_a6 = ifelse(Product_Info_2 == "A6",1,0),
            Product_Info_2_a7 = ifelse(Product_Info_2 == "A7",1,0),
            Product_Info_2_a8 = ifelse(Product_Info_2 == "A8",1,0),
            Product_Info_2_b1 = ifelse(Product_Info_2 == "B1",1,0),
            Product_Info_2_b2 = ifelse(Product_Info_2 == "B2",1,0),
            Product_Info_2_c1 = ifelse(Product_Info_2 == "C1",1,0),
            Product_Info_2_c2 = ifelse(Product_Info_2 == "C2",1,0),
            Product_Info_2_c3 = ifelse(Product_Info_2 == "C3",1,0),
            Product_Info_2_c4 = ifelse(Product_Info_2 == "C4",1,0),
            Product_Info_2_d1 = ifelse(Product_Info_2 == "D1",1,0),
            Product_Info_2_d2 = ifelse(Product_Info_2 == "D2",1,0),
            Product_Info_2_d3 = ifelse(Product_Info_2 == "D3",1,0),
            Product_Info_2_d4 = ifelse(Product_Info_2 == "D4",1,0),
            Product_Info_2_e1 = ifelse(Product_Info_2 == "E1",1,0)) -> train

test %>% 
  transform(Product_Info_2_a1 = ifelse(Product_Info_2 == "A1",1,0),
            Product_Info_2_a2 = ifelse(Product_Info_2 == "A2",1,0),
            Product_Info_2_a3 = ifelse(Product_Info_2 == "A3",1,0),
            Product_Info_2_a4 = ifelse(Product_Info_2 == "A4",1,0),
            Product_Info_2_a5 = ifelse(Product_Info_2 == "A5",1,0),
            Product_Info_2_a6 = ifelse(Product_Info_2 == "A6",1,0),
            Product_Info_2_a7 = ifelse(Product_Info_2 == "A7",1,0),
            Product_Info_2_a8 = ifelse(Product_Info_2 == "A8",1,0),
            Product_Info_2_b1 = ifelse(Product_Info_2 == "B1",1,0),
            Product_Info_2_b2 = ifelse(Product_Info_2 == "B2",1,0),
            Product_Info_2_c1 = ifelse(Product_Info_2 == "C1",1,0),
            Product_Info_2_c2 = ifelse(Product_Info_2 == "C2",1,0),
            Product_Info_2_c3 = ifelse(Product_Info_2 == "C3",1,0),
            Product_Info_2_c4 = ifelse(Product_Info_2 == "C4",1,0),
            Product_Info_2_d1 = ifelse(Product_Info_2 == "D1",1,0),
            Product_Info_2_d2 = ifelse(Product_Info_2 == "D2",1,0),
            Product_Info_2_d3 = ifelse(Product_Info_2 == "D3",1,0),
            Product_Info_2_d4 = ifelse(Product_Info_2 == "D4",1,0),
            Product_Info_2_e1 = ifelse(Product_Info_2 == "E1",1,0)) -> test

## 더 간단하게 더미변수 만들기
Product_Info_2_dummy <- dummy(train$Product_Info_2)
cbind(train,(data.frame(Product_Info_2_dummy)))

Product_Info_2_dummy <- dummy(test$Product_Info_2)
cbind(test,(data.frame(Product_Info_2_dummy)))
```


#### Product_Info_2 - 문자와 숫자의 분류
```{r}
train %>%
  transform(Product_Info_2_cha = factor(substr(Product_Info_2,1,1)),
            Product_Info_2_num = factor(substr(Product_Info_2,2,2)))
train$Product_Info_2 <- factor(train$Product_Info_2)

test %>%
  transform(Product_Info_2_cha = factor(substr(Product_Info_2,1,1)),
            Product_Info_2_num = factor(substr(Product_Info_2,2,2)))
test$Product_Info_2 <- factor(test$Product_Info_2)
```

#### Medical_Keyword_1:48 total variable - 병을 가지고 있는 유뮤의 합(0,1어느 것이 병이 있는 것인지 알 수 없음)
```{r}
train[,paste0("Medical_Keyword_",1:48)] -> Medical_Keyword_dataframe 
apply(Medical_Keyword_dataframe,1,sum)-> train$Medical_Keyword_total

test[,paste0("Medical_Keyword_",1:48)] -> Medical_Keyword_dataframe 
apply(Medical_Keyword_dataframe,1,sum)-> test$Medical_Keyword_total
```


#### BMI_Age  
```{r}
train$Bmi_Age <- train$BMI*train$Ins_Age
test$Bmi_Age <- test$BMI*test$Ins_Age
```


## modeling

## xgboost사용
```{r}
library(xgboost) 
set.seed(1234)

test$Response <- -1

trainLabel <- as.numeric(train$Response) 
testLabel <- as.numeric(test$Response) 
```

```{r}
isNA.train <- sapply(train, function(x){sum(is.na(x))})
names(train)[isNA.train > 0]
train[, names(train)[isNA.train > 0]]

imputation <- function(x){
  index <- which(is.na(x) > 0)
  x[index] <- -1
  return(x)
}

train.noNA <- data.frame(lapply(train, imputation))
test.noNA <- data.frame(lapply(test, imputation))
```


```{r}
train.noNA$Id <- NULL
test.noNA$Id <- NULL
trainMat <- model.matrix(Response ~ 0 + ., data = train.noNA)
testMat <- model.matrix(Response ~ 0 + ., data = test.noNA)
trainLabel <- as.numeric(train$Response) 
testLabel <- as.numeric(train$Response) 
```

```{r}
doParallel::registerDoParallel(4)
params <- list(objective = "reg:linear", 
               eval_metric = "rmse",
               eta = 0.025,
               max_depth = 10,
               min_child_weight = 6,
               subsample = 0.9
)

set.seed(2017)
prudential.xgb.cv <- xgb.cv(params = params,
                            data = trainMat,
                            label = trainLabel,
                            nrounds = 600,
                            nfold = 5,
			                      early_stopping_rounds = 30,
                            print_every_n = 10)
```


```{r}
min.cv <- arrange(prudential.xgb.cv$evaluation_log, test_rmse_mean) %>%
  head(1)

iter <- min.cv$iter
```

```{r}
prudential.xgb <- xgboost(params = params,
                          data = trainMat,
                          label = trainLabel,
                          nrounds = iter,
                          print_every_n = 10)
```

```{r}
prudential.predict <- predict(prudential.xgb, testMat)
prudential.predict <- round(prudential.predict)
prudential.predict[prudential.predict <= 0] <- 1
```

```{r}
submission <- data.frame(Id = test$Id,
                         Response = prudential.predict)
write_csv(submission, "submission_2.csv")
```

##### score 0.622



### 중요 변수 선정
```{r}
library(Boruta)

set.seed(7)
bor.result <- Boruta(train.noNA, trainLabel, maxRuns = )
getSelectedAttributes(bor.result)
```

```{r}
plot(bor.result)
```

```{r}
train.Bor <- train.noNA[, getSelectedAttributes(bor.result)]
test.Bor <- test.noNA[,getSelectedAttributes(bor.result)]
```

```{r}
train.Bor.Mat <- as.matrix(train.Bor)
test.Bor.Mat <- as.matrix(test.Bor)
```



#################################################################################
#############################      Reference      ###############################

#1. https://www.kaggle.com/c/prudential-life-insurance-assessment/discussion/19010
#2. https://www.kaggle.com/c/prudential-life-insurance-assessment/discussion/19003
#3. https://www.kaggle.com/bobcz3/prudential-life-insurance-assessment/t-sne-for-prudential




