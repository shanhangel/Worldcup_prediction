世界杯结果预测
========================================================
## By Casper

*Just for fun*

基本思路：世界杯比赛正如火如荼，各路大神开始为比赛作分析预测，预测的方法多种多样，但是基本可分为以足球评论员为代表的基于足球知识预测，以及以章鱼哥为代表的随机性预测。本文采用的是机器学习的方法，以历史世界杯数据作为training data，以淘汰赛两队比赛胜负为预测对象，小组赛表现为预测因子，构建多个模型，用上届世杯的数据作为validation，筛选出最优模型，对本届世界杯的淘汰赛结果进行预测，灵感来自于Kaggle的March Machine Learning Mania比赛对NCAA结果的预测


先上预测结果图

![title](path/to/your/image)


## 1. Getting the data
数据来源为FIFA官网，抓取1966-2010共十一届世界杯的历史数据网页，通过解析html文件，获得包含小组赛胜场数、负场数、平局数、进球数、被进球数、积分信息，再结合淘汰赛的胜负结果，生成表格

![fifa_web](fifa_web截图.png)


```r
library(XML)
Url_2010 <- "http://www.fifa.com/tournaments/archive/worldcup/southafrica2010/matches/index.html"
Url_2006 <- "http://www.fifa.com/tournaments/archive/worldcup/germany2006/matches/index.html"
Url_2002 <- "http://www.fifa.com/tournaments/archive/worldcup/koreajapan2002/matches/index.html"
Url_1998 <- "http://www.fifa.com/tournaments/archive/worldcup/france1998/matches/index.html"
Url_1994 <- "http://www.fifa.com/tournaments/archive/worldcup/usa1994/matches/index.html"
Url_1990 <- "http://www.fifa.com/tournaments/archive/worldcup/italy1990/matches/index.html"
Url_1986 <- "http://www.fifa.com/tournaments/archive/worldcup/mexico1986/matches/index.html"
Url_1982 <- "http://www.fifa.com/tournaments/archive/worldcup/spain1982/matches/index.html"
Url_1978 <- "http://www.fifa.com/tournaments/archive/worldcup/argentina1978/matches/index.html"
Url_1974 <- "http://www.fifa.com/tournaments/archive/worldcup/germany1974/matches/index.html"
Url_1970 <- "http://www.fifa.com/tournaments/archive/worldcup/mexico1970/matches/index.html"
Url_1966 <- "http://www.fifa.com/tournaments/archive/worldcup/england1966/matches/index.html"

web_2010 <- htmlTreeParse(Url_2010, useInternal=TRUE)
```



## 2. Clean the data
### 2.1 以2010年世界杯结果为testing dataset

获得小组赛结果

```r
score_2010 <- xpathSApply(web_2010, "//td[@class='c']", xmlValue)
team_name <- xpathSApply(web_2010, "//td[@class='l']", xmlValue)
```

根据表格看出有7列数据

```r
gpresult_2010_1 <- matrix(score_2010, ncol=7, byrow=TRUE) 
gpresult_2010_2 <- cbind(team_name, gpresult_2010_1)
colnames(gpresult_2010_2) <- c("Team", "Played", "Won", "Draw", "Lost", 
"Goals For", "Goals Against", "Points")
```

获得淘汰赛结果，小组赛共有48场，淘汰赛16场，共64场，这里需要第49-64场的数据

```r
home_team <- xpathSApply(web_2010, "//td[@class='l homeTeam']", 
                         xmlValue)[49:64]
away_team <- xpathSApply(web_2010, "//td[@class='r awayTeam']",
                         xmlValue)[49:64]
```


```r
##注意淘汰赛中的结果只有根据比分来判别，取第49-64场的比分数据，Node是
##<td style="width:120px" class="c ">， c后面多了一个空格
result <- xpathSApply(web_2010, "// td[@class='c ']", xmlValue)[49:64]
```


根据比分解析胜负结果，括号里面是点球结果，PSO表示点球大战。判定：如果没有点球决胜，每项的第一个元素为胜负依据，如果有点球决胜，第三个元素作为依据

```r
result1 <- as.character(result)
split <- strsplit(result1," ")
result2 <- list()
for (i in 1:16){
    if (regexpr("PSO", result1[[i]])>0){
       result2[i] <- split[[i]][length(split[[i]])-1] 
    }
    else{
        result2[i] <- split[[i]][1]
    }
}

split1 <- strsplit(as.character(result2),":")
result3 <- list()
for (i in 1:16){
    if (split1[[i]][1]>split1[[i]][2]){
       result3[i]=1 
    }
    else{
        result3[i]=0
    }
}

eliresult_2010 <- data.frame(home_team=home_team, away_team=away_team, 
                             result=result,result3=as.numeric(result3))
```

组合小组赛和淘汰赛数据，以team作为index

![数据组合](数据组合.png)


```r
final_result_2010 <- merge(eliresult_2010, gpresult_2010_2, by.x="home_team", 
                      by.y="Team", sort=TRUE)                                                             
final_result_2010 <- merge(final_result_2010, gpresult_2010_2, by.x="away_team", 
                      by.y="Team", sort=FALSE)
final_result_2010 <- data.frame(final_result_2010[,2], final_result_2010[,1],
                                final_result_2010[,4:18])

final_result_2010[18] <- "2010"
colnames(final_result_2010) <- c("Home", "Away", "Result", "Played_Home", 
                                 "Won_Home", "Draw_Home", "Lost_Home", 
                                 "Goals_For_Home", "Goals_Against_Home", 
                                 "Point_Home","Played_Away", "Won_Away",
                                 "Draw_Away", "Lost_Away", "Goals_For_Away",
                                 "Goals_Against_Away", "Point_Away", "Year")
write.csv(final_result_2010,"./test.csv")
```


### 2.2 以1966-2006年世界杯结果为training dataset
进行过程中发现一些问题，1998-2010年世界杯的参赛队伍为32支，比赛为64场，其中淘汰赛为第49-64场；1982-1994年参赛队伍为24支，比赛为52场，其中淘汰赛为第37-52场；196-1978年参赛队伍为16支，比赛为38场，发现严重问题，78年以前小组赛分为两轮，第一轮晋级的，进入第二轮小组赛，继续比赛争出现，因此无法利用小组赛信息作为淘汰赛的预测因素，因此training data只采用1982-2006年的数据。 %>_<%

建立function，以处理testing data的方法，建立training dataset

```r
create_train_data <- function(Url){
    
    web <- htmlTreeParse(Url, useInternal=TRUE)
    year <- substr(Url, nchar(Url)-22, nchar(Url)-19)
    
    if (Url%in%c(Url_2006, Url_2002, Url_1998)){
        emi_index <- 49:64
    }
    else if (Url%in%c(Url_1994, Url_1990, Url_1986)){
        emi_index <- 37:52
    }
         
    score <- xpathSApply(web, "//td[@class='c']", xmlValue)
    team_name <- xpathSApply(web, "//td[@class='l']", xmlValue)
    
    gpresult_1 <- matrix(score, ncol=7, byrow=TRUE) 
    gpresult_2 <- cbind(team_name, gpresult_1)
    colnames(gpresult_2) <- c("Team", "Played", "Won", "Draw", "Lost", 
                                   "Goals For", "Goals Against", "Points")
    
    home_team <- xpathSApply(web, "//td[@class='l homeTeam']", 
                             xmlValue)[emi_index]
    away_team <- xpathSApply(web, "//td[@class='r awayTeam']",
                             xmlValue)[emi_index]
    
    result <- xpathSApply(web, "// td[@class='c ']", xmlValue)[emi_index]

    result1 <- as.character(result)
    split <- strsplit(result1," ")
    result2 <- list()
    for (i in 1:16){
        if (regexpr("PSO", result1[[i]])>0){
            result2[i] <- split[[i]][length(split[[i]])-1] 
        }
        else{
            result2[i] <- split[[i]][1]
        }
    }
    
    split1 <- strsplit(as.character(result2),":")
    result3 <- list()
    for (i in 1:16){
        if (split1[[i]][1]>split1[[i]][2]){
            result3[i]=1 
        }
        else{
            result3[i]=0
        }
    }
    
    eliresult <- data.frame(home_team=home_team, away_team=away_team, 
                                 result=result,result3=as.numeric(result3))
    
    final_result <- merge(eliresult, gpresult_2, by.x="home_team", 
                               by.y="Team", sort=TRUE)                                                             
    final_result <- merge(final_result, gpresult_2, by.x="away_team", 
                               by.y="Team", sort=FALSE)
    final_result <- data.frame(final_result[,2], final_result[,1],
                               final_result[,4:18])
    
    final_result[18] <- year
    
    colnames(final_result) <- c("Home", "Away", "Result", "Played_Home", 
                                "Won_Home", "Draw_Home", "Lost_Home", 
                                "Goals_For_Home", "Goals_Against_Home", 
                                "Point_Home", "Played_Away", "Won_Away", 
                                "Draw_Away", "Lost_Away", "Goals_For_Away", 
                                "Goals_Against_Away", "Point_Away", "Year")
    filename=paste("training","_", year, sep="")
    write.csv(final_result, paste(filename,".csv",
                                  sep=""))
    
}

Whole_Url <- c(Url_2006, Url_2002, Url_1998, Url_1994, Url_1990, Url_1986)

for (i in 1:6){
    create_train_data(Whole_Url[i])
}

training_1986 <- read.csv("./training_1986.csv")
training_1990 <- read.csv("./training_1990.csv")
training_1994 <- read.csv("./training_1994.csv")
training_1998 <- read.csv("./training_1998.csv")
training_2002 <- read.csv("./training_2002.csv")
training_2006 <- read.csv("./training_2006.csv")

training <- rbind(training_1986, training_1990, training_1994, training_1998,
                  training_2002, training_2006)

write.csv(training, "training.csv")
```


## 3. Data Analysis

```r
test <- read.csv("test.csv")

train_data <- data.frame(Result=as.factor(training$Result), 
                         Played_Home=as.numeric(training$Played_Home),
                         Won_Home=as.numeric(training$Won_Home),
                         Draw_Home=as.numeric(training$Draw_Home),
                         Lost_Home=as.numeric(training$Lost_Home),
                         Goals_For_Home=as.numeric(training$Goals_For_Home),
                         Goals_Against_Home=as.numeric(training$Goals_Against_Home),
                         Point_Home=as.numeric(training$Point_Home),
                         Played_Away=as.numeric(training$Played_Away),
                         Won_Away=as.numeric(training$Won_Away),
                         Draw_Away=as.numeric(training$Draw_Away),
                         Lost_Away=as.numeric(training$Lost_Away),
                         Goals_For_Away=as.numeric(training$Goals_For_Away),
                         Goals_Against_Away=as.numeric(training$Goals_Against_Away),
                         Point_Away=as.numeric(training$Point_Away))
```

### 3.1 用SVD检查哪个因素对结果影响最大

```r
svd1 <- svd(train_data[,c(3,4,6,7,10,11,13,14)])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

可以看出各个因素中的共线性非常严重，分析原因如下，每队的出场数是一定的，胜负平三场的总数是一定的，胜负结果确定以后积分也是一定的，因此，需要排除出场数"Played"，负场数"Lost_Home"和"Lost_Away"，以及小组积分"Point_Home"和"Point_Away"

用train_data建立prediction model，用test进行模型筛选

### 3.2 Try Logistic Regression Model

```r
fit_LR <- glm(Result ~ Won_Home + Goals_For_Home 
              + Goals_Against_Home + Point_Home + Won_Away + Draw_Away 
              + Goals_For_Away + Goals_Against_Away, data=train_data, 
              family= "binomial")

prediction_LR <- predict(fit_LR, test)
prediction_LR[prediction_LR<0.5] <- 0
prediction_LR[prediction_LR>=0.5] <- 1
summary(fit_LR)
```

```
## 
## Call:
## glm(formula = Result ~ Won_Home + Goals_For_Home + Goals_Against_Home + 
##     Point_Home + Won_Away + Draw_Away + Goals_For_Away + Goals_Against_Away, 
##     family = "binomial", data = train_data)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.952  -1.127   0.681   0.980   1.646  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)  
## (Intercept)         -0.2239     2.0760   -0.11    0.914  
## Won_Home            -0.5125     0.5479   -0.94    0.350  
## Goals_For_Home       0.2816     0.1311    2.15    0.032 *
## Goals_Against_Home  -0.1412     0.1904   -0.74    0.458  
## Point_Home           0.0174     0.2264    0.08    0.939  
## Won_Away             0.4214     0.8685    0.49    0.628  
## Draw_Away            0.4352     0.5700    0.76    0.445  
## Goals_For_Away      -0.3758     0.2009   -1.87    0.061 .
## Goals_Against_Away   0.3069     0.2394    1.28    0.200  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 129.69  on 95  degrees of freedom
## Residual deviance: 116.74  on 87  degrees of freedom
## AIC: 134.7
## 
## Number of Fisher Scoring iterations: 4
```

### 3.3 Try Decision Tree Model

```r
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

fit_DT <- rpart(Result ~ Won_Home + Goals_For_Home 
                + Goals_Against_Home + Point_Home + Won_Away + Draw_Away 
                + Goals_For_Away + Goals_Against_Away, data=train_data, 
                method="class")

prediction_DT <- predict(fit_DT, test, type="class")


fancyRpartPlot(fit_DT)
```

```
## Warning: conversion failure on 'Rattle 2014-鍏湀-12 13:16:37 huangshan' in 'mbcsToSbcs': dot substituted for <e5>
## Warning: conversion failure on 'Rattle 2014-鍏湀-12 13:16:37 huangshan' in 'mbcsToSbcs': dot substituted for <85>
## Warning: conversion failure on 'Rattle 2014-鍏湀-12 13:16:37 huangshan' in 'mbcsToSbcs': dot substituted for <ad>
## Warning: conversion failure on 'Rattle 2014-鍏湀-12 13:16:37 huangshan' in 'mbcsToSbcs': dot substituted for <e6>
## Warning: conversion failure on 'Rattle 2014-鍏湀-12 13:16:37 huangshan' in 'mbcsToSbcs': dot substituted for <9c>
## Warning: conversion failure on 'Rattle 2014-鍏湀-12 13:16:37 huangshan' in 'mbcsToSbcs': dot substituted for <88>
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
summary(fit_DT)
```

```
## Call:
## rpart(formula = Result ~ Won_Home + Goals_For_Home + Goals_Against_Home + 
##     Point_Home + Won_Away + Draw_Away + Goals_For_Away + Goals_Against_Away, 
##     data = train_data, method = "class")
##   n= 96 
## 
##        CP nsplit rel error xerror   xstd
## 1 0.10256      0    1.0000  1.000 0.1234
## 2 0.02564      3    0.6923  1.077 0.1246
## 3 0.01000      6    0.6154  1.128 0.1252
## 
## Variable importance
##     Goals_For_Home     Goals_For_Away Goals_Against_Away 
##                 34                 18                 15 
##         Point_Home           Won_Home Goals_Against_Home 
##                 12                  9                  7 
##           Won_Away 
##                  4 
## 
## Node number 1: 96 observations,    complexity param=0.1026
##   predicted class=1  expected loss=0.4062  P(node) =1
##     class counts:    39    57
##    probabilities: 0.406 0.594 
##   left son=2 (46 obs) right son=3 (50 obs)
##   Primary splits:
##       Goals_For_Home     < 5.5  to the left,  improve=2.3560, (0 missing)
##       Won_Away           < 2.5  to the right, improve=1.4330, (0 missing)
##       Goals_For_Away     < 4.5  to the right, improve=1.2720, (0 missing)
##       Goals_Against_Away < 1.5  to the left,  improve=1.1740, (0 missing)
##       Draw_Away          < 1.5  to the left,  improve=0.8403, (0 missing)
##   Surrogate splits:
##       Point_Home         < 4.5  to the left,  agree=0.615, adj=0.196, (0 split)
##       Goals_Against_Away < 2.5  to the right, agree=0.615, adj=0.196, (0 split)
##       Goals_Against_Home < 1.5  to the left,  agree=0.594, adj=0.152, (0 split)
##       Won_Home           < 1.5  to the left,  agree=0.583, adj=0.130, (0 split)
##       Goals_For_Away     < 2.5  to the left,  agree=0.542, adj=0.043, (0 split)
## 
## Node number 2: 46 observations,    complexity param=0.1026
##   predicted class=0  expected loss=0.4783  P(node) =0.4792
##     class counts:    24    22
##    probabilities: 0.522 0.478 
##   left son=4 (38 obs) right son=5 (8 obs)
##   Primary splits:
##       Goals_For_Away     < 2.5  to the right, improve=3.0490, (0 missing)
##       Goals_For_Home     < 3.5  to the right, improve=1.2570, (0 missing)
##       Goals_Against_Home < 1.5  to the right, improve=1.0700, (0 missing)
##       Point_Home         < 4.5  to the right, improve=0.7228, (0 missing)
##       Goals_Against_Away < 3.5  to the right, improve=0.2727, (0 missing)
##   Surrogate splits:
##       Won_Away           < 0.5  to the right, agree=0.870, adj=0.250, (0 split)
##       Goals_Against_Home < 0.5  to the right, agree=0.848, adj=0.125, (0 split)
## 
## Node number 3: 50 observations,    complexity param=0.02564
##   predicted class=1  expected loss=0.3  P(node) =0.5208
##     class counts:    15    35
##    probabilities: 0.300 0.700 
##   left son=6 (30 obs) right son=7 (20 obs)
##   Primary splits:
##       Goals_Against_Away < 2.5  to the left,  improve=2.6670, (0 missing)
##       Won_Away           < 1.5  to the right, improve=1.1970, (0 missing)
##       Goals_For_Home     < 9.5  to the left,  improve=1.0000, (0 missing)
##       Draw_Away          < 1.5  to the left,  improve=0.7505, (0 missing)
##       Goals_For_Away     < 6.5  to the right, improve=0.2691, (0 missing)
##   Surrogate splits:
##       Goals_For_Home < 10.5 to the left,  agree=0.68, adj=0.20, (0 split)
##       Goals_For_Away < 4.5  to the left,  agree=0.64, adj=0.10, (0 split)
##       Won_Away       < 0.5  to the right, agree=0.62, adj=0.05, (0 split)
## 
## Node number 4: 38 observations,    complexity param=0.1026
##   predicted class=0  expected loss=0.3947  P(node) =0.3958
##     class counts:    23    15
##    probabilities: 0.605 0.395 
##   left son=8 (28 obs) right son=9 (10 obs)
##   Primary splits:
##       Goals_For_Home     < 3.5  to the right, improve=2.5290, (0 missing)
##       Point_Home         < 4.5  to the right, improve=1.8080, (0 missing)
##       Goals_For_Away     < 4.5  to the right, improve=0.9521, (0 missing)
##       Goals_Against_Home < 1.5  to the right, improve=0.7579, (0 missing)
##       Won_Home           < 1.5  to the right, improve=0.3887, (0 missing)
##   Surrogate splits:
##       Point_Home < 4.5  to the right, agree=0.921, adj=0.7, (0 split)
##       Won_Home   < 1.5  to the right, agree=0.895, adj=0.6, (0 split)
## 
## Node number 5: 8 observations
##   predicted class=1  expected loss=0.125  P(node) =0.08333
##     class counts:     1     7
##    probabilities: 0.125 0.875 
## 
## Node number 6: 30 observations,    complexity param=0.02564
##   predicted class=1  expected loss=0.4333  P(node) =0.3125
##     class counts:    13    17
##    probabilities: 0.433 0.567 
##   left son=12 (21 obs) right son=13 (9 obs)
##   Primary splits:
##       Goals_For_Home     < 8.5  to the left,  improve=1.1460, (0 missing)
##       Draw_Away          < 1.5  to the left,  improve=0.7333, (0 missing)
##       Point_Home         < 5.5  to the right, improve=0.5333, (0 missing)
##       Won_Away           < 1.5  to the right, improve=0.3048, (0 missing)
##       Goals_Against_Away < 1.5  to the left,  improve=0.2333, (0 missing)
## 
## Node number 7: 20 observations
##   predicted class=1  expected loss=0.1  P(node) =0.2083
##     class counts:     2    18
##    probabilities: 0.100 0.900 
## 
## Node number 8: 28 observations
##   predicted class=0  expected loss=0.2857  P(node) =0.2917
##     class counts:    20     8
##    probabilities: 0.714 0.286 
## 
## Node number 9: 10 observations
##   predicted class=1  expected loss=0.3  P(node) =0.1042
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 12: 21 observations,    complexity param=0.02564
##   predicted class=0  expected loss=0.4762  P(node) =0.2188
##     class counts:    11    10
##    probabilities: 0.524 0.476 
##   left son=24 (7 obs) right son=25 (14 obs)
##   Primary splits:
##       Goals_Against_Home < 2.5  to the right, improve=0.7619, (0 missing)
##       Goals_For_Home     < 6.5  to the left,  improve=0.2216, (0 missing)
##       Goals_Against_Away < 1.5  to the left,  improve=0.2216, (0 missing)
##       Goals_For_Away     < 4.5  to the left,  improve=0.1984, (0 missing)
##       Won_Home           < 2.5  to the left,  improve=0.1905, (0 missing)
##   Surrogate splits:
##       Goals_For_Home < 6.5  to the left,  agree=0.762, adj=0.286, (0 split)
##       Point_Home     < 4.5  to the left,  agree=0.762, adj=0.286, (0 split)
##       Goals_For_Away < 5.5  to the right, agree=0.762, adj=0.286, (0 split)
## 
## Node number 13: 9 observations
##   predicted class=1  expected loss=0.2222  P(node) =0.09375
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 24: 7 observations
##   predicted class=0  expected loss=0.2857  P(node) =0.07292
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 25: 14 observations
##   predicted class=1  expected loss=0.4286  P(node) =0.1458
##     class counts:     6     8
##    probabilities: 0.429 0.571
```

### 3.4 Try Random Forest Model

```r
library(randomForest)
fit_RF <- randomForest(Result ~ Won_Home + Goals_For_Home 
                       + Goals_Against_Home + Point_Home + Won_Away + Draw_Away 
                       + Goals_For_Away + Goals_Against_Away, data=train_data, 
                       importance=TRUE, ntree=100)
prediction_RF <- predict(fit_RF, test)
summary(fit_RF)
```

```
##                 Length Class  Mode     
## call              5    -none- call     
## type              1    -none- character
## predicted        96    factor numeric  
## err.rate        300    -none- numeric  
## confusion         6    -none- numeric  
## votes           192    matrix numeric  
## oob.times        96    -none- numeric  
## classes           2    -none- character
## importance       32    -none- numeric  
## importanceSD     24    -none- numeric  
## localImportance   0    -none- NULL     
## proximity         0    -none- NULL     
## ntree             1    -none- numeric  
## mtry              1    -none- numeric  
## forest           14    -none- list     
## y                96    factor numeric  
## test              0    -none- NULL     
## inbag             0    -none- NULL     
## terms             3    terms  call
```

### 3.5 结果比较
因为training data量比较小，为了避免出现overfit，在这里采用Logistic Regression Model，预测模型对validation数据的正确率为75%

```r
model_check <- data.frame(test$Result, prediction_LR, prediction_DT, prediction_RF)

library(knitr)
kable(model_check, format = "markdown")
```

```
## 
## 
## | test.Result| prediction_LR|prediction_DT |prediction_RF |
## |-----------:|-------------:|:-------------|:-------------|
## |           1|             1|1             |1             |
## |           0|             0|1             |1             |
## |           0|             0|0             |0             |
## |           1|             0|0             |0             |
## |           1|             1|1             |1             |
## |           0|             0|0             |0             |
## |           0|             0|1             |1             |
## |           0|             0|0             |1             |
## |           1|             1|0             |1             |
## |           1|             0|0             |1             |
## |           1|             0|1             |1             |
## |           1|             0|0             |0             |
## |           1|             1|0             |0             |
## |           1|             1|1             |1             |
## |           0|             1|1             |1             |
## |           0|             0|0             |0             |
```

## 4. Result


## 5. Future Work
本文利用了世界杯历史数据作为training data，以小组赛表现为Predictor，以淘汰赛结果为Output，从Logistic Regression, Decision Tree和Random Forest三种模型中筛选了对test data结果最好的Logistic Regression用在今年世界杯的小组赛数据上作为淘汰赛的预测。

但是本预测也有很多问题需要解决：首先，training data的量严重不足，这是因为世界杯历史比赛量比较小，以后可以再增加欧洲杯、亚洲杯等有淘汰赛的比赛作为training，其次，Variables偏少，不多的Variables共线性严重，从足球的角度来讲，可以参考的Predictor有控球率，射门数，犯规数，传球数等等。
