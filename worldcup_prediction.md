���籭���Ԥ��
========================================================
## By Casper

*Just for fun*

����˼·�����籭�����������ݱ����·����ʼΪ����������Ԥ�⣬Ԥ��ķ������ֶ��������ǻ����ɷ�Ϊ����������ԱΪ����Ļ�������֪ʶԤ�⣬�Լ��������Ϊ����������Ԥ�⡣���Ĳ��õ��ǻ���ѧϰ�ķ���������ʷ���籭������Ϊtraining data������̭�����ӱ���ʤ��ΪԤ�����С��������ΪԤ�����ӣ��������ģ�ͣ����Ͻ�������������Ϊvalidation��ɸѡ������ģ�ͣ��Ա������籭����̭���������Ԥ�⣬���������Kaggle��March Machine Learning Mania������NCAA�����Ԥ��


����Ԥ����ͼ(Jun.18 Prediction)

![final_result](final_prediction.png)


## 1. Getting the data
������ԴΪFIFA������ץȡ1966-2010��ʮһ�����籭����ʷ������ҳ��ͨ������html�ļ�����ð���С����ʤ��������������ƽ������������������������������Ϣ���ٽ����̭����ʤ����������ɱ��

![fifa_web](fifa_web��ͼ.png)


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
### 2.1 ��2010�����籭���Ϊtesting dataset

���С�������

```r
score_2010 <- xpathSApply(web_2010, "//td[@class='c']", xmlValue)
team_name <- xpathSApply(web_2010, "//td[@class='l']", xmlValue)
```

���ݱ�񿴳���7������

```r
gpresult_2010_1 <- matrix(score_2010, ncol=7, byrow=TRUE) 
gpresult_2010_2 <- cbind(team_name, gpresult_2010_1)
colnames(gpresult_2010_2) <- c("Team", "Played", "Won", "Draw", "Lost", 
"Goals For", "Goals Against", "Points")
```

�����̭�������С��������48������̭��16������64����������Ҫ��49-64��������

```r
home_team <- xpathSApply(web_2010, "//td[@class='l homeTeam']", 
                         xmlValue)[49:64]
away_team <- xpathSApply(web_2010, "//td[@class='r awayTeam']",
                         xmlValue)[49:64]
```


```r
##ע����̭���еĽ��ֻ�и��ݱȷ����б�ȡ��49-64���ıȷ����ݣ�Node��
##<td style="width:120px" class="c ">�� c�������һ���ո�
result <- xpathSApply(web_2010, "// td[@class='c ']", xmlValue)[49:64]
```


���ݱȷֽ���ʤ����������������ǵ�������PSO��ʾ�����ս���ж������û�е����ʤ��ÿ��ĵ�һ��Ԫ��Ϊʤ�����ݣ�����е����ʤ��������Ԫ����Ϊ����

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

���С��������̭�����ݣ���team��Ϊindex

![�������](�������.png)


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


```r
library(knitr)
kable(final_result_2010, format = "markdown")
```

```
## 
## 
## |Home        |Away           | Result|Played_Home |Won_Home |Draw_Home |Lost_Home |Goals_For_Home |Goals_Against_Home |Point_Home |Played_Away |Won_Away |Draw_Away |Lost_Away |Goals_For_Away |Goals_Against_Away |Point_Away |Year |
## |:-----------|:--------------|------:|:-----------|:--------|:---------|:---------|:--------------|:------------------|:----------|:-----------|:--------|:---------|:---------|:--------------|:------------------|:----------|:----|
## |Argentina   |Mexico         |      1|3           |3        |0         |0         |7              |1                  |9          |3           |1        |1         |1         |3              |2                  |4          |2010 |
## |Argentina   |Germany        |      0|3           |3        |0         |0         |7              |1                  |9          |3           |2        |0         |1         |5              |1                  |6          |2010 |
## |Uruguay     |Germany        |      0|3           |2        |1         |0         |4              |0                  |7          |3           |2        |0         |1         |5              |1                  |6          |2010 |
## |Brazil      |Chile          |      1|3           |2        |1         |0         |5              |2                  |7          |3           |2        |0         |1         |3              |2                  |6          |2010 |
## |Germany     |England        |      1|3           |2        |0         |1         |5              |1                  |6          |3           |1        |2         |0         |2              |1                  |5          |2010 |
## |Germany     |Spain          |      0|3           |2        |0         |1         |5              |1                  |6          |3           |2        |0         |1         |4              |2                  |6          |2010 |
## |Paraguay    |Spain          |      0|3           |1        |2         |0         |3              |1                  |5          |3           |2        |0         |1         |4              |2                  |6          |2010 |
## |Netherlands |Spain          |      0|3           |3        |0         |0         |5              |1                  |9          |3           |2        |0         |1         |4              |2                  |6          |2010 |
## |Netherlands |Slovakia       |      1|3           |3        |0         |0         |5              |1                  |9          |3           |1        |1         |1         |4              |5                  |4          |2010 |
## |Netherlands |Brazil         |      1|3           |3        |0         |0         |5              |1                  |9          |3           |2        |1         |0         |5              |2                  |7          |2010 |
## |Paraguay    |Japan          |      1|3           |1        |2         |0         |3              |1                  |5          |3           |2        |0         |1         |4              |2                  |6          |2010 |
## |Spain       |Portugal       |      1|3           |2        |0         |1         |4              |2                  |6          |3           |1        |2         |0         |7              |0                  |5          |2010 |
## |Uruguay     |Korea Republic |      1|3           |2        |1         |0         |4              |0                  |7          |3           |1        |1         |1         |5              |6                  |4          |2010 |
## |Uruguay     |Ghana          |      1|3           |2        |1         |0         |4              |0                  |7          |3           |1        |1         |1         |2              |2                  |4          |2010 |
## |USA         |Ghana          |      0|3           |1        |2         |0         |4              |3                  |5          |3           |1        |1         |1         |2              |2                  |4          |2010 |
## |Uruguay     |Netherlands    |      0|3           |2        |1         |0         |4              |0                  |7          |3           |3        |0         |0         |5              |1                  |9          |2010 |
```

### 2.2 ��1966-2006�����籭���Ϊtraining dataset
���й����з���һЩ���⣬1998-2010�����籭�Ĳ�������Ϊ32֧������Ϊ64����������̭��Ϊ��49-64����1982-1994���������Ϊ24֧������Ϊ52����������̭��Ϊ��37-52����1966-1978���������Ϊ16֧������Ϊ38�������ص���������ˣ�78����ǰС������Ϊ���֣���һ�ֽ����ģ�����ڶ���С���������������ٴ���С����֣�����޷�����С������Ϣ��Ϊ��̭����Ԥ�����ء����Ա��ĵ�training dataֻ����1986-2006������ݡ� %>_<%

����function���Դ���testing data�ķ���������training dataset

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

### 3.1 ��SVD����ĸ����ضԽ��Ӱ�����

```r
svd1 <- svd(train_data[,c(3,4,6,7,10,11,13,14)])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

���Կ������������еĹ����Էǳ����أ�����ԭ�����£�ÿ�ӵĳ�������һ���ģ�ʤ��ƽ������������һ���ģ�ʤ�����ȷ���Ժ����Ҳ��һ���ģ���ˣ���Ҫ�ų�������"Played"��������"Lost_Home"��"Lost_Away"���Լ�С�����"Point_Home"��"Point_Away"

��train_data����prediction model����test����ģ��ɸѡ

### 3.2 Try Logistic Regression Model

```r
fit_LR <- glm(Result ~ Won_Home + Draw_Home + Goals_For_Home 
              + Goals_Against_Home + Won_Away + Draw_Away 
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
## glm(formula = Result ~ Won_Home + Draw_Home + Goals_For_Home + 
##     Goals_Against_Home + Won_Away + Draw_Away + Goals_For_Away + 
##     Goals_Against_Away, family = "binomial", data = train_data)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.977  -1.114   0.679   0.959   1.661  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)  
## (Intercept)           0.296      2.919    0.10     0.92  
## Won_Home             -0.665      0.851   -0.78     0.44  
## Draw_Home            -0.163      0.670   -0.24     0.81  
## Goals_For_Home        0.296      0.137    2.17     0.03 *
## Goals_Against_Home   -0.172      0.227   -0.76     0.45  
## Won_Away              0.428      0.860    0.50     0.62  
## Draw_Away             0.432      0.567    0.76     0.45  
## Goals_For_Away       -0.376      0.200   -1.88     0.06 .
## Goals_Against_Away    0.305      0.239    1.27     0.20  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 129.69  on 95  degrees of freedom
## Residual deviance: 116.68  on 87  degrees of freedom
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

fit_DT <- rpart(Result ~ Won_Home + Draw_Home + Goals_For_Home 
                + Goals_Against_Home + Won_Away + Draw_Away 
                + Goals_For_Away + Goals_Against_Away, data=train_data, 
                method="class")

prediction_DT <- predict(fit_DT, test, type="class")


fancyRpartPlot(fit_DT)
```

```
## Warning: conversion failure on 'Rattle 2014-六月-18 16:31:40 huangshan' in 'mbcsToSbcs': dot substituted for <e5>
## Warning: conversion failure on 'Rattle 2014-六月-18 16:31:40 huangshan' in 'mbcsToSbcs': dot substituted for <85>
## Warning: conversion failure on 'Rattle 2014-六月-18 16:31:40 huangshan' in 'mbcsToSbcs': dot substituted for <ad>
## Warning: conversion failure on 'Rattle 2014-六月-18 16:31:40 huangshan' in 'mbcsToSbcs': dot substituted for <e6>
## Warning: conversion failure on 'Rattle 2014-六月-18 16:31:40 huangshan' in 'mbcsToSbcs': dot substituted for <9c>
## Warning: conversion failure on 'Rattle 2014-六月-18 16:31:40 huangshan' in 'mbcsToSbcs': dot substituted for <88>
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r
summary(fit_DT)
```

```
## Call:
## rpart(formula = Result ~ Won_Home + Draw_Home + Goals_For_Home + 
##     Goals_Against_Home + Won_Away + Draw_Away + Goals_For_Away + 
##     Goals_Against_Away, data = train_data, method = "class")
##   n= 96 
## 
##        CP nsplit rel error xerror   xstd
## 1 0.10256      0    1.0000  1.000 0.1234
## 2 0.02564      3    0.6923  1.026 0.1239
## 3 0.01000      6    0.6154  1.103 0.1249
## 
## Variable importance
##     Goals_For_Home     Goals_For_Away Goals_Against_Away 
##                 36                 19                 17 
##           Won_Home Goals_Against_Home          Draw_Home 
##                 10                  8                  5 
##           Won_Away 
##                  5 
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
##       Goals_Against_Away < 2.5  to the right, agree=0.615, adj=0.196, (0 split)
##       Goals_Against_Home < 1.5  to the left,  agree=0.594, adj=0.152, (0 split)
##       Won_Home           < 1.5  to the left,  agree=0.583, adj=0.130, (0 split)
##       Draw_Home          < 0.5  to the right, agree=0.552, adj=0.065, (0 split)
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
##       Goals_Against_Away < 3.5  to the right, improve=0.2727, (0 missing)
##       Draw_Home          < 1.5  to the right, improve=0.2065, (0 missing)
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
##       Draw_Home          < 0.5  to the left,  improve=0.5192, (0 missing)
##   Surrogate splits:
##       Goals_For_Home < 10.5 to the left,  agree=0.68, adj=0.20, (0 split)
##       Draw_Home      < 0.5  to the left,  agree=0.64, adj=0.10, (0 split)
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
##       Goals_For_Away     < 4.5  to the right, improve=0.9521, (0 missing)
##       Goals_Against_Home < 1.5  to the right, improve=0.7579, (0 missing)
##       Draw_Home          < 0.5  to the left,  improve=0.7348, (0 missing)
##       Won_Home           < 1.5  to the right, improve=0.3887, (0 missing)
##   Surrogate splits:
##       Won_Home  < 1.5  to the right, agree=0.895, adj=0.6, (0 split)
##       Draw_Home < 1.5  to the left,  agree=0.789, adj=0.2, (0 split)
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
##       Draw_Home          < 0.5  to the left,  improve=0.4000, (0 missing)
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
fit_RF <- randomForest(Result ~ Won_Home + Draw_Home + Goals_For_Home 
                       + Goals_Against_Home + Won_Away + Draw_Away 
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

### 3.5 ����Ƚ�
��Ϊtraining data���Ƚ�С��Ϊ�˱������overfit�����������Logistic Regression Model��Ԥ��ģ�Ͷ�validation���ݵ���ȷ��Ϊ75%

```r
model_check <- data.frame(test$Result, prediction_LR, prediction_DT, prediction_RF)

kable(model_check, format = "markdown")
```

```
## 
## 
## | test.Result| prediction_LR|prediction_DT |prediction_RF |
## |-----------:|-------------:|:-------------|:-------------|
## |           1|             1|1             |0             |
## |           0|             0|1             |1             |
## |           0|             0|0             |0             |
## |           1|             0|0             |0             |
## |           1|             1|1             |0             |
## |           0|             0|0             |1             |
## |           0|             0|1             |0             |
## |           0|             0|0             |1             |
## |           1|             0|0             |1             |
## |           1|             0|0             |1             |
## |           1|             0|1             |0             |
## |           1|             0|0             |0             |
## |           1|             1|0             |0             |
## |           1|             1|1             |1             |
## |           0|             1|1             |1             |
## |           0|             0|0             |0             |
```

## 4. Result
### 4.1 ��ȡ2014�����籭��С��������

```r
Url_2014 <- "http://www.fifa.com/worldcup/groups/index.html"
web_2014 <- htmlTreeParse(Url_2014, useInternal=TRUE)
team <- xpathSApply(web_2014, "//span[@class='t-nText']", xmlValue)
index <- seq(1,64,by=2)
team_1 <- team[index]
played <- xpathSApply(web_2014, "//td[@class='tbl-matchplayed']", xmlValue)
win <- xpathSApply(web_2014, "//td[@class='tbl-win']", xmlValue)
draw <- xpathSApply(web_2014, "//td[@class='tbl-draw']", xmlValue)
lost <- xpathSApply(web_2014, "//td[@class='tbl-lost']", xmlValue)
goal_for <- xpathSApply(web_2014, "//td[@class='tbl-goalfor']", xmlValue)
goal_against <- xpathSApply(web_2014, "//td[@class='tbl-goalagainst']", xmlValue)
score <- xpathSApply(web_2014, "//td[@class='tbl-pts']", xmlValue)
group_2014 <- data.frame(Team=team_1, Win=win, Draw=draw, 
                         Goal_for=goal_for, Goal_against=goal_against)
```

### 4.2 �����̭�������С�������
������̭��������򣬿�������ǰ�ĵ���ģ����Ԥ���ǿ���Ľ�����ֱ��� A1 vs B2, C1 vs D2, E1 vs F2, G1 vs H2 etc.

```r
round_2_1 <- data.frame(Home=group_2014$Team[c(1,9,17,25,5,13,21,29)], 
                      Away=group_2014$Team[c(6,14,22,30,2,10,18,26)],
                      Result=NA)
round_2_2 <- merge(round_2_1, group_2014, by.x="Home", by.y="Team", sort=FALSE) 
round_2 <- merge(round_2_2, group_2014, by.x="Away", by.y="Team", sort=FALSE)

final_8 <- data.frame(Home=as.character(round_2[,2]), 
                      Away=as.character(round_2[,1]), Result=round_2[,3],
                      Won_Home=as.numeric(as.character(round_2[,4])), 
                      Draw_Home=as.numeric(as.character(round_2[,5])), 
                      Goals_For_Home=as.numeric(as.character(round_2[,6])), 
                      Goals_Against_Home=as.numeric(as.character(round_2[,7])),
                      Won_Away=as.numeric(as.character(round_2[,8])),
                      Draw_Away=as.numeric(as.character(round_2[,9])), 
                      Goals_For_Away=as.numeric(as.character(round_2[,10])), 
                      Goals_Against_Away=as.numeric(as.character(round_2[,11])))
```

### 4.3 Prediction
8ǿ�����

```r
prediction_final_8 <- predict(fit_LR, final_8)
prediction_final_8[prediction_final_8<0.5] <- 0
prediction_final_8[prediction_final_8>=0.5] <- 1
final_8[,"Result"] <- as.factor(prediction_final_8)
```

4ǿ�����

```r
## ����ʤ������ж���Щ�������4ǿ����������̭������A1��B2��ʤ�߶Կ�C1��D2��ʤ��
index_1 <- NA
for (i in 1:8){
  if (final_8$Result[i]==1){
    index_1[i]=1
}
else if (final_8$Result[i]==0){
    index_1[i]=2
}
}

Home <- c(as.character(final_8[1,index_1[1]]), 
          as.character(final_8[3,index_1[3]]),
          as.character(final_8[5,index_1[5]]),
          as.character(final_8[7,index_1[7]]))

Away <- c(as.character(final_8[2,index_1[2]]), 
          as.character(final_8[4,index_1[4]]),
          as.character(final_8[6,index_1[6]]),
          as.character(final_8[8,index_1[8]]))

round_3_1 <- data.frame(Home=Home, Away=Away, Result=NA)
round_3_2 <- merge(round_3_1, group_2014, by.x="Home", by.y="Team", sort=FALSE) 
round_3 <- merge(round_3_2, group_2014, by.x="Away", by.y="Team", sort=FALSE)

final_4 <- data.frame(Home=as.character(round_3[,2]), 
                      Away=as.character(round_3[,1]), Result=round_3[,3],
                      Won_Home=as.numeric(as.character(round_3[,4])), 
                      Draw_Home=as.numeric(as.character(round_3[,5])), 
                      Goals_For_Home=as.numeric(as.character(round_3[,6])), 
                      Goals_Against_Home=as.numeric(as.character(round_3[,7])),
                      Won_Away=as.numeric(as.character(round_3[,8])),
                      Draw_Away=as.numeric(as.character(round_3[,9])), 
                      Goals_For_Away=as.numeric(as.character(round_3[,10])), 
                      Goals_Against_Away=as.numeric(as.character(round_3[,11])))


prediction_final_4 <- predict(fit_LR, final_4)
prediction_final_4[prediction_final_4<0.5] <- 0
prediction_final_4[prediction_final_4>=0.5] <- 1
final_4[,"Result"] <- as.factor(prediction_final_4)
```

��������

```r
## ͬ��������ķ��������������
index_2 <- NA
for (i in 1:4){
  if (final_4$Result[i]==1){
    index_2[i]=1
}
else if (final_4$Result[i]==0){
    index_2[i]=2
}
}

Home <- c(as.character(final_4[1,index_2[1]]), 
          as.character(final_4[3,index_2[3]]))

Away <- c(as.character(final_4[2,index_2[2]]), 
          as.character(final_4[4,index_2[4]]))

round_4_1 <- data.frame(Home=Home, Away=Away, Result=NA)
round_4_2 <- merge(round_4_1, group_2014, by.x="Home", by.y="Team", sort=FALSE) 
round_4 <- merge(round_4_2, group_2014, by.x="Away", by.y="Team", sort=FALSE)

semi_final <- data.frame(Home=as.character(round_4[,2]), 
                      Away=as.character(round_4[,1]), Result=round_3[,3],
                      Won_Home=as.numeric(as.character(round_4[,4])), 
                      Draw_Home=as.numeric(as.character(round_4[,5])), 
                      Goals_For_Home=as.numeric(as.character(round_4[,6])), 
                      Goals_Against_Home=as.numeric(as.character(round_4[,7])),
                      Won_Away=as.numeric(as.character(round_4[,8])),
                      Draw_Away=as.numeric(as.character(round_4[,9])), 
                      Goals_For_Away=as.numeric(as.character(round_4[,10])), 
                      Goals_Against_Away=as.numeric(as.character(round_4[,11])))


prediction_final_2 <- predict(fit_LR, semi_final)
prediction_final_2[prediction_final_2<0.5] <- 0
prediction_final_2[prediction_final_2>=0.5] <- 1
semi_final[,"Result"] <- as.factor(prediction_final_2)
```

������

```r
index_3 <- NA
for (i in 1:2){
  if (semi_final$Result[i]==1){
    index_3[i]=1
}
else if (semi_final$Result[i]==0){
    index_3[i]=2
}
}

Home <- as.character(semi_final[1,index_3[1]])

Away <- as.character(semi_final[2,index_3[2]])

round_5_1 <- data.frame(Home=Home, Away=Away, Result=NA)
round_5_2 <- merge(round_5_1, group_2014, by.x="Home", by.y="Team", sort=FALSE) 
round_5 <- merge(round_5_2, group_2014, by.x="Away", by.y="Team", sort=FALSE)

final <- data.frame(Home=as.character(round_5[,2]), 
                      Away=as.character(round_5[,1]), Result=round_3[,3],
                      Won_Home=as.numeric(as.character(round_5[,4])), 
                      Draw_Home=as.numeric(as.character(round_5[,5])), 
                      Goals_For_Home=as.numeric(as.character(round_5[,6])), 
                      Goals_Against_Home=as.numeric(as.character(round_5[,7])),
                      Won_Away=as.numeric(as.character(round_5[,8])),
                      Draw_Away=as.numeric(as.character(round_5[,9])), 
                      Goals_For_Away=as.numeric(as.character(round_5[,10])), 
                      Goals_Against_Away=as.numeric(as.character(round_5[,11])))


prediction_final_1 <- predict(fit_LR, final)
prediction_final_1[prediction_final_1<0.5] <- 0
prediction_final_1[prediction_final_1>=0.5] <- 1
final[,"Result"] <- as.factor(prediction_final_1)
```


```r
Round_of_16 <- as.character(group_2014$Team[c(1,6,9,14,17,22,25,30,5,2,13,10,21,18,29,26)])
Quarter_Final <- c(as.character(final_8[1,index_1[1]]),
                   as.character(final_8[2,index_1[2]]),
                   as.character(final_8[3,index_1[3]]),
                   as.character(final_8[4,index_1[4]]),
                   as.character(final_8[5,index_1[5]]),
                   as.character(final_8[6,index_1[6]]),
                   as.character(final_8[7,index_1[7]]),
                   as.character(final_8[8,index_1[8]]))
Semi_Final <- c(as.character(final_4[1,index_2[1]]),
                as.character(final_4[2,index_2[2]]),
                as.character(final_4[3,index_2[3]]),
                as.character(final_4[4,index_2[4]]))

Final <- c(as.character(semi_final[1,index_3[1]]),
           as.character(semi_final[2,index_3[2]]))
```

�ھ�

```r
if (final$Result[1]==1){
    champion=final[1,1]
} else if (final$Result[1]==0){
    champion=final[1,2]
}


Champion <- as.character(champion)
```


```r
Round_of_16
```

```
##  [1] "Brazil"         "Chile"          "Colombia"       "Italy"         
##  [5] "France"         "Iran"           "Germany"        "Korea Republic"
##  [9] "Netherlands"    "Mexico"         "Costa Rica"     "C<U+00F4>te d'Ivoire"
## [13] "Argentina"      "Switzerland"    "Belgium"        "USA"
```

```r
Quarter_Final
```

```
## [1] "Chile"         "Colombia"      "France"        "Germany"      
## [5] "Netherlands"   "C<U+00F4>te d'Ivoire" "Switzerland"   "USA"
```

```r
Semi_Final
```

```
## [1] "Colombia"    "Germany"     "Netherlands" "USA"
```

```r
Final
```

```
## [1] "Germany"     "Netherlands"
```

```r
Champion
```

```
## [1] "Netherlands"
```


## 5. Future Work
�������������籭��ʷ������Ϊtraining data����С��������Ϊinput������̭�����Ϊoutput����Logistic Regression, Decision Tree��Random Forest����ģ����ɸѡ�˶�test data�����õ�Logistic Regression�����ڽ������籭��С������������Ϊ��̭����Ԥ�⡣Ԥ������׼ȷ�Ի��д�ʱ��ļ��飬�������ʤ���ľ���������̫�࣬ʵ���ӽ�����֧������п��������ˣ�����ֻ���ṩһ��˼·��

��Ԥ��Ҳ�кܶ�������Ҫ��������ȣ�training data�������ز��㣬������Ϊ���籭��ʷ�������Ƚ�С��������1986��֮ǰ�ı�������Ϊ���Ʋ�ͬ����������С���������Բ�����Ϊtraining data������취����������ŷ�ޱ������ޱ�������̭���ı�����Ϊtraining����Σ�Variablesƫ�٣������Variables���������أ�������ĽǶ����������Բο���Predictor�п����ʣ������������������������ȵȣ����ſƼ���չ�������е�����ҲԽ��Խȫ�棬�Ժ��������Լ�����Ա��������Ϊ�ο����ݡ���Щ����Ҫ�����ݲɼ��Ĺ����н����

������һ����������֮�󣬾Ϳ��Բ���Щ�Ƚϸ��ӵ�ģ�ͣ�SVM����ANN������Ϊ����Ԥ��ܺõ�ѡ����֮���ḻ�����������Ƚ�ģ�͵ļ����������Ԥ�����ȷ�ʣ����������������ڱ�������֮ǰ����Զ���ܹ�ȷ���ģ�������Ϊ����Ϸ���Ժ��������룬�������������Ϊ��һ�˶���Ϊ���ص�������


