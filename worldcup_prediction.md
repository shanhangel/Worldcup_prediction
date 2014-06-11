世界杯结果预测
========================================================
By Casper
Just for fun

基本思路：世界杯比赛正如火如荼，各路大神开始为比赛作分析预测，预测的方法多种多样
，但是基本可分为以足球评论员为代表的基于足球知识预测，以及以章鱼哥为代表的随机性
预测。本文采用的是机器学习的方法，以历史世界杯数据作为training data，以淘汰赛两队
比赛胜负为预测对象，小组赛表现为预测因子，构建多个模型，用上届世杯的数据作为
validation，筛选出最优模型，对本届世界杯的淘汰赛结果进行预测，灵感来自于Kaggle的
March Machine Learning Mania比赛对NCAA结果的预测


先上预测结果图
![title](path/to/your/image)


## 1.Getting the data
数据来源为FIFA官网，抓取1966-2010共十一届世界杯的历史数据网页，通过解析html文件，
获得包含小组赛胜场数、负场数、平局数、进球数、被进球数、积分信息，再结合淘汰赛的
胜负结果，生成表格

```r
setwd("./worldcup_prediction")
```

```
## Error: cannot change working directory
```

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



## 2.Clean the data
###2.1以2010年世界杯结果为testing dataset

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

*注意淘汰赛中的结果只有根据比分来判别，取第49-64场的比分数据，Node是
<td style="width:120px" class="c ">， c后面多了一个空格*

根据比分解析胜负结果，括号里面是点球结果，PSO表示点球大战。判定：如果没有点球决
胜，每项的第一个元素为胜负依据，如果有点球决胜，第三个元素作为依据

```r
result1 <- as.character(result)
```

```
## Error: object 'result' not found
```

```r
split <- strsplit(result1," ")
```

```
## Error: object 'result1' not found
```

```r
result2 <- list()
for (i in 1:16){
    if (regexpr("PSO", result1[[i]])>0){
       result2[i] <- split[[i]][length(split[[i]])-1] 
    }
    else{
        result2[i] <- split[[i]][1]
    }
}
```

```
## Error: object 'result1' not found
```

```r
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
```

```
## Error: subscript out of bounds
```

```r
eliresult_2010 <- data.frame(home_team=home_team, away_team=away_team, 
                             result=result,result3=as.numeric(result3))
```

```
## Error: object 'result' not found
```

组合小组赛和淘汰赛数据，以team作为index

![数据组合](数据组合.png)


```r
final_result_2010 <- merge(eliresult_2010, gpresult_2010_2, by.x="home_team", 
                      by.y="Team", sort=TRUE)                                                             
```

```
## Error: object 'eliresult_2010' not found
```

```r
final_result_2010 <- merge(final_result_2010, gpresult_2010_2, by.x="away_team", 
                      by.y="Team", sort=FALSE)
```

```
## Error: object 'final_result_2010' not found
```

```r
final_result_2010 <- data.frame(final_result_2010[,2], final_result_2010[,1],
                                final_result_2010[,4:18])
```

```
## Error: object 'final_result_2010' not found
```

```r
final_result_2010[18] <- "2010"
```

```
## Error: object 'final_result_2010' not found
```

```r
colnames(final_result_2010) <- c("Home", "Away", "Result", "Played_Home", 
                                 "Won_Home", "Draw_Home", "Lost_Home", 
                                 "Goals_For_Home", "Goals_Against_Home", 
                                 "Point_Home","Played_Away", "Won_Away",
                                 "Draw_Away", "Lost_Away", "Goals_For_Away",
                                 "Goals_Against_Away", "Point_Away", "Year")
```

```
## Error: object 'final_result_2010' not found
```

```r
write.csv(final_result_2010,"./worldcup_prediction/test.csv")
```

```
## Error: object 'final_result_2010' not found
```


### 2.2以1966-2006年世界杯结果为training dataset
进行过程中发现一些问题，1998-2010年世界杯的参赛队伍为32支，比赛为64场，其中淘汰
赛为第49-64场；1982-1994年参赛队伍为24支，比赛为52场，其中淘汰赛为第37-52场；1966-1978年参赛队伍为16支，比赛为38场，发现严重问题，78年以前小组赛分为两轮，第一轮晋级的，进入第二轮小组赛，继续比赛争出现，因此无法利用小组赛信息作为淘汰赛的预测因素，因此training data只采用1982-2006年的数据。 %>_<%

