## ===============================By Casper====================================
## =============================Just for fun===================================

## 基本思路：世界杯比赛正如火如荼，各路大神开始为比赛作分析预测，预测的方法多种
## 多样，但是基本可分为以足球评论员为代表的基于足球知识预测，以及以章鱼哥为代表
## 的随机性预测。本文采用的是机器学习的方法，以历史世界杯数据作为training data，
## 以淘汰赛两队比赛胜负为预测对象，小组赛表现为预测因子，构建多个模型，用上届世
## 杯的数据作为validation，筛选出最优模型，对本届世界杯的淘汰赛结果进行预测，灵
## 感来自于Kaggle的March Machine Learning Mania比赛对NCAA结果的预测

## 先上预测结果图

## ============================================================================

## 1. Getting the data
## 数据来源为FIFA官网，抓取1966-2010共十一届世界杯的历史数据网页，通过解析html文
## 件，获得包含小组赛胜场数、负场数、平局数、进球数、被进球数、积分信息，再结合
## 淘汰赛的胜负结果，生成表格

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


## 插入html图片

## ============================================================================

## 2. Clean the data

## 以2010年世界杯结果为testing dataset

## 获得小组赛结果
score_2010 <- xpathSApply(web_2010, "//td[@class='c']", xmlValue)
team_name <- xpathSApply(web_2010, "//td[@class='l']", xmlValue)

##根据表格看出有7列数据
gpresult_2010_1 <- matrix(score_2010, ncol=7, byrow=TRUE) 
gpresult_2010_2 <- cbind(team_name, gpresult_2010_1)
colnames(gpresult_2010_2) <- c("Team", "Played", "Won", "Draw", "Lost", 
"Goals For", "Goals Against", "Points")

## 小组赛比赛场数是一定的，根据胜场、平局可以计算出负场和积分，因此为了防止共线
## 性参数的干扰，排除"Played",“Lost"和"Points"几列
## gpresult_2010_3 <- gpresult_2010_2[,c(1,3,4,6,7)]

## 获得淘汰赛结果，小组赛共有48场，淘汰赛16场，共64场，这里需要第49-64场的数据
home_team <- xpathSApply(web_2010, "//td[@class='l homeTeam']", 
                         xmlValue)[49:64]
away_team <- xpathSApply(web_2010, "//td[@class='r awayTeam']",
                         xmlValue)[49:64]

## 注意淘汰赛中的结果只有根据比分来判别，取第49-64场的比分数据，
## Node是<td style="width:120px" class="c ">， c后面多了一个空格
result <- xpathSApply(web_2010, "// td[@class='c ']", xmlValue)[49:64]


## 根据比分解析胜负结果，括号里面是点球结果，PSO表示点球大战。判定：如果没有点球
## 决胜，每项的第一个元素为胜负依据，如果有点球决胜，第三个元素作为依据
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

## 组合小组赛和淘汰赛数据，以team作为index
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
write.csv(final_result_2010,"./worldcup_prediction/test.csv")

## 以1966-2006年世界杯结果为training dataset，进行过程中发现一些问题，1998-2010
## 年世界杯的参赛队伍为32支，比赛为64场，其中淘汰赛为第49-64场；1982-1994年参赛
## 队伍为24支，比赛为52场，其中淘汰赛为第37-52场；1966-1978年参赛队伍为16支，比
## 赛为38场，发现严重问题，78年以前小组赛分为两轮，第一轮晋级的，进入第二轮小组
## 赛，继续比赛争出现，因此无法利用小组赛信息作为淘汰赛的预测因素，因此training
## data只采用1982-2006年的数据。 %>_<%

## 建立function，以处理testing data的方法，建立training dataset
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
    write.csv(final_result, paste("./worldcup_prediction/",filename,".csv",
                                  sep=""))
    
}

Whole_Url <- c(Url_2006, Url_2002, Url_1998, Url_1994, Url_1990, Url_1986)

for (i in 1:6){
    create_train_data(Whole_Url[i])
}

training_1986 <- read.csv("./worldcup_prediction/training_1986.csv")
training_1990 <- read.csv("./worldcup_prediction/training_1990.csv")
training_1994 <- read.csv("./worldcup_prediction/training_1994.csv")
training_1998 <- read.csv("./worldcup_prediction/training_1998.csv")
training_2002 <- read.csv("./worldcup_prediction/training_2002.csv")
training_2006 <- read.csv("./worldcup_prediction/training_2006.csv")

training <- rbind(training_1986, training_1990, training_1994, training_1998,
                  training_2002, training_2006)

write.csv(training, "./worldcup_prediction/training.csv")


## ============================================================================

## 3. Data Analysis

test <- read.csv("./worldcup_prediction/test.csv")

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


## SVD

par(mfrow=c(1,1))
svd1 <- svd(train_data[,c(3,4,6,7,10,11,13,14)])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)

## 可以看出各个因素中的共线性非常严重，分析原因如下，每队的出场数是一定的，胜负
## 平三场的总数是一定的，胜负结果确定以后积分也是一定的，因此，需要排除出场数
## "Played",负场数"Lost_Home"和"Lost_Away"，以及小组积分"Point_Home"和
## "Point_Away"

## 用train_data建立prediction model，用test进行模型筛选
## Try Logistic Regression Model
fit_LR <- glm(Result ~ Goals_For_Home + Goals_Against_Home + Goals_For_Away +
                  Goals_Against_Away, data=train_data, family= "binomial")

prediction_LR <- predict(fit_LR, test)
prediction_LR[prediction_LR<0.5] <- 0
prediction_LR[prediction_LR>=0.5] <- 1

## Try Decision Tree Model
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

fit_DT <- rpart(Result ~ Goals_For_Home + Goals_Against_Home + Goals_For_Away 
                + Goals_Against_Away, data=train_data, method="class")

prediction_DT <- predict(fit_DT, test, type="class")


fancyRpartPlot(fit_DT)

dev.copy(pdf, file="fit_DT.pdf")
dev.off()

## Try Random Forest Model
library(randomForest)
fit_RF <- randomForest(as.factor(Result) ~ Goals_For_Home + Goals_Against_Home
                       + Goals_For_Away + Goals_Against_Away, data=train_data, 
                       importance=TRUE, ntree=100)
prediction_RF <- predict(fit_RF, test)


## Try Artificial Nerual Network Model

library(neuralnet)
fit_NN <- neuralnet(Result ~ Goals_For_Home + Goals_Against_Home + 
                    Goals_For_Away + Goals_Against_Away, train_data)
prediction_NN <- predict(fit_NN, test)


## 检查不同模型的预测效果
model_check <- data.frame(test$Result, prediction_LR, prediction_DT, 
                          prediction_RF)
print(fit_NN)
plot(fit_NN)



