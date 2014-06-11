���籭���Ԥ��
========================================================
By Casper
Just for fun

����˼·�����籭�����������ݱ����·����ʼΪ����������Ԥ�⣬Ԥ��ķ������ֶ���
�����ǻ����ɷ�Ϊ����������ԱΪ����Ļ�������֪ʶԤ�⣬�Լ��������Ϊ����������
Ԥ�⡣���Ĳ��õ��ǻ���ѧϰ�ķ���������ʷ���籭������Ϊtraining data������̭������
����ʤ��ΪԤ�����С��������ΪԤ�����ӣ��������ģ�ͣ����Ͻ�������������Ϊ
validation��ɸѡ������ģ�ͣ��Ա������籭����̭���������Ԥ�⣬���������Kaggle��
March Machine Learning Mania������NCAA�����Ԥ��


����Ԥ����ͼ
![title](path/to/your/image)


## 1.Getting the data
������ԴΪFIFA������ץȡ1966-2010��ʮһ�����籭����ʷ������ҳ��ͨ������html�ļ���
��ð���С����ʤ��������������ƽ������������������������������Ϣ���ٽ����̭����
ʤ����������ɱ��

```r
setwd("./worldcup_prediction")
```

```
## Error: cannot change working directory
```

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



## 2.Clean the data
###2.1��2010�����籭���Ϊtesting dataset

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

*ע����̭���еĽ��ֻ�и��ݱȷ����б�ȡ��49-64���ıȷ����ݣ�Node��
<td style="width:120px" class="c ">�� c�������һ���ո�*

���ݱȷֽ���ʤ����������������ǵ�������PSO��ʾ�����ս���ж������û�е����
ʤ��ÿ��ĵ�һ��Ԫ��Ϊʤ�����ݣ�����е����ʤ��������Ԫ����Ϊ����

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

���С��������̭�����ݣ���team��Ϊindex

![�������](�������.png)


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


### 2.2��1966-2006�����籭���Ϊtraining dataset
���й����з���һЩ���⣬1998-2010�����籭�Ĳ�������Ϊ32֧������Ϊ64����������̭
��Ϊ��49-64����1982-1994���������Ϊ24֧������Ϊ52����������̭��Ϊ��37-52����1966-1978���������Ϊ16֧������Ϊ38���������������⣬78����ǰС������Ϊ���֣���һ�ֽ����ģ�����ڶ���С�������������������֣�����޷�����С������Ϣ��Ϊ��̭����Ԥ�����أ����training dataֻ����1982-2006������ݡ� %>_<%

