Worldcup_prediction
===================

a simple method to predict worldcup result

基本思路：世界杯比赛正如火如荼，各路大神开始为比赛作分析预测，预测的方法多种多样，但是基本可分为以足球评论员为代表的基于足球知识预测，以及以章鱼哥为代表的随机性预测。本文采用的是机器学习的方法，以历史世界杯数据作为training data，以淘汰赛两队比赛胜负为预测对象，小组赛表现为预测因子，构建多个模型，用上届世杯的数据作为validation，筛选出最优模型，对本届世界杯的淘汰赛结果进行预测，灵感来自于Kaggle的March Machine Learning Mania比赛对NCAA结果的预测

Free to copy and modify. Please include author name if you copy code.
