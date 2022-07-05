install.packages("reticulate")
install.packages("RcppTOML")
# Warning in install.packages :
#   installation of package ‘RcppTOML’ had non-zero exit status
library(reticulate)
use_python("/usr/local/bin/python")


# 示例 ----
# agaricus.train - Large list ----
library(mlr3)
library(mlr3verse)
## 创建分类任务
task = as_task_classif(iris, target = "Species")
## 选择学习器, 并设置两个超参数: 最大深度, 最小分支节点数
learner = lrn("classif.rpart" , maxdepth = 3, minsplit = 10)
## 划分训练集/测试集
set.seed(123)
split = partition(task, ratio = 0.7)
## 训练模型
learner$train(task, row_ids = split$train)
## 模型预测
predictions = learner$predict(task, row_ids = split$test)
## 模型评估
predictions$confusion # 混淆矩阵
#> truth
#> response setosa versicolor virginica
#> setosa 15 0 0
#> versicolor 0 14 0
#> virginica 0 1 15
predictions$score(msr("classif.acc")) # 准确率
#> classif.acc
#> 0.978
# 

# eg 2 ----
# install.packages('glmnet')
# install.packages('devtools')
library(devtools)
install_version("glmnet", version = "3.0") # fail

library(tidyverse)
library(mlr3verse)
library(glmnet) # 正则化广义回归模型


boston = tsk("boston_housing")$data()
glimpse(boston)



# eg 3 ----
rm(list=ls())
library(mlr3verse)
library(tidyverse)
load("data/pima.rda")
dat = na.omit(dat)


task = as_task_classif(dat, target = "diabetes")
task
#> <TaskClassif:dat> (392 x 9)
#> * Target: diabetes
#> * Properties: twoclass
#> * Features (8):
#> - dbl (8): age, glucose, insulin, mass, pedigree, pregna
autoplot(task)


autoplot(task$clone()$select(task$feature_names[1:3]),
         type = "pairs") 

#install.packages("GGally")
install.packages("ranger")

Ranger = lrn("classif.ranger") # 需要安装 ranger 包
Ranger # 不能处理 NA
#> <LearnerClassifRanger:classif.ranger>
#> * Model: 
set.seed(123)
split = partition(task, ratio = 0.8)
# 默认 stratify = TRUE

Ranger$param_set # 查看所有超参数及默认值



library(paradox)
search_space = ps(
  num.trees = p_int(lower = 1, upper = 20,
                    trafo = function(x) 20 * x),
  min.node.size = p_int(lower = 3, upper = 30))
at = auto_tuner(
  learner = Ranger,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.acc"),
  search_space = search_space,
  method = "random_search",
  term_evals = 10)


set.seed(615)
at$train(task, row_ids = split$train)
#> INFO
at$tuning_result # 调参结果

Ranger$param_set$values = at$tuning_result$learner_param_vals
Ranger$train(task, row_ids = split$train)




