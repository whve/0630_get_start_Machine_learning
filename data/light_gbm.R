rm(list=ls())
#BiocManager::install("lightgbm")   ##  ***为需要安装的包的名称
library(lightgbm)
library(R6)
data(agaricus.train, package='lightgbm')
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label = train$label)
model <- lgb.cv(
  params = list(
    objective = "regression"
    , metric = "l2"
  )
  , data = dtrain
)


##连续变量 函数构建
lightgbm_model_GM <- function(X,
                              Y,
                              kfold=10,
                              seed=1234,
                              categorical_feature=NULL){
  set.seed(seed)
  library(lightgbm)
  kfold10 <- sample(rep(1:kfold,ceiling(nrow(X)/kfold))[1:nrow(X)])
  
  prob <- matrix(NA,nrow(X),1)
  for (i in (1:kfold)){
    cat(paste("##########fold",i,"##############\n"))
    
    
    lgb.train = lgb.Dataset(data=as.matrix(X[kfold10!=i,]), label=Y[kfold10!=i],free_raw_data=F)
    
    lgb.grid = list(objective = "regression",
                    metric = "l2",
                    learning_rate=0.005,
                    feature_fraction=0.2,
                    min_data_in_leaf=15,
                    n_estimators=2000,
                    bagging_fraction=0.8,
                    bagging_freq=1
    )
    
    dtest <- lgb.Dataset(data=as.matrix(X[kfold10==i,]), label = Y[kfold10==i],free_raw_data=F)
    valids <- list(test = dtest)
    
    set.seed(seed)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train,record=F,valids=valids,categorical_feature=categorical_feature,verbose=-1)
    
    prob[kfold10==i,1]  <- predict(lgb.model,data=as.matrix(X[kfold10==i,]))
    
  }
  
  cor_temp <- cor.test(Y,prob,method="spearman")
  
  r_value <- cor_temp$estimate
  p_value <- cor_temp$p.value
  
  
  result<-list(r_value=r_value,p_value=p_value,pred=prob)
  return(result)
}

library(haven)
data_ac <- read_dta("lightgbm_overweight.dta")
data_ac <- read_dta("基线细菌预测孕周天数.dta")
X <- data_ac[,c(13:158)]
Y <- data_ac[,1]
X <- data_ac[,c(2:147)]
Y <- data_ac[,1]
##必须用matrix
X <- as.matrix(X)
Y <- as.matrix(Y)
output <- lightgbm_model_GM(X,Y)



##分类变量 函数构建
rm(list=ls())
library(lightgbm)
lightgbm_model_GM <- function(X,
                              Y,
                              kfold=10,
                              seed=1234,
                              categorical_feature=NULL){  #categorical_feature 制定哪些列属于分类特征
  set.seed(seed)
  library(lightgbm)
  kfold10 <- sample(rep(1:kfold,ceiling(nrow(X)/kfold))[1:nrow(X)])
  
  prob <- matrix(NA,nrow(X),1)
  for (i in (1:kfold)){
    cat(paste("##########fold",i,"##############\n"))
    
    
    lgb.train = lgb.Dataset(data=as.matrix(X[kfold10!=i,]), label=Y[kfold10!=i],free_raw_data=F)  ##设置训练集数据
    
    lgb.grid = list(objective = "binary",
                    metric = "binary_logloss",
                    boosting="gbdt", #默认gbdt, 可选rf 随机森林
                    learning_rate=0.005,
                    feature_fraction=0.8,  ##默认是1.0，小于1，LightGBM 将会在每次迭代中随机选择部分特征. 例如, 如果设置为 0.8, 将会在每棵树训练之前选择 80% 的特征
                    min_data_in_leaf=15,
                    n_estimators=2000,
                    bagging_fraction=0.8, ##类似于 feature_fraction, 但是它将在不进行重采样的情况下随机选择部分数据
                    bagging_freq=1 ## bagging 的频率, 0 意味着禁用 bagging. k 意味着每 k 次迭代执行bagging
    )
    
    dtest <- lgb.Dataset(data=as.matrix(X[kfold10==i,]), label = Y[kfold10==i],free_raw_data=F)
    valids <- list(test = dtest)
    
    set.seed(seed)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train,record=F,valids=valids,categorical_feature=categorical_feature,verbose=-1)
    #data= 此处为训练数据集，valids 为验证数据集，疑问此处的valids参与了调参 同时又作为了后续的测试集？
    prob[kfold10==i,1]  <- predict(lgb.model,data=as.matrix(X[kfold10==i,]))
  }
  #library(pROC)
  #ROC <- roc(Y[,1], prob[,1])
  #AUC <- round(auc(ROC), 3)
  #CI <- round(ci(auc(ROC)),3)
  #result1<-list(AUC=AUC,CI=CI)
  #return(result1)
  result2<-list(pred=prob)
  return(result2)
}


data_ac <- read_dta("真菌细菌_全基线_预测结局.dta")
X <- data_ac[,c(12:252)] #真菌细菌
Y1 <- data_ac[,4] #4 巨大儿， 5 低出生体重， 6早产
Y2 <- data_ac[,5] #4 巨大儿， 5 低出生体重， 6早产
Y3 <- data_ac[,6] #4 巨大儿， 5 低出生体重， 6早产

X <- as.matrix(X)
Y1 <- as.matrix(Y1)
Y2 <- as.matrix(Y2)
Y3 <- as.matrix(Y3)


healthy_macrosomia <- lightgbm_model_GM(X,Y1)
healthy_lowbirthwt <- lightgbm_model_GM(X,Y2)
healthy_preterm <- lightgbm_model_GM(X,Y3)


healthy_birth1 <- healthy_macrosomia #1 巨大儿， 2
healthy_birth2 <- healthy_lowbirthwt #1 巨大儿， 2 低出生体重
healthy_birth3 <- healthy_preterm #1 巨大儿， 2 低出生体重

library(pROC)
pred1 <- healthy_birth1$pred
pred2 <- healthy_birth2$pred
pred3 <- healthy_birth3$pred
ROC_birth1 <- roc(Y1[,1], pred1[,1])
ROC_birth2 <- roc(Y2[,1], pred2[,1])
ROC_birth3 <- roc(Y3[,1], pred3[,1])

AUC1 <- round(auc(ROC_birth1), 3)
AUC2 <- round(auc(ROC_birth2), 3)
AUC3 <- round(auc(ROC_birth3), 3)
CI1 <- round(ci(auc(ROC_birth1)),3)

plot(1-ROC_birth$specificities, ROC_birth$sensitivities, type="l", col="red", lty=1, xlab="横坐标标题", ylab="纵坐标标题", lwd=2,)
abline(0,1)
legend(0.55, 0.35, c("preterm_prediction"), lty=c(1), 
       lwd=c(2),
       col=c("red"),
       bty="n")# #o 为给图例加边框 

plot(1-ROC_preg$specificities, ROC_preg$sensitivities, type="l", col="red", lty=1, xlab="横坐标标题", ylab="纵坐标标题", lwd=2,)
abline(0,1)
legend(0.55, 0.35, c("preterm_prediction"), lty=c(1), 
       lwd=c(2),
       col=c("red"),
       bty="n")# #o 为给图例加边框 

##多个RCO结果 同时作图至一张图
plot(1-ROC_birth1$specificities, ROC_birth1$sensitivities, type="l", col="red", lty=1, xlab="横坐标标题", ylab="纵坐标标题", lwd=2,)
abline(0,1)

lines (1-ROC_birth2$specificities, ROC_birth2$sensitivities, type="l", col="blue", lty=1, lwd=2,)
lines (1-ROC_birth3$specificities, ROC_birth3$sensitivities, type="l", col="green", lty=1, lwd=2,)

legend(0.55, 0.35, c("macrosomia", "preterm", "lowbirthwt" ), lty=c(1, 1), 
       lwd=c(2,2),
       col=c("red", "blue", "green"),
       bty="n") ##o 为给图例加边框




####lightgbmExplainer
BiocManager::install("lightgbmExplainer")  #an R package that makes LightGBM fully interpretable
library("lightgbmExplainer")