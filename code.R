library(ISLR)
library(MASS)
data("Default")

set.seed(12334)
train_idx <- sample(seq(Default %>% nrow),size = (Default %>% nrow)*0.8)
testD <- Default[-train_idx,]
trainD <- Default[train_idx,]
testY <- testD$default; trainY <- trainD$default

# 10-fold cv function

clda_fold_cv <- function(train,y_index,k=10,model=1){

  if(k<2) stop("K-fold should be up to 1!")
  if(nrow(train)<k) stop("K-fold should be smaller than nrow(train)!")
  # model 1 : logistic, model 2 : lda, model 3 : qda, model 4 : naivebayes 
  
  idx <- sample(seq(train %>% nrow),train %>% nrow)
  n <- length(idx)
  t <- seq(0,n,length=k+1)
  accur <- selected_length <- rep(0,k)
  
  for(i in seq(k)){
    start <- as.integer(t[i]+1)
    end <- as.integer(t[i+1])
    selected_idx <- start:end
    selected_length[i] <- length(selected_idx)
    val_idx <- idx[selected_idx]; val_dat <- train[val_idx,]
    tra_idx <- idx[-selected_idx]; tra_dat <- train[tra_idx,]

    if(model==1){
      tra_fit <- glm(tra_dat[,y_index]~.,data=tra_dat[,-y_index],family=binomial)
      val_tbl <- table(factor(predict(object = tra_fit,newdata=val_dat[,-y_index])>0.5),val_dat[,y_index])
    } else if(model %in% 2:3){
      if(model==2) tra_fit <- lda(tra_dat[,y_index]~.,data=tra_dat[,-y_index]) 
      else tra_fit <- qda(tra_dat[,y_index]~.,data=tra_dat[,-y_index])
      val_tbl <- table(predict(tra_fit,val_dat[,-y_index])$class,val_dat[,y_index])
    } else{
      tra_fit <- naiveBayes(tra_dat[,y_index]~.,data=tra_dat[,-y_index],laplace = 1)
      val_tbl <- table(predict(tra_fit,val_dat[,-y_index]),val_dat[,y_index])
    }
      accur[i] <- val_tbl %>% diag %>% sum / val_tbl %>% sum
  }
  
  return(sum(accur*selected_length)/sum(selected_length))
}

ll <- vector("list",4)
ll <- lapply(1:4,function(x) clda_fold_cv(trainD,1,k = 10,model=x))
ll # lda, qda
