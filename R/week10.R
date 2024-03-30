# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(janitor)
library(caret)

# Data Import and Cleaning
gss_tbl<-read_sav(file="../data/GSS2016.sav") %>%
  drop_na(MOSTHRS)%>%
  rename(work_hours=MOSTHRS) %>% #renaming it for my convenience and to minimize confusion
  select(-HRS1,
         -HRS2,
         -where(~mean(is.na(.))>0.75))%>%
  sapply(as.numeric)%>%
  as_tibble()
  

# Visualization
gss_tbl%>%
  ggplot(aes(x=work_hours))+
  geom_histogram()

#Analysis

#Ols regression = lm (intercept)
# Elastic net = glmnet (alpha, lambda)
# random forest = ranger (mtry, splitrule, min.node.size)
# eXtreme Gradient Boosting = xgbLinear (nrounds, lambda, alpha, eta)


set.seed(2001)
random_data<-gss_tbl[sample(nrow(gss_tbl)),]
training<- random_data[1:round(0.75*nrow(random_data)),]
test <-random_data[(round(0.75*nrow(random_data))+1):nrow(random_data),] 
fold_indices <- createFolds(training$work_hours, 10)
Methods <- c("lm","glmnet","ranger","xgbLinear")
lm_grid <- expand.grid(intercept=TRUE)
glmnet_grid <- expand.grid(alpha = c(0, 1),
                           lambda = seq(0.0001, 0.1, length = 20))
ranger_grid <-  expand.grid(mtry = c(2, 3, 4, 5, 10, 20, 50, 100), 
                            splitrule = c("variance","extratrees"),
                            min.node.size = 5)
xgbLinear_grid <- expand.grid(nrounds = seq(5, 50, by = 10),
                                          lambda = c(0, 0.1, 2),
                                          alpha = seq(0, 1, by = 0.1),
                                          eta = c(0.5, 1))
GRID <- c(" expand.grid(intercept=TRUE) " ,glmnet_grid,ranger_grid,xgbLinear_grid)


for (i in 1:length(Methods)){
model_test <- train(
  work_hours~.,
  data=training,
  method= Methods[i],
  preProcess= "medianImpute",
  na.action= na.pass,
  trControl = trainControl(method = "cv", 
                            indexOut = fold_indices,
                            number = 10,  
                            verboseIter = TRUE),
  tuneGrid = GRID[i]
)

testing[[Methods[i]]] <- list(
  model=model,
  cv_rsq = max(model$results$Rsquared, na.rm = TRUE)
)

predictions <- predict(model, test, na.action = na.pass)
results[[Methods[i]]]$ho_rsq <- cor(predictions, test$work_hours)^2
}



#working
model_test <- train(
  work_hours~.,
  data=training,
  method= Methods[1],
  preProcess= "medianImpute",
  na.action= na.pass,
  tuneLength=10,
  trControl = trainControl(method = "cv", 
                           indexOut = fold_indices,
                           number = 10,  
                           search = "grid",
                           verboseIter = TRUE),
  tuneGrid = str_replace_all(GRID[1],'/"',"a")
)
