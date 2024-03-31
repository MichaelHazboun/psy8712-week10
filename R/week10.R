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



random_data<-gss_tbl[sample(nrow(gss_tbl)),]
training<- random_data[1:round(0.75*nrow(random_data)),]
test <-random_data[(round(0.75*nrow(random_data))+1):nrow(random_data),] 
fold_indices <- createFolds(training$work_hours, 10)
Methods <- c("lm","glmnet","ranger","xgbLinear")
# You can ignore the bellow comments, this is just a reminder I left for myself for my pain, sadness and struggle.

# lm_grid <- NULL
# glmnet_grid <- expand.grid(alpha = c(0, 1),
                           # lambda = seq(0.0001, 0.1, length = 20))
# ranger_grid <-  expand.grid(mtry = c(2, 3, 4, 5, 10, 20, 50, 100), 
                            # splitrule = c("variance","extratrees"),
                            # min.node.size = 5)
# xgbLinear_grid <- expand.grid(nrounds = seq(5, 50, by = 10),
                                          # lambda = c(0, 0.1, 2),
                                          # alpha = seq(0, 1, by = 0.1),
                                          # eta = c(0.5, 1))
# GRID <- c(lm_grid ,glmnet_grid,ranger_grid,xgbLinear_grid)
testing <- list()
Mygrid <- "thing"
for (i in 1:length(Methods)){
  set.seed(2001)
  if(i==1){
    Mygrid <- NULL
  } else if(i==2) {
    Mygrid <- expand.grid(alpha = c(0, 1),
                          lambda = seq(0.0001, 0.1, length = 20))
  }else if(i==3) {
    Mygrid <- expand.grid(mtry = c(2, 3, 4, 5, 10, 20, 50, 100), 
                          splitrule = c("variance","extratrees"),
                          min.node.size = 5)
  }else if(i==4) {
    Mygrid <-expand.grid(nrounds = seq(5, 50, by = 10),
                         lambda = c(0, 0.1, 2),
                         alpha = seq(0, 1, by = 0.1),
                         eta = c(0.5, 1))
  }
  
  model<- train(
    work_hours~.,
    data=training,
    method= Methods[i],
    preProcess= "medianImpute",
    na.action= na.pass,
    trControl = trainControl(method = "cv", 
                             indexOut = fold_indices,
                             number = 10,  
                             verboseIter = TRUE),
    tuneGrid = Mygrid
  )
  
  testing[[Methods[i]]] <- list(
    model=model,
    cv_rsq1 = max(model$results$Rsquared, na.rm = TRUE)
  )
  
  predictions <- predict(model, test, na.action = na.pass)
  testing[[Methods[i]]]$ho_rsq1 <- cor(predictions, test$work_hours)^2
}

# Publication
table1_tbl <- tibble(
  algo= as.character(),
  cv_rsq=as.character(),
  ho_rsq=as.character()
)
i=1
for (i in 1:length(Methods)){ 
  cv_rsq <- str_remove(formatC(testing[[Methods[i]]]$cv_rsq1,
                               format = 'f', 
                               digits = 2),
                       "^0")
  ho_rsq <- str_remove(formatC(testing[[Methods[i]]]$ho_rsq1,
                               format = 'f', 
                               digits = 2),
                       "^0")
  working <- tibble(
    algo = Methods[i],
    cv_rsq = cv_rsq,
    ho_rsq = ho_rsq)
  table1_tbl <- bind_rows(table1_tbl, working)
}

table1_tbl


#1) 
# My final model 10-fold CV r squared continued to improve/increase as we went from lm--> glmnet --> ranger --> xgbLinear, with the OLS regression being much worse than the rest.
# Additionally, the holdout r squares had a similar pattern (increase between models), but this time it peaked with the random forest and dipped again with the eXtreme Gradient boosting. 
# At the same time the holdout r squares were simply smaller than those 10 fold CV r squares.
# I feel like the CV being larger than the HO is mainly because were testing the model with the data we trained it on, vs new data. It's worse at predicting new data compared to fitting the data it was trained on.
# Moreover, the increasing R squares is most probably due the increased complexity of the models, more parameters, better predictions. 
# However, the peaking of the R squares for the holdout test being the random forest might be due to it not over fitting the model while the extreme gradient boosting might have done that.

#2)
# I accidentally answered this in my answer to the last question, but put simply, they decreased from the K-fold CV to the holdout CV. This is most probably happening because the model is better at predicting the data that I was trained on, compared to a dataset that is foreign to the model.

#3)
# I would honestly use the random forest model, the difference between it's R squared and the xgbLinear R squared was minimal with the K-fold Cv and the random forest had the best r squared when we did the holdout CV.
# That on its own gives me enough reason to want to use the random forest, but when you consider that the random forest was significantly faster than the xgbLinear, which definetly performed the second best, the choice becomes even more clear.
# The random forest, is complex enough to be quite accurate, while not over fitting the data as much as the xgbLinear model. Additionally, it doesn't take all that much time. The random forrest definetly took a lot more time than the glmnet and the lm models, but it out performed both adn the added weight time wasn't as painful as the added wait from the xgbLinear model.
# So, as a tldr, I'd choose the random forest model, because it's quite accurate, complicated enough, and not too slow.















# More of my pain
# model_test <- train(
  # work_hours~.,
  # data=training,
  # method= Methods[1],
  # preProcess= "medianImpute",
  # na.action= na.pass,
  # tuneLength=10,
  # trControl = trainControl(method = "cv", 
                           # indexOut = fold_indices,
                           # number = 10,  
                           # search = "grid",
                           # verboseIter = TRUE),
  # tuneGrid = GRID[1]
# )
