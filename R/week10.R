# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(janitor)
library(caret)


# Data Import and Cleaning
gss_tbl<-read_sav(file="../data/GSS2016.sav") %>%
  drop_na(MOSTHRS)%>% # got rid of na's
  rename(work_hours=MOSTHRS) %>% #renaming it for my convenience and to minimize confusion
  select(-HRS1,
         -HRS2, # getting rid of undesired columns
         -where(~mean(is.na(.))>0.75))%>% #Tried a bunch of different keeping the one with less than 75% missingness. This path made logical sense and worked, (figuring out the ~ was a pain though) 
  sapply(as.numeric)%>% #nothing was working in the later section, saw that other people did this, tried it,then it worked
  as_tibble()
  

# Visualization
gss_tbl%>% # don't think an explanation is really needed here.
  ggplot(aes(x=work_hours))+
  geom_histogram()

#Analysis

#Ols regression = lm (intercept)
# Elastic net = glmnet (alpha, lambda)
# random forest = ranger (mtry, splitrule, min.node.size)
# eXtreme Gradient Boosting = xgbLinear (nrounds, lambda, alpha, eta)



random_data<-gss_tbl[sample(nrow(gss_tbl)),] #randomising the data

training<- random_data[1:round(0.75*nrow(random_data)),] #splitting the data
test <-random_data[(round(0.75*nrow(random_data))+1):nrow(random_data),] #splitting the data

fold_indices <- createFolds(training$work_hours, 10) # to keep fold composition the same (you told us this is recommended in slide 38)

Methods <- c("lm","glmnet","ranger","xgbLinear") # to pull from these for the loop. Also, I used xgbLinear, because it had the smallest number of hyperparameters from the options on the website
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
testing <- list() #a reset/satrting point
Mygrid <- "thing" #a reset/satrting point
for (i in 1:length(Methods)){
  set.seed(2001) #for reproducability
  if(i==1){
    Mygrid <- NULL #I tried having the tuneGrid= GRID[i] before (can see some of the code above and bellow), it didn't work and this was one of the eventual ways I got it to work. The null is because the LM hyper parameter is intercept=T which is the default setting
  } else if(i==2) {
    Mygrid <- expand.grid(alpha = c(0, 1), # took these values from the slides/datacamp
                          lambda = seq(0.0001, 0.1, length = 20))
  }else if(i==3) {
    Mygrid <- expand.grid(mtry = c(2, 3, 4, 5, 10, 20, 50, 100), # took these values from the slides/datacamp
                          splitrule = c("variance","extratrees"), # the model wasn't working for some reason, looked at other people to see how it could work, and adding the extratrees option fixed some of it (or I changed that while changing other things and they worked at the same time, at this point I can't remember, it was multiple hours of struggle and pain)
                          min.node.size = 5)
  }else if(i==4) {
    Mygrid <-expand.grid(nrounds = seq(5, 50, by = 10),# took these values from online
                         lambda = c(0, 0.1, 2),
                         alpha = seq(0, 1, by = 0.1),
                         eta = c(0.5, 1))
  }
  
  model<- train(
    work_hours~.,
    data=training,
    method= Methods[i], # can change the method so the loop works
    preProcess= "medianImpute", # you wanted a median imputation
    na.action= na.pass, #we have missing data so we need this
    trControl = trainControl(method = "cv", #what you asked us to do
                             indexOut = fold_indices, #slide 38
                             number = 10,  #10 fold
                             verboseIter = TRUE), # more detailed description
    tuneGrid = Mygrid #the way I got it to actually work
  )
  
  testing[[Methods[i]]] <- list(
    model=model,
    cv_rsq1 = max(model$results$Rsquared, na.rm = TRUE) # pulling out the k-fold r squares from the models and saving them somewhere
  )
  
  predictions <- predict(model, test, na.action = na.pass)
  testing[[Methods[i]]]$ho_rsq1 <- cor(predictions, test$work_hours)^2 #calculating and HO r squares and saving them for later use
}

# Publication
table1_tbl <- tibble(
  algo= as.character(), #template/ base tible
  cv_rsq=as.character(),
  ho_rsq=as.character()
)
i=1 # this is just here because I was testing stuff (plus I think it's needed so I becomes a variable that recognisable, but that might not be the case, and is only the case when my loop is broke (which it isn't right now [figers crossed]))
for (i in 1:length(Methods)){ 
  cv_rsq <- str_remove(formatC(testing[[Methods[i]]]$cv_rsq1, #formatting the k fold r squared
                               format = 'f', 
                               digits = 2),
                       "^0")
  ho_rsq <- str_remove(formatC(testing[[Methods[i]]]$ho_rsq1, #formatting the OH r squared
                               format = 'f', 
                               digits = 2),
                       "^0")
  working <- tibble( # putting it in a tibble
    algo = Methods[i],
    cv_rsq = cv_rsq,
    ho_rsq = ho_rsq)
  table1_tbl <- bind_rows(table1_tbl, working) #combining the tibbles across iterations
}

table1_tbl # double checked how it looks at the end


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
