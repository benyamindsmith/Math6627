## ----message=FALSE, warning=FALSE, include=FALSE-------------------------------------

library(tidyverse)
library(reshape2)
library(readxl)
library(nlme)
library(glmnet)

dt1 <- read_excel("ConsultingData_3.xlsx", sheet=1) 
dt2 <- read_excel("ConsultingData_3.xlsx", sheet=2)

dtt2<-model.matrix(~Gene.Symbol, dt2)
# For now not using joined data
#joined_data<- dt1 %>% left_join(dt2)

dtt <- dt1 %>%
        t() %>% 
        as_tibble() %>% 
        setNames(dt1 %>% 
  t() %>% 
  as_tibble() %>% 
  slice(1:2) %>%
  as.list() %>% 
  lapply(function(x) x[!is.na(x)]) %>% 
  unname() %>%
  unlist()) %>% 
  slice(c(3:n()))

dtt<- dtt %>% 
      mutate(across(names(dtt)[-c(1,3,4)], ~as.numeric(.x)),
             Group = ifelse(Group ==  "Ulcerative" | Group =="Ulcerative Colitis", "Ulcerative Colitis",Group),
             Ethnicity =  ifelse( Ethnicity ==  "cacuasian" | Group =="caucasian", "caucasian",Ethnicity) )

#write.csv(dtt, "ConsultingData_3.csv") 


## ----message=FALSE, warning=FALSE, include=FALSE-------------------------------------
library(factoextra)
pca<- prcomp(data.matrix(dtt[,-1]), center = TRUE, scale = TRUE)

# We can use the first 38 principal components
pca_summary<-summary(pca)

# This agrees with the paper results
pca_summary$importance[,1:38]


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.cap="Mean Square Error of Prediction With PCA Regresion (38 Components)"----
library(pls)
library(caret)
set.seed(1)

pcr_model <- pcr(Group~ ., 
                 data = data.matrix(dtt) %>% as.data.frame.matrix(), 
                 scale = TRUE, 
                 validation = "CV",
                 ncomp=38)


validationplot(pcr_model, val.type=c("MSEP"),main="Mean Square Error of Prediction With PCA Regresion (38 Components)")



## ----echo=FALSE, message=FALSE, warning=FALSE,fig.cap="Predicted vs Measured Values using PCA Regression with 38 Components"----
predplot(pcr_model, main="Predicted vs Measured Values using PCA Regression with\n 38 Components")



## ----message=FALSE, warning=FALSE, include=FALSE-------------------------------------
set.seed(1)
dtt2 <- data.matrix(dtt)
y <- dtt2[,1]
X <- dtt2[,-1]

train = sample(seq(length(y)),75,replace=FALSE)
# 10 -fold crossvalidation
cv_model <- cv.glmnet(X, y, family='multinomial', alpha = 1)
best_lambda<- cv_model$lambda.min


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Multinomial Deviance vs $\\lambda_{Group}$"----
plot(cv_model,main=expression("Multinomial Deviance vs "*lambda[Group]*""))


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Predicted vs True Confusion Matrix (Multinomial Model)"----
library(plyr)
train_model <- glmnet(X[train,], y[train],family='multinomial', alpha = 1, lambda = best_lambda)

confusion.glmnet(train_model, newx = X[-train,], newy = y[-train]) %>% 
    melt() %>% 
  mutate(Predicted = revalue(as.character(Predicted),
                             c(
                               "1"="Crohn's Disease",
                               "2"="Normal",
                               "3"="Ulcerative Colitis")),
         True =  revalue(as.character(True),
                             c(
                               "1"="Crohn's Disease",
                               "2"="Normal",
                               "3"="Ulcerative Colitis"))) %>% 
  ggplot(mapping=aes(x=True,y=Predicted,fill=value,label=value))+
  geom_tile()+
  geom_text()+
  scale_fill_gradient(low = "white", high = "red")+
  ggtitle("Predicted vs True Confusion Matrix (Multinomial Model)\n (Percent Correct:  %76.47 )")+
  theme(legend.position="none")


## ----echo=FALSE----------------------------------------------------------------------
sparseEstimates<-data.frame(
       `Crohn's Disease`=train_model$beta$`1`[,],
       `Normal` =train_model$beta$`2`[,],
       `Ulcerative Colitis`=train_model$beta$`3`[,])

colnames(sparseEstimates)<-c("Crohn's Disease","Normal","Ulcerative Colitis")

sparseEstimates %>% 
  filter(!(`Crohn's Disease`==0 &`Normal`==0 & `Ulcerative Colitis`==0)) %>% 
  knitr::kable(caption = "Sparse Estimates for Multinomial Classification of Groups")


## ------------------------------------------------------------------------------------
sparseEstimates %>% 
    select(!(`Normal`)) %>% 
    filter(!(`Crohn's Disease`==0 & `Ulcerative Colitis`==0))%>% 
  knitr::kable(caption = "Sparse Estimates of Crohn's disease and Ulcerative Collitis Predictors")


## ----eval=FALSE----------------------------------------------------------------------
## 

