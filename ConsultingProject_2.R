library(tidyverse)
library(stringr)
library(stringi)
library(glmnet)
library(lme4)
library(jsonlite)
library(nlme)
library(yaml)
library(tidyr)
dt<- readr::read_csv('./ted_main.csv')

# No missing data
naniar::vis_miss(dt)+
  theme(axis.text.x = element_text(angle = 90))

######################
## Data Engineering ##
######################

ratingsDf<- dt$ratings %>% 
            lapply(function(x) read_yaml(text=x)) %>% 
            lapply(function(x) do.call(rbind,x))

# Using a for loop because vectorizing is hard. Don't make fun of me

for(i in 1:length(ratingsDf)){
  
  ratingsDf[[i]]<- ratingsDf[[i]] %>% 
                   as.data.frame.matrix() %>% 
                   transmute(ratingID= id,
                             ratingTag = name,
                             count=count,
                             url= rep(dt$url[i],length(ratingsDf[[i]][,1])))
  
}

ratingsDf<-do.call(rbind,ratingsDf) %>% unnest()

# Manually editing JSON Files to be read into R
# The titles may differ as such
setwd("Proj2JsonFiles")
for(i in 1:length(dt$related_talks)){
 
  write(dt$related_talks[i] %>%
          str_remove_all("(?<=\\w)\\'(?=\\w)") %>%
          str_remove_all("’") %>%  
          str_remove_all("\\\\'") %>% 
          str_replace("é","e") %>% 
          stri_trans_general("latin-ascii"),paste0(i,'.json'))
}

relatedTalksJSON<- list()

for(i in 1:length(dt$related_talks)){
  tryCatch(
    { relatedTalksJSON[[i]]<- read_yaml(paste0(i,'.json')) },
    error = function(e){
      tryCatch(
        {
          relatedTalksJSON[[i]]<-read_yaml(text=dt$related_talks[i] %>% str_remove_all("\\'"))
        },
        error = function(f) {
           relatedTalksJSON[[i]]<-read_yaml(text=dt$related_talks[i] %>% str_remove_all('\\"') %>% str_remove_all("\\\\'") %>% stri_trans_general("latin-ascii")) 
        },
        finally=i
               )
      }
      ,
    finally=i
  )
}


for(i in 1:length(relatedTalksJSON)){
  
    relatedTalksJSON[[i]]<- relatedTalksJSON[[i]] %>% lapply(function(y) as.data.frame(y) %>% mutate(url=dt$url[i]))
      
  
}

relatedTalksJSONDf<-list()
for (i in 1:length(relatedTalksJSON)){
  relatedTalksJSONDf[[i]]<-do.call(rbind,relatedTalksJSON[i])
}

relatedTalksJSONDf <- relatedTalksJSONDf %>% lapply(function(x) t(x))
relatedTalksJSONDf<-do.call(rbind,do.call(rbind,relatedTalksJSONDf)) %>% 
                    transmute(related_talks_id=id,
                              related_talks_hero=hero,
                              related_talks_speaker=speaker,
                              related_talks_title=title,
                              related_talks_duration=duration,
                              related_talks_slug = slug,
                              related_talks_view_count = viewed_count,
                              url=url)


tagsDf <- dt$tags %>% 
  lapply(function(x) read_yaml(text=x) %>% as_tibble()) 

for(i in 1:length(tagsDf)){
  tagsDf[[i]]<- tagsDf[[i]] %>% 
                   transmute(tagValue=value,
                             url = rep(dt$url[i],length(tagsDf [[i]][,1])))
}

tagsDf<- do.call(rbind,tagsDf)


# Need to fix dates
fullyJoinedDf<- ratingsDf %>%
                left_join(tagsDf,by= "url") %>% 
                left_join(dt, by="url") %>% 
                select(!c(description,tags,related_talks,ratings)) %>% 
                mutate(
                       film_date = lubridate::as_datetime(film_date),
                       published_date=lubridate::as_datetime(published_date))
              

# Working dataframe
dtt<-fullyJoinedDf %>%
      select(!ratingID) %>% 
      pivot_wider(names_from = ratingTag, 
                  values_from = count,
                  names_prefix = "rating_") %>% 
      pivot_wider(names_from=tagValue,
                  values_from=tagValue,
                  names_prefix = "tag_") 

dtt<- dtt %>% 
      mutate(across(names(dtt)[grepl("tag_",names(dtt))],~ ifelse(!is.na(.x),1,0)))


############
# Analysis #
############

y <- dtt$views
X <- data.matrix(dtt[,-which(names(dtt) %in% c("views"))])


#perform k-fold cross-validation to find optimal lambda value
cv_model_views <- cv.glmnet(X, y, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda<- cv_model_views$lambda.min

#produce plot of test MSE by lambda value
plot(cv_model_views, main =expression("MSE vs "*lambda[views]*""))


best_model_views <- glmnet(X, y, alpha = 1, lambda = best_lambda)

as.matrix(coef(best_model_views),rownames) %>% 
  as.data.frame.matrix() %>% 
  filter(s0!=0) %>% 
  knitr::kable(caption="Sparse Estimates for Ted Talk Popularity (in terms of Views)")

tibble(`Rating Tag` = unique(fullyJoinedDf$ratingTag),
       Classification = c("Good","Good","Good",
                          "Good","Bad","Bad",
                          "Good","Good","Bad",
                          "Good","Good","Ambiguos",
                          "Bad","Bad")) %>%
  knitr::kable(caption="Unique Rating Tags accross all Ted Talks")


# Including Good/Bad Ratio
dtt<-dtt %>% 
  rowwise() %>% 
  mutate(`Good/Bad Ratio` = sum(rating_Funny,
                                rating_Beautiful,
                                rating_Ingenious,
                                rating_Courageous,
                                rating_Informative,
                                rating_Fascinating,
                                rating_Persuasive,
                                `rating_Jaw-dropping`)/sum(rating_Longwinded,
                                                           rating_Confusing,
                                                           rating_Unconvincing,
                                                           rating_Obnoxious,
                                                           rating_Inspiring))

dtt %>%
group_by(`Good/Bad Ratio`) %>% 
arrange(-desc(`Good/Bad Ratio`),.by_group = T) %>%
select(name,`Good/Bad Ratio`,views,comments,published_date) %>% 
filter(`Good/Bad Ratio` < 0.6083570) %>% 
knitr::kable(caption="Top 10 Worst Ted Talks")


dtt %>%
group_by(`Good/Bad Ratio`) %>% 
arrange(desc(`Good/Bad Ratio`)) %>%
select(name,`Good/Bad Ratio`,views,comments,published_date) %>% 
filter(`Good/Bad Ratio` >= 18.714285) %>% 
knitr::kable(caption="Top 10 Best Ted Talks")  


# Comments

y <- dtt$comments
X <- data.matrix(dtt[,-which(names(dtt) %in% c("comments"))])

#perform k-fold cross-validation to find optimal lambda value
cv_model_comments <- cv.glmnet(X, y, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda<- cv_model_comments$lambda.min

#produce plot of test MSE by lambda value
plot(cv_model_comments, main =expression("MSE vs "*lambda[comments]*""))


best_model_comments <- glmnet(X, y, alpha = 1, lambda = best_lambda)

as.matrix(coef(best_model_comments),rownames) %>% 
  as.data.frame.matrix() %>% 
  filter(s0!=0) %>% 
  knitr::kable(caption="Sparse Estimates for Ted Talk Popularity (in terms of comments)")


fullyJoinedDf %>% 
  mutate(published_year = lubridate::year(published_date)) %>% 
  group_by(published_year,tagValue) %>% 
  summarize(total_views=sum(views)) %>% 
  slice_max(order_by = total_views, n = 5) %>% 
  ggplot()+
  geom_point(mapping=aes(x=as.factor(published_year),y=total_views,color=tagValue))+
  facet_wrap(~tagValue)+
  ggtitle("Characteristics Predicting Popularity over time (views)")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=90),
        axis.title.y=element_blank(),
        axis.title.x = element_blank())

fullyJoinedDf %>% 
  mutate(published_year = lubridate::year(published_date)) %>% 
  group_by(published_year,tagValue) %>% 
  summarize(total_comments=sum(comments)) %>% 
  slice_max(order_by = total_comments, n = 5) %>% 
  ggplot()+
  geom_point(mapping=aes(x=as.factor(published_year),y=total_comments,color=tagValue))+
  facet_wrap(~tagValue)+
  ggtitle("Characteristics Predicting Popularity over time (comments)")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=90),
        axis.title.y=element_blank(),
        axis.title.x = element_blank())

set.seed(6627)
library(nlme)
fit_views <- lme(views~ duration+ tagValue + languages + comments +duration*tagValue+languages*tagValue+comments*tagValue, 
                 data=fullyJoinedDf%>% 
                   filter(tagValue %in% c("culture","psychology","business","entertainment",
                                          "dance","technology","global issues","design",
                                          "science","TEDx","health","TED Fellows",  
                                          "communication","innovation","society","future",
                                          "humanity","social change")),
                 random= ~ 1|main_speaker)


sum_views<-summary(fit_views)

knitr::kable(sum_views$tTable, caption = "Fixed effects of the views mixed model")

set.seed(6627)

fit_comments <- lme(comments~ duration+ tagValue + languages + views +duration*tagValue+languages*tagValue+views*tagValue, 
                 data=fullyJoinedDf%>% 
                   filter(tagValue %in% c("culture","creativity","dance","children",     
                                           "education","science","technology","religion",    
                                           "God","global issues","entertainment","business",    
                                           "TEDx","philosophy","neuroscience","health"  ,     
                                           "TED Fellows","society","social change","future",     
                                           "innovation","humanity","communication")),
                 random= ~ 1|main_speaker)


sum_comments<-summary(fit_comments)


knitr::kable(sum_comments$tTable, caption = "Fixed effects of the comments mixed model")


## ----eval=FALSE, tidy=TRUE, tidy.opts=list(width.cutoff=60)------------------------------
## 

