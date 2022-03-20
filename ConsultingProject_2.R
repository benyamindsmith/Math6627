## ----echo=FALSE, fig.cap="There is little to no missing data in this dataset", message=FALSE, warning=FALSE----
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


## ----eval=FALSE, tidy=TRUE, tidy.opts=list(width.cutoff=60)------------------------------
## [{'id': 3, 'name': 'Courageous', 'count': 139}, {'id': 2, 'name': 'Confusing', 'count': 25}, {'id': 1, 'name': 'Beautiful', 'count': 48}, {'id': 9, 'name': 'Ingenious', 'count': 31}, {'id': 21, 'name': 'Unconvincing', 'count': 35}, {'id': 11, 'name': 'Longwinded', 'count': 21}, {'id': 8, 'name': 'Informative', 'count': 218}, {'id': 10, 'name': 'Inspiring', 'count': 113}, {'id': 22, 'name': 'Fascinating', 'count': 44}, {'id': 25, 'name': 'OK', 'count': 51}, {'id': 23, 'name': 'Jaw-dropping', 'count': 35}, {'id': 24, 'name': 'Persuasive', 'count': 112}, {'id': 7, 'name': 'Funny', 'count': 9}, {'id': 26, 'name': 'Obnoxious', 'count': 11}]


## ----eval=FALSE, tidy=TRUE, tidy.opts=list(width.cutoff=60)------------------------------
## [{'id': 127, 'hero': 'https://pe.tedcdn.com/images/ted/5cd871dcf27ba4288021c2bfe6a3f6796dab2538_2880x1620.jpg', 'speaker': 'Ngozi Okonjo-Iweala', 'title': 'Want to help Africa? Do business here', 'duration': 1213, 'slug': 'ngozi_okonjo_iweala_on_doing_business_in_africa', 'viewed_count': 1044183}, {'id': 1929, 'hero': 'https://pe.tedcdn.com/images/ted/82bbf525e7b13a879e6b7299303ec510f7ceb9fb_1600x1200.jpg', 'speaker': 'Michael Metcalfe', 'title': 'We need money for aid. So let’s print it.', 'duration': 864, 'slug': 'michael_metcalfe_we_need_money_for_aid_so_let_s_print_it', 'viewed_count': 756965}, {'id': 584, 'hero': 'https://pe.tedcdn.com/images/ted/98530_800x600.jpg', 'speaker': 'Paul Collier', 'title': 'New rules for rebuilding a broken nation', 'duration': 994, 'slug': 'paul_collier_s_new_rules_for_rebuilding_a_broken_nation', 'viewed_count': 406525}, {'id': 1196, 'hero': 'https://pe.tedcdn.com/images/ted/7bb5389d0360ef7905de6b6a017b7ce836ad673d_800x600.jpg', 'speaker': 'Rory Stewart', 'title': 'Time to end the war in Afghanistan', 'duration': 1202, 'slug': 'rory_stewart_time_to_end_the_war_in_afghanistan', 'viewed_count': 659270}, {'id': 270, 'hero': 'https://pe.tedcdn.com/images/ted/1cffd7f06b5754232bc90a0ca15b1339487d7200_2400x1800.jpg', 'speaker': 'Paul Collier', 'title': 'The \"bottom billion\"', 'duration': 1011, 'slug': 'paul_collier_shares_4_ways_to_help_the_bottom_billion', 'viewed_count': 990214}, {'id': 2806, 'hero': 'https://pe.tedcdn.com/images/ted/f26393b438dfc2ed8c8ae66d0c7291ac08629153_2880x1620.jpg', 'speaker': 'Jim Yong Kim', 'title': \"Doesn't everyone deserve a chance at a good life?\", 'duration': 1332, 'slug': 'jim_yong_kim_doesn_t_everyone_deserve_a_chance_at_a_good_life', 'viewed_count': 1341183}]


## ----eval=FALSE, tidy=TRUE, tidy.opts=list(width.cutoff=60)------------------------------
## ['business', 'corruption', 'culture', 'economics', 'entrepreneur', 'global development', 'global issues', 'investment', 'military', 'policy', 'politics', 'poverty']


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------------------------

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


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------------------------
# Need to fix dates
fullyJoinedDf<- ratingsDf %>%
                left_join(tagsDf,by= "url") %>% 
                left_join(dt, by="url") %>% 
                select(!c(description,tags,related_talks,ratings)) %>% 
                mutate(
                       film_date = lubridate::as_datetime(film_date),
                       published_date=lubridate::as_datetime(published_date))
              


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------------------------
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


## ----message=FALSE, warning=FALSE, include=FALSE-----------------------------------------
y <- dtt$views
X <- data.matrix(dtt[,-which(names(dtt) %in% c("views"))])


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap= "MSE vs $\\lambda_{views}$"------
#perform k-fold cross-validation to find optimal lambda value
cv_model_views <- cv.glmnet(X, y, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda<- cv_model_views$lambda.min

#produce plot of test MSE by lambda value
plot(cv_model_views, main =expression("MSE vs "*lambda[views]*""))


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
best_model_views <- glmnet(X, y, alpha = 1, lambda = best_lambda)

as.matrix(coef(best_model_views),rownames) %>% 
  as.data.frame.matrix() %>% 
  filter(s0!=0) %>% 
  knitr::kable(caption="Sparse Estimates for Ted Talk Popularity (in terms of Views)")


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
tibble(`Rating Tag` = unique(fullyJoinedDf$ratingTag),
       Classification = c("Good","Good","Good",
                          "Good","Bad","Bad",
                          "Good","Good","Bad",
                          "Good","Good","Ambiguos",
                          "Bad","Bad")) %>%
  knitr::kable(caption="Unique Rating Tags accross all Ted Talks")


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
dtt %>%
group_by(`Good/Bad Ratio`) %>% 
arrange(-desc(`Good/Bad Ratio`),.by_group = T) %>%
select(name,`Good/Bad Ratio`,views,comments,published_date) %>% 
filter(`Good/Bad Ratio` < 0.6083570) %>% 
knitr::kable(caption="Top 10 Worst Ted Talks")


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
dtt %>%
group_by(`Good/Bad Ratio`) %>% 
arrange(desc(`Good/Bad Ratio`)) %>%
select(name,`Good/Bad Ratio`,views,comments,published_date) %>% 
filter(`Good/Bad Ratio` >= 18.714285) %>% 
knitr::kable(caption="Top 10 Best Ted Talks")  


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
# Comments

y <- dtt$comments
X <- data.matrix(dtt[,-which(names(dtt) %in% c("comments"))])


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap= "MSE vs $\\lambda_{comments}$"----
#perform k-fold cross-validation to find optimal lambda value
cv_model_comments <- cv.glmnet(X, y, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda<- cv_model_comments$lambda.min

#produce plot of test MSE by lambda value
plot(cv_model_comments, main =expression("MSE vs "*lambda[comments]*""))


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
best_model_comments <- glmnet(X, y, alpha = 1, lambda = best_lambda)

as.matrix(coef(best_model_comments),rownames) %>% 
  as.data.frame.matrix() %>% 
  filter(s0!=0) %>% 
  knitr::kable(caption="Sparse Estimates for Ted Talk Popularity (in terms of comments)")


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Characteristics Predicting Popularity over time in terms of views"----
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


## ----echo=FALSE, message=FALSE, warning=FALSE,fig.cap ="Characteristics Predicting Popularity over time in terms of comments"----
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


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------
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


## ----echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------

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

