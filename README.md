# R-scraping-using-Jsonlite

---
title: "R scraping using Jsonlite"
author: "ahmed"
date: '2022-06-20'
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R scraping using jsonlite library

this code shows how to extract json tables and performing primary data analysis tasks 

in this example we use data from 
<https://www.topuniversities.com/university-rankings/world-university-rankings/2018>

start with **inspect** > **network** > **fetch/XHR** > **copy risponse**


```{r library,echo=TRUE,warning=FALSE}
library(rvest)
library(xml2)
library(jsonlite)
library(tidyverse)
library(ggrepel)
```

## gather the data
gather the data with some wrangling

```{r gather data, echo=TRUE,warning=FALSE}
XHR<- "https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/357051.txt?rc4i6a"
res<- jsonlite::fromJSON(XHR)$data[, c(
  "rank_display", "score", "title", "country", "region" )]
sep<-function(s){
str_remove_all(s, '<div class=\"td-wrap\"><a href=\"')%>%str_remove('</a></div>')%>% 
  paste0("https://www.topuniversities.com",.)%>%
  str_replace_all(.,pattern = '\" class=\"uni-link\">',replacement = ' ')%>%
  data.frame(x=.,title=s,stringsAsFactors = FALSE)%>%
  separate(x, c("website", "name"), extra = "merge",sep = " " ,fill = "left")
}
res1<- sapply(res$title,sep)
res1<- as.data.frame(t(res1),optional = TRUE)
rownames(res1) <- c()

results<- merge(res,res1,by="title")
results<-results%>% select(name,rank_display,score,country,region,website)

results$score<- as.numeric(results$score)
results<- results%>% filter(!is.na(score))
```
## plotting 

then we can create informative plots 
**mean scores**
```{r plotting, echo=TRUE}
results%>%group_by(country,region)%>%summarize(mean_score= mean(score))%>%ungroup()%>%
  mutate(country = reorder(country, mean_score)) %>%
  ggplot(aes(country, mean_score,fill=region)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 7)) +
  xlab("")
```
![image](https://user-images.githubusercontent.com/36051221/174509130-8e1a9899-f628-4231-b4fe-bb738b025d17.png)



we can also see the change of the score of each of top 20 universities of **2018** in **2022**   
```{r top_uni_2018, echo=TRUE}
results_2018_top<- results%>%top_n(.,20,score)
XHR_2<- "https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3740566.txt?rdin9r" ##same as we did in 2018 data
res_1<- jsonlite::fromJSON(XHR_2)$data[, c(
  "rank_display", "score", "title", "country", "region" )]
res_2<- sapply(res_1$title,sep)
res_2<- as.data.frame(t(res_2),optional = TRUE)
rownames(res_2) <- c()
results_2022<- merge(res_1,res_2,by="title")
results_2022<-results_2022%>% select(name,rank_display,score,country,region,website)
results_2022$score<- as.numeric(results_2022$score)
results_2022<- results_2022%>% filter(!is.na(score))

re_wide<- inner_join(results_2018_top,results_2022,by=c("name","region","website","country")) %>%
  rename("2018" = score.x ,"2022" = score.y)%>%
  select(name, "2018","2022",country,region)
re_wide%>%
ggplot(aes(x= "2018",xend="2022", y= `2018` ,yend=`2022`,
color= region,group=country))+geom_segment()+
geom_text_repel(aes(label=name,x="2018",y=`2018`), size = 2,nudge_x = -.35)+
geom_label(aes(label=`2022`,x="2022",y=`2022`), size = 2.5,nudge_x = 0.08)+
geom_label(aes(label=`2018`,x="2018",y=`2018`), size = 2.5,nudge_x =- 0.08)+
ylab("score") +xlab("year")

```
![image](https://user-images.githubusercontent.com/36051221/174509288-da2605a8-65ae-4230-a999-5202a5b77b08.png)
