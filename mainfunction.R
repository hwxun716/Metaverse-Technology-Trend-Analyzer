#install.packages('rsconnect')
#setwd("/Users/Giselle/Desktop/4100package")
library(dplyr)
library(tidytext)
library(ggplot2)
library(textstem)
library(readr)
library(stringr)
library(rvest)
library(tidyverse)
library(RCurl)
library(tidyr)
library(wordcloud)

library(rsconnect)

CountTech <- function(){
  
  jobtitles <- read.csv("Job_titles.csv", TRUE,",")
  technologylist <- read.csv("technologies_list.csv", TRUE,",")
  
  categories <- unique(technologylist$Category)
  
  countKeyword <- function(text_to_view,keyword){
    # a function to count how many times the keyword in the technology list
    # appears in the text-to-view. 
    text_to_view <- tolower(text_to_view)
    keyword <- tolower(keyword)
    ct = 0 # count of keyword
    if (nchar(keyword) < 5){
      ct = ct + str_count(text_to_view,
                          fixed(paste0(' ',keyword,'.'))
      )
      ct = ct + str_count(text_to_view,
                          fixed(paste0(' ',keyword,','))
      )
      ct = ct + str_count(text_to_view,
                          fixed(paste0(' ',keyword,' '))
      )
    } else if (str_count(keyword,fixed('/'))){
      kw = str_split(keyword,'/',simplify = T)
      for (i in kw){
        ct = ct + countKeyword(text_to_view,str_trim(i))
      }
    }
    else {
      ct = str_count(text_to_view,fixed(keyword))
    }
    ct
  }
  
  #search job
  searchJob <- function(job){
    #define base terms
    base_url <- 'https://www.cybercoders.com/'
    page <- 0
    
    #define variables as data container
    job_title <- character()
    job_skill <- character()
    job_location <- character()
    job_salary <- character()
    job_company <- character()
    
    
    #loop the pages and break whenever no more reocords can be found
    while(TRUE) {
      page = page+1
      #url <- getURL('https://www.cybercoders.com/search/?page=1&searchterms=data+scientist&searchlocation=&newsearch=true&originalsearch=true&sorttype=')
      url <- getURL(str_c(base_url,'search/?page=',page,'&searchterms=',URLencode(job),'&searchlocation=&newsearch=true&originalsearch=true&sorttype='))
      html_raw <- read_html(url)
      
      #locate the element with attribute class = job-title
      job_title_page <- html_raw %>%
        html_nodes("[class='job-title']") %>%
        html_text() %>%
        str_remove_all('\\r\\n') %>%
        str_trim()
      
      #locate the element with attribute class = skill-list, concatenate all skills into one text strings
      job_skill_page <- html_raw %>%
        html_nodes("[class='skill-list']") %>% 
        lapply(function(x) html_nodes(x, "[class='skill-name']")) %>%
        lapply(function(x) html_text(x)) %>%
        lapply(function(x) str_c(x,collapse = ', ')) %>%
        unlist()
      
      #locate the element with attribute class = location
      job_location_page <- html_raw %>%
        html_nodes("[class='location']") %>%
        html_text() 
      
      #locate the element with attribute class = wage
      job_salary_page <- html_raw %>%
        html_nodes("[class='wage']") %>%
        html_text() %>%
        str_replace('[[:alpha:][:punct:]]+ (.+)$','\\1') %>%
        str_remove_all(' ')
      
      
      # break the loop when no more record can be found
      if (length(job_title_page)==0) {
        page = page -1
        break
      }
      
      #store data
      job_title <- c(job_title, job_title_page)
      job_skill <- c(job_skill, job_skill_page)
      job_location <-c(job_location, job_location_page)
      job_salary <-c(job_salary, job_salary_page)
      #No info at this website, but keep this variable as scrapping other websites do
      job_company <- c(job_company, rep(NA,length(job_title_page)))
      
      #print progress
      print(str_c(' Scrapping for Page: ',page, ' is done!'))
      
    }
    #consolidate all varialbles into a dataframe
    df_cyber_coders <- as.data.frame(cbind(job_title,job_skill, job_company, job_location, job_salary), stringsAsFactors = FALSE)
    
    str(df_cyber_coders)
    df_cyber_coders
  }# definition of searchJob ends
  
  
  # table for counting freq. of all techs
  tech=unique(technologylist$Technology)
  count_tech_total=numeric(length(tech))
  for (job in jobtitles$Job_Title){  # Run it one by one by change the code to: for (job in jobtitles$Job_Title[1])
    desp_text <- searchJob(job) %>% paste(collapse = '')
    count_tech=numeric(length(tech))
    for (i in 1:length(tech)){
      count_tech[i] = countKeyword(desp_text,tech[i]) # count keyword in that job's descriptions
    }
    count_tech_total <- count_tech_total + count_tech
  }
  
  
  
  df = data.frame(tech,count_tech_total)
  df %>% write.csv('technologies_counted.csv',row.names = F)
  
  # from df to fill the freq. in technologylist
  for (i in categories){
    tech_in_cate = technologylist[technologylist$Category==i,'Technology']
    for (j in tech_in_cate){
      technologylist[technologylist$Category==i & technologylist$Technology==j,'Frequency']=df[df$tech==j,'count_tech_total']
    }
  }
  technologylist %>% write.csv('freq.csv')
  
  # plot the top 10 hot technologies
  df[order(-df$count_tech_total),][1:10,] %>% 
    ggplot(aes(x=reorder(tech,desc(count_tech_total)),y=count_tech_total))+ 
    geom_bar(stat = "identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1))+ 
    labs(title="Mentioned times of technologies in job descriptions", 
         x="Technology", y = "Frequency")
  
}



