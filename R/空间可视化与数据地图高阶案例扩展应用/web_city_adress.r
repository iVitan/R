## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## visualization of geospatial and data map_case and model 

library("httr") 
library("magrittr") 
library("jsonlite")

GetJD <- function(address){
    url = "http://api.map.baidu.com/geocoder/v2/"
    header  <- c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36")
     payload = list(
         'output' = 'json', 
        'ak' = 'X8zlxPUdSe2weshrZ1WqnWxb43cfBI2N'
         )
     addinfo <- data.frame()
     for (i in address){
         payload[["address"]] = i
         tryCatch({
              web <- GET(url,add_headers(.headers = header),query = payload)
             content <- web %>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE) %>% `[[`(2) %>% `[[`(1)
             addinfo <- rbind(addinfo,content)
         },error = function(e){
             cat(sprintf("任务【%s】处理失败!",i),sep = "\n")
             addinfo <- rbind(addinfo,list("lng" = NA ,"lat" = NA))
          })
         Sys.sleep(runif(1))
         cat(sprintf("正在抓取第【%s】个地址",i),sep = "\n")
         }
     cat("所有数据全部抓取完毕!!!",sep = "\n")
     return(cbind(address,addinfo,stringsAsFactors = FALSE)) 
   }



