library(tidyverse)

st<-as.Date("2013-1-1")

len<-as.Date("2018-12-31")-st

dates<-sapply(0:len, function(i){
  (st+i) %>% as.character()
})

weather=data.frame()

for(date in dates){
  fname<-paste0("data/weather/",date,".json")
  wet<-jsonlite::read_json(fname)
  timed<-wet$hourly$data %>% map_df(`[`) %>% 
    mutate(time=as.POSIXlt(time, origin = "1970-01-01") %>% format("%Y-%m-%d-%H-%w")) %>% 
    separate(time,c("year","month","day","hour","weekday"),sep="-") %>% as.data.frame()
  weather=plyr::rbind.fill(weather,timed)
}

saveRDS(weather,"data/weather.rds")