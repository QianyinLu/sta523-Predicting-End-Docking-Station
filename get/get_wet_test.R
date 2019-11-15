library(tidyverse)

key<-"9e3c829d6ef2609428893d0252751748"

st<-as.Date("2018-1-1")

len<-as.Date("2018-12-31")-st

dates<-sapply(0:len, function(i){
  (st+i) %>% as.character()
})

if(!dir.exists("data")){
  dir.create("data")
}

if(!dir.exists("data/weather")){
  dir.create("data/weather")
}

for(date in dates){
  qr<-paste0("https://api.darksky.net/forecast/9e3c829d6ef2609428893d0252751748/38.9070412,-77.0376289,",date,"T00:00:00?units=si&exclude=minutely")
  fname<-paste0("data/weather/",date,".json")
  download.file(qr,fname)
}