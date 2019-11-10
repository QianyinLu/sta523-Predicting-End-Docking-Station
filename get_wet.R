library(tidyverse)

key<-"9e3c829d6ef2609428893d0252751748"

st<-as.Date("2013-1-1")

len<-as.Date("2017-12-31")-st

dates<-sapply(0:len, function(i){
  (st+i) %>% as.character()
})

if(!dir.exists("data")){
  dir.create("data")
}

if(!dir.exists("data/weather")){
  dir.create("data/weather")
}

for(i in 1000:1826){
  qr<-paste0("https://api.darksky.net/forecast/9e3c829d6ef2609428893d0252751748/38.9070412,-77.0376289,",dates[i],"T00:00:00?units=si&exclude=minutely")
  fname<-paste0("data/weather/",dates[i],".json")
  download.file(qr,fname)
}