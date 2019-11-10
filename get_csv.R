for(i in 2013:2017){
  h<-"http://www2.stat.duke.edu/~sms185/data/bike/"
  fname<-paste0("cbs_",i,".csv")
  
  download.file(paste0(h,fname),fname)
}