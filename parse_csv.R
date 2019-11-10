
df<-data.frame()

for(i in 2013:2017){
  fname<-paste0("cbs_",i,".csv")
  
  d<-read.csv(fname)
  
  df<-rbind(df,d)
}

saveRDS(df,"bike.rds")