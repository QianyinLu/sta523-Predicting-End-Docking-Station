library(tidyverse)
library(vroom)
library(multidplyr)
library(lubridate)

files=paste0("cbs_",2013:2017,".csv")

cbs_names = c("duration", "start_date", "end_date", "start_station_number", 
              "start_station","start_station_number", "start_station", 
              "bike_number","member_type")

# code from slides
clust <- multidplyr::new_cluster(4)
multidplyr::cluster_assign_partition(clust, file_name = files)
multidplyr::cluster_send(clust, cbs_data <- vroom::vroom(file_name))
cbs <- multidplyr::party_df(clust, "cbs_data")

#code from steven
cbs = cbs %>%
  rename(duration = Duration,
         start_date = `Start date`,
         end_date = `End date`,
         start_station_number = `Start station number`,
         start_station = `Start station`,
         end_station_number = `End station number`,
         end_station = `End station`,
         member_type = `Member type`,
         bike_number = `Bike number`) %>% 
  mutate(year=year(start_date)) %>%
  mutate(month=month(start_date)) %>%
  mutate(day=day(start_date)) %>%
  mutate(hour=hour(start_date)) %>%
  mutate(weekday=wday(start_date)-1) %>%
  select(-end_date,-start_date)

cbs_collected = collect(cbs)

saveRDS(cbs_collected,"bike.rds")