library(tidyverse)
library(vroom)
#devtools::install_github("tidyverse/multidplyr")
# make sure all other packages are up to date first
library(multidplyr)
library(lubridate)
library(data.table)
library(profvis)

p <-profvis ({
base_url = "http://www2.stat.duke.edu/~sms185/data/bike/"
files = c("cbs_2013.csv", "cbs_2014.csv", "cbs_2015.csv", "cbs_2016.csv", "cbs_2017.csv")

cbs_names = c("duration", "start_date", "end_date", "start_station_number", 
              "start_station","start_station_number", "start_station", 
              "bike_number","member_type")

# code from slides
clust <- multidplyr::new_cluster(3)
multidplyr::cluster_assign_partition(clust, file_name = paste0(base_url, files))
multidplyr::cluster_send(clust, cbs_data <- vroom::vroom(file_name))
cbs <- multidplyr::party_df(clust, "cbs_data")

cbs = cbs %>%
  dplyr::rename(duration = Duration,
                start_date = `Start date`,
                end_date = `End date`,
                start_station_number = `Start station number`,
                start_station = `Start station`,
                end_station_number = `End station number`,
                end_station = `End station`,
                member_type = `Member type`,
                bike_number = `Bike number`)

cbs = cbs %>% mutate(year = lubridate::year(start_date),
                     month = lubridate::month(start_date),
                     day = lubridate::day(start_date),
                     hour = lubridate::hour(start_date),
                     # wday returns 2~6 for Monday~Friday
                     weekday = lubridate::wday(start_date) %in% 2:6)

cbs_dt = data.table(collect(cbs))

weather = readRDS("weather.rds")
weat_cond = weather %>% 
  select(year,month,day,hour,weekday,icon,temperature) %>% 
  #weekday 1~5 are Monday~Friday
  mutate(weekday = weekday %in% 1:5,
         icon = recode(icon,
                       `clear-day` = "nice",
                       `clear-night` = "nice",
                       `cloudy` = "nice",
                       `fog` = "bad",
                       `partly-cloudy-day` = "nice",
                       `partly-cloudy-night` = "nice",
                       `rain` = "terrible",
                       `sleet` = "terrible",
                       `snow` = "terrible"))

cbs_dt = merge(cbs_dt,weat_cond,all.x=T,sort = F)

setkey(cbs_dt, start_station, duration, end_station)

get_similar_rows <- function(row, input_dt, duration_minus = 0.2,duration_plus=0.2,
                             month_delta = 2, hour_delta=2, temp_delta = 10) {
  # filter the data.table to rows with:
  # - the same start station
  # - durations in [1-duration_delta, 1+duration_delta]*actual_duration
  # - months within +- month_delta
  # - hours withim +- hour delta
  
  
  same_start = cbs_dt[J(row$start_station), nomatch=0L]
  
  same_start[T
             & duration %between% c((1-duration_minus)*row$duration, (1+duration_plus)*row$duration)
             #& abs(month - row$month)<= month_delta
             & abs(hour-row$hour) <= hour_delta
             & weekday == row$weekday
             #& icon == row$icon
             #& abs(temperature - row$temperature) <= temp_delta
             ]
}

make_predictions <- function(similar_rows, all_stops) {
  # takes in a list of similar rows
  # assigns each unique stop the observed probability
  # assigns any missing stops zero probability
  
  nonzero_probs = as.data.frame(table(similar_rows$end_station)/nrow(similar_rows))
  predictions = spread(nonzero_probs, key=Var1, value=Freq)
  
  zero_probs = all_stops[!(all_stops %in% names(predictions))]
  predictions[zero_probs] = 0
  
  predictions
}

prediction_wrapper = function(row, input_dt, features, all_stops, ordered_names) {
  # takes in a row from the test_set
  # computes and appends predictions
  
  similar_rows = get_similar_rows(row, input_dt)
  
  if(nrow(similar_rows)==0){
    # if there are are no similar rows in our data, just guess
    return(row[,ordered_names])
  }
  
  # compute and append predictions
  predictions = make_predictions(similar_rows, all_stops)
  output_row = cbind(row[features], predictions)
  
  # return everything in the same order to make rbind faster
  output_row[,ordered_names]
}

test_set = vroom::vroom("http://www2.stat.duke.edu/~sms185/data/bike/cbs_test.csv")
test_wet = test_set %>% mutate(year = lubridate::year(start_date),
                               month = lubridate::month(start_date),
                               day = lubridate::day(start_date),
                               hour = lubridate::hour(start_date),
                               # wday returns 2~6 for Monday~Friday
                               weekday = lubridate::wday(start_date) %in% 2:6)
test_wet = merge(test_wet,weat_cond,all.x = T,sort = F) %>% arrange(start_date)

ordered_names = names(test_set)

exclude_features = c("year","month","day","hour","weekday","icon","temperature")
ordered_names = ordered_names[!(ordered_names %in% exclude_features)]
features = c("start_date","end_date","duration","start_station_number",
             "start_station","bike_number", "member_type")
all_stops = names(test_set)[!(names(test_set) %in% c(features,exclude_features))]

prediction_wrapper(test_wet[500,], cbs_dt, features, all_stops, ordered_names)

final_predictions = plyr::adply(test_wet, 1, function(x) prediction_wrapper(x, cbs_dt, features, all_stops, ordered_names),
                                .progress = "text")
final_predictions = final_predictions %>% select(-exclude_features) 
#write_csv(final_predictions, "cbs_git-r-done.csv")

write_csv(final_predictions, "cbs_git-r-done.csv")
})





