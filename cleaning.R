# get raw file from github
# note: to do this in the future, click on the "raw" option in github  on the page containing the csv file 
# and copy the URL of the page you're redirected to
raw <- read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/raw.csv", header = T)

# Order Games by date
date_ordered <- raw[order(as.Date(raw$game_date, format="%m/%d/%Y")),]

# create new data frame with normalized dates
date_normal = date_ordered
date_normal$game_date = as.Date(date_normal$game_date, format="%m/%d/%Y")
date_normal$normal_date = 0

# loop through and normalize the dates
date = date_normal$game_date[1]
counter = 1
for (i in 1:length(date_normal$game_date)){
  date1 = date_normal$game_date[i]
  if (date1 != date) {
    date = date1
    counter = counter + 1
  }
  date_normal$normal_date[i] = counter
}

# make sure normalized dates are sorted
is.unsorted(date_normal$normal_date) # Should return false

# calculate percentage of shots made each game and merge into date_normal data frame
dates = subset(date_normal, select = c(normal_date,shot_made_flag))
dates_unique = unique(subset(dates, select = c(normal_date)))
dates_unique$avg = 0
dates_unique$shots_made = 0
dates_unique$shots_taken = 0
for (i in 1:length(dates_unique$normal_date)){
  shots_attempted = subset(dates, normal_date == i)$shot_made_flag
  dates_unique$shots_made[i] = sum(shots_attempted)
  dates_unique$avg[i] = mean(shots_attempted)
  dates_unique$shots_taken[i] = length(shots_attempted)
}

# add avg and total shots made to date_normal dataframe
date_normal = merge(date_normal, dates_unique, by="normal_date")

# Calculate clutch shooting percentages 
# specify shots taken with <= clutch_threshold minutes remaining 
clutch_threshold = 1
dates_unique$clutch_perc = 0
dates_unique$clutch_shots_made = 0
dates_unique$clutch_shots_taken = 0
for (i in 1:length(dates_unique$normal_date)){
  clutches_attempted = subset(date_normal, 
                        normal_date == i & minutes_remaining <= clutch_threshold)$shot_made_flag
  dates_unique$clutch_perc[i] = mean(clutches_attempted)
  dates_unique$clutch_shots_made[i] = sum(clutches_attempted)
  dates_unique$clutch_shots_taken[i] = length(clutches_attempted)
}
clutch_shooting = subset(dates_unique, select = c(normal_date, clutch_perc, clutch_shots_made, clutch_shots_taken))
# filter out NaNs from clutch_shooting percentages
clutch_shooting = subset(clutch_shooting, 
                         !is.nan(clutch_perc) & !is.nan(clutch_shots_made) & !is.nan(clutch_shots_taken))
