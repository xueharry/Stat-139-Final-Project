# get raw file from github
# note: to do this in the future, click on the "raw" option in github  on the page containing the csv file 
# and copy the URL of the page you're redirected to
raw <- read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/raw.csv", header = T)

# Order Games by date
date_ordered <- raw[order(as.Date(raw$game_date, format="%m/%d/%Y")),]

# create new data frame with normalized dates
date_normal = date_ordered
# reformat data into more R-readable format
date_normal$game_date_formatted = as.Date(date_normal$game_date, format="%m/%d/%Y")
date_normal$game_number = 0

# loop through and normalize the dates
date = date_normal$game_date_formatted[1]
counter = 1
for (i in 1:length(date_normal$game_date_formatted)){
  date1 = date_normal$game_date_formatted[i]
  if (date1 != date) {
    date = date1
    counter = counter + 1
  }
  date_normal$game_number[i] = counter
}

# make sure normalized dates are sorted
is.unsorted(date_normal$game_number) # Should return false

# calculate percentage of shots made each game and merge into date_normal data frame
dates = subset(date_normal, select = c(game_number,shot_made_flag))
dates_unique = unique(subset(dates, select = c(game_number)))
dates_unique$avg = 0
dates_unique$shots_made = 0
dates_unique$shots_taken = 0
for (i in 1:length(dates_unique$game_number)){
  shots_attempted = subset(dates, game_number == i)$shot_made_flag
  dates_unique$shots_made[i] = sum(shots_attempted)
  dates_unique$avg[i] = mean(shots_attempted)
  dates_unique$shots_taken[i] = length(shots_attempted)
}

# Calculate clutch shooting percentages 
# specify shots taken with <= clutch_threshold minutes remaining 
clutch_threshold = 1
dates_unique$clutch_perc = 0
dates_unique$clutch_shots_made = 0
dates_unique$clutch_shots_taken = 0
for (i in 1:length(dates_unique$game_number)){
  clutches_attempted = subset(date_normal, 
                        game_number == i & minutes_remaining <= clutch_threshold)$shot_made_flag
  dates_unique$clutch_perc[i] = mean(clutches_attempted)
  dates_unique$clutch_shots_made[i] = sum(clutches_attempted)
  dates_unique$clutch_shots_taken[i] = length(clutches_attempted)
}
clutch_shooting = subset(dates_unique, select = c(game_number, clutch_perc, clutch_shots_made, clutch_shots_taken))
# filter out NaNs from clutch_shooting percentages
clutch_shooting = subset(clutch_shooting, 
                         !is.nan(clutch_perc) & !is.nan(clutch_shots_made) & !is.nan(clutch_shots_taken))
 
# Make overtime dummy variable
dates_unique$ot = 0
dates_unique$ot_taken = 0
dates_unique$ot_made = 0
dates_unique$ot_avg = 0
for (i in 1:length(dates_unique$game_number)){
  overtime = subset(date_normal, 
                    game_number == i & period > 4)$shot_made_flag
  if (length(overtime) == 0) {
    dates_unique$ot[i] = 0
  }
  else {
    dates_unique$ot[i] = 1
  }
  dates_unique$ot_taken[i] = length(overtime)
  dates_unique$ot_made[i] = sum(overtime)
  dates_unique$ot_avg[i] = mean(overtime)
}

# Rename column 
colnames(date_normal)[27] <- "win"

# add avg and total shots made to date_normal dataframe
# write.csv(date_normal, "/Users/ChrisChen/Desktop/cleaned.csv")
date_normal = merge(date_normal, dates_unique, by="game_number")