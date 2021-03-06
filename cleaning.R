# cleaning.R
# Data preprocessing and cleaning

# get raw file from github
# note: to do this in the future, click on the "raw" option in github  on the page containing the csv file 
# and copy the URL of the page you're redirected to
raw = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/raw.csv", header = T)
wl = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/win_loss.csv", header = T)
# grab only the columns we need
wl = subset(wl, select=c(date,win.loss))
wl$win = 0
# format date into R-readable format
wl$game_date_formatted <- as.Date(wl$date,  format = "%B %d, %Y")
for (i in 1:length(wl$win.loss)){
  win = toString(wl$win.loss[i])
  if (win == "W") {
    wl$win[i] = 1
  }
}
wl = subset(wl, select=c(-win.loss, -date))
# Order Games by date
date_normal <- raw[order(as.Date(raw$game_date, format="%m/%d/%y")),]

# reformat data into more R-readable format
date_normal$game_date_formatted = as.Date(date_normal$game_date, format="%m/%d/%y")
date_normal = subset(date_normal, select=-win.loss)
date_normal = merge(date_normal, wl, by="game_date_formatted")
date_normal$game_number = 0

# Put in indicators for whether a game was at home or away
# Initialize to 1
date_normal$home = 1
# Change away games to 0
date_normal[grepl("@", date_normal$matchup) == TRUE, "home"] = 0

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
dates = subset(date_normal, select = c(game_number,shot_made_flag,win))
dates_unique = unique(subset(dates, select = c(game_number,win)))
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
                        game_number == i & ((minutes_remaining <= clutch_threshold & period == 4) | period > 4))$shot_made_flag
  dates_unique$clutch_perc[i] = mean(clutches_attempted)
  dates_unique$clutch_shots_made[i] = sum(clutches_attempted)
  dates_unique$clutch_shots_taken[i] = length(clutches_attempted)
}
dates_unique$non_clutch_perc = (dates_unique$shots_made - dates_unique$clutch_shots_made) / (dates_unique$shots_taken - dates_unique$clutch_shots_taken)
clutch_shooting = subset(dates_unique, select = c(game_number, win, clutch_perc, clutch_shots_made, clutch_shots_taken))
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

# Dummy variable for 3-pointers
date_normal$three_pointer = 1
date_normal[grepl("2PT Field Goal", date_normal$shot_type) == TRUE, "three_pointer"] = 0

# Dummy variables for combined_shot_type, 5 total, base is layup
date_normal$jump_shot = 0
date_normal$dunk = 0
date_normal$tip_shot = 0
date_normal$hook_shot = 0
date_normal$bank_shot = 0
date_normal[grepl("Jump Shot", date_normal$combined_shot_type) == TRUE, "jump_shot"] = 1
date_normal[grepl("Dunk", date_normal$combined_shot_type) == TRUE, "dunk"] = 1
date_normal[grepl("Tip Shot", date_normal$combined_shot_type) == TRUE, "tip_shot"] = 1
date_normal[grepl("Hook Shot", date_normal$combined_shot_type) == TRUE, "hook_shot"] = 1
date_normal[grepl("Bank Shot", date_normal$combined_shot_type) == TRUE, "bank_shot"] = 1

# add avg and total shots made to date_normal dataframe
# avoid remerging win
dates_unique1 = subset(dates_unique, select=c(-win))
date_normal = merge(date_normal, dates_unique1, by="game_number")
date_normal$season_norm = 0
count = 1
season = date_normal$season[1]
for (i in 1:length(date_normal$season)){
  season1 = date_normal$season[i]
  if (season1 != season) {
    count = count + 1
    season = season1
  }
  date_normal$season_norm[i] = count 
}
write.csv(dates_unique, "/Users/ChrisChen/Desktop/by_date.csv")
write.csv(clutch_shooting, "/Users/ChrisChen/Desktop/clutch_shots.csv")
write.csv(date_normal, "/Users/ChrisChen/Desktop/cleaned.csv")
