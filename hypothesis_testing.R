mydata = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/cleaned.csv", header = T)
# hypothesis_testing.R
# Hypothesis tests

clutch_shots = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/clutch_shots.csv", header = T)
bydate_data = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/by_date.csv", header = T)
## load packages nortest and boot
## to analyze whether clutch shot percentage is higher in wins versus losses
t.test(clutch_shots$clutch_perc~clutch_shots$win)
## shot percentage is higher in wins
## check for normality of the response variables
ad.test(clutch_shots$clutch_perc)
## not Normally distributed, bootstrap to account for it
clutch_shots_win = subset(clutch_shots, win > 0)
clutch_shots_loss = subset(clutch_shots, win < 1)
## create a function to calculate the confidence intervals using vectors of his shot results
boot.mean = function(x,B) {
  n = length(x)
  boot.samples = matrix( sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  interval = mean(x) + c(-1,1)*1.96*se
  print(interval)
  return(list(boot.statistics = boot.statistics, interval=interval, se=se))
}
win_clutch_perc = c(rep(1,sum(clutch_shots_win$clutch_shots_made)), rep(0,sum(clutch_shots_win$clutch_shots_taken)-sum(clutch_shots_win$clutch_shots_made)))
win_clutch_perc_ci = boot.mean(x = win_clutch_perc, B = 1000)$interval
loss_clutch_perc = c(rep(1,sum(clutch_shots_loss$clutch_shots_made)), rep(0,sum(clutch_shots_loss$clutch_shots_taken)-sum(clutch_shots_loss$clutch_shots_made)))
loss_clutch_perc_ci = boot.mean(x = loss_clutch_perc, B = 1000)$interval
## since the two confidence intervals do not overlap, at the 95% confidence level there is significant evidence of a difference in Kobe's clutch field goal percentage in games won versus games lost. 
## so Kobe appears to have shot significantly better in the clutch during wins as opposed to losses.
## Finger injury occurred on February 5, 2008 season (season 11, overall game number 473, season ends at game 507)
## Compare Kobe's performance before the injury and after injury during the season. Hypothesis: Kobe is notoriously tough so he is not as affected by injuries.
bydate_data_finger = subset(bydate_data, game_number > 472 & game_number < 508)
bydate_data_nofinger = subset(bydate_data, game_number > 452 & game_number < 473)
finger_shot_perc = c(rep(1, sum(bydate_data_finger$shots_made)), rep(0, sum(bydate_data_finger$shots_taken)-sum(bydate_data_finger$shots_made)))
finger_shot_perc_ci = boot.mean(x = finger_shot_perc, B = 1000)$interval
nofinger_shot_perc = c(rep(1, sum(bydate_data_nofinger$shots_made)), rep(0, sum(bydate_data_nofinger$shots_taken)-sum(bydate_data_nofinger$shots_made)))
nofinger_shot_perc_ci = boot.mean(x = nofinger_shot_perc, B = 1000)$interval
## since the confidence intervals overlap, the data does not provide enough evidence to show that Kobe performed significantly differently before and after he suffered the finger injury.
## note that although Kobe Bryant played all 82 games, dataset only includes 55 total games from the 2007-2008 season.
## also note that the confidence interval of Kobe's performance when he played through the injury is wider, likely due to the smaller sample size of only 35 games provided in the dataset.
