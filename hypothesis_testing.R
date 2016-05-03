mydata = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/cleaned.csv", header = T)
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
boot.mean = function(x,B) {
  n = length(x)
  boot.samples = matrix( sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  interval = mean(x) + c(-1,1)*1.96*se
  print(interval)
  return(list(boot.statistics = boot.statistics, interval=interval, se=se))
}
win_clutch_perc = c(rep(1,sum(clutch_shots_win$clutch_shots_made)), rep(0,sum(clutch_shots_win$clutch_shots_taken-clutch_shots_win$clutch_shots_made)))
win_clutch_perc_ci = boot.mean(x = win_clutch_perc, B = 1000)$interval
loss_clutch_perc = c(rep(1,sum(clutch_shots_loss$clutch_shots_made)), rep(0,sum(clutch_shots_loss$clutch_shots_taken-clutch_shots_loss$clutch_shots_made)))
loss_clutch_perc_ci = boot.mean(x = loss_clutch_perc, B = 1000)$interval
## since the two confidence intervals do not overlap, there is significant evidence of a difference in Kobe's clutch field goal percentage in games won versus games lost. 
## based on the confidence intervals, Kobe's true clutch field goal percentage 
## so Kobe appears to have shot significantly better in the clutch during wins as opposed to losses.
ad.test(bydate_data$avg)
## to compare Kobe's field goal percentage in clutch situations versus his overall performance.
## in overall dataset, game-to-game shooting percentages are not Normally distributed, so bootstrap needs to be used.
overall_shot_perc = c(rep(1, sum(bydate_data$shots_made)), rep(0, sum(bydate_data$shots_taken-bydate_data$shots_made)))
overall_shot_perc_ci = boot.mean(x = overall_shot_perc, B = 1000)$interval
clutch_shot_perc = c(rep(1,sum(clutch_shots$clutch_shots_made)), rep(0,sum(clutch_shots$clutch_shots_taken-clutch_shots$clutch_shots_made)))
clutch_shot_perc_ci = boot.mean(x = clutch_shot_perc, B = 1000)$interval
## since these confidence intervals do not overlap, at the 95% confidence level there is a significant difference between his field goal percentage in the clutch versus his overall performance.
## based on these confidence intervals, the data suggests that Kobe actually performs worse during clutch situations compared to his overall performance.
## Finger injury occurred on February 5, 2008 season (season 11, overall game number 473, season ends at game 507)
## Compare Kobe's performance before the injury and after injury during the season. Hypothesis: Kobe is notoriously tough so he is not as affected by injuries.
bydate_data_finger = subset(bydate_data, game_number > 472 & game_number < 508)
bydate_data_season11 = subset(bydate_data, game_number > 452 & game_number < 508)
finger_mean_difference_data = replicate(1000, boot_mean_difference(bydate_data_finger$avg, bydate_data_season11$avg))
ad.test(finger_mean_difference_data)
finger_mean_difference_ci = c(quantile(finger_mean_difference_data, c(0.025,0.975))[1], quantile(finger_mean_difference_data, c(0.025,0.975))[2])
## confidence interval contains 0, so at a 0.05 significance level there is not enough evidence to reject the null hypothesis that Kobe had the same field goal percentage before and after his finger injury
## note that although Kobe Bryant played all 82 games, dataset only includes 55 total games from the 2007-2008 season.