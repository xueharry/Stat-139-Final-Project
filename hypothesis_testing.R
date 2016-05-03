mydata = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/cleaned.csv", header = T)
clutch_shots = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/clutch_shots.csv", header = T)
bydate_data = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/by_date.csv", header = T)
## load packages nortest and boot
## to analyze whether clutch shot percentage is higher in wins versus losses
t.test(clutch_shots$clutch_perc~clutch_shots$win)
## shot percentage is higher in wins
## to analyze whether Kobe takes more clutch shots in wins versus losses
t.test(clutch_shots$clutch_shots_taken~clutch_shots$win)
## number of shots taken is higher in losses versus wins
## does Kobe make more clutch shots in wins if he takes less shots?
t.test(clutch_shots$clutch_shots_made~clutch_shots$win)
## no significant difference in clutch shots made between wins and losses
## check for normality of the response variables
ad.test(clutch_shots$clutch_perc)
ad.test(clutch_shots$clutch_shots_made)
ad.test(clutch_shots$clutch_shots_taken)
## none of these variables are normal, bootstrap to account for it
clutch_shots_win = subset(clutch_shots, win > 0)
clutch_shots_loss = subset(clutch_shots, win < 1)
win_clutch_perc = clutch_shots_win$clutch_perc
loss_clutch_perc = clutch_shots_loss$clutch_perc
boot_mean_difference = function(x,y){
  A = sample(x, length(x), replace = T)
  B = sample(y, length(y), replace = T)
  return(mean(A)-mean(B))
}
clutch_win_mean_difference_data = replicate(1000, boot_mean_difference(x = win_clutch_perc, y = loss_clutch_perc))
ad.test(clutch_win_mean_difference_data)
## data looks to be Normally distributed
## because confidence interval is analogous to t test output, calculate that instead
clutch_mean_difference_ci = c(quantile(clutch_win_mean_difference_data, c(0.025,0.975))[1], quantile(clutch_win_mean_difference_data, c(0.025,0.975)[2]))
## since confidence interval does not include 0, at a 0.05 significance level there is evidence of a difference in average shot percentage depending on whether the Lakers won or not.
## so Kobe appears to have shot significantly better in the clutch during wins as opposed to losses.
ad.test(bydate_data$avg)
clutch_regular_mean_difference_data = replicate(1000, boot_mean_difference(x = clutch_shots$clutch_perc, y = bydate_data$avg))
ad.test(clutch_regular_mean_difference_data)
## data looks to be Normally distributed
clutch_regular_mean_difference_ci = c(quantile(clutch_regular_mean_difference_data, c(0.025,0.975))[1], quantile(clutch_regular_mean_difference_data, c(0.025,0.975)[2]))
## since confidence interval does not include 0, at a 0.05 significance level there is significant evidence of a difference in average shot percentage depending on whether Kobe is playing in a clutch situation.
## since the confidence interval is negative, the data suggests that Kobe actually performs worse in clutch situations compared to his overall game performance.
## Finger injury occurred on February 5, 2008 season (season 11, overall game number 473, season ends at game 507)
## Compare Kobe's performance before the injury and after injury during the season. Hypothesis: Kobe is notoriously tough so he is not as affected by injuries.
bydate_data_finger = subset(bydate_data, game_number > 472 & game_number < 508)
bydate_data_season11 = subset(bydate_data, game_number > 452 & game_number < 508)
finger_mean_difference_data = replicate(1000, boot_mean_difference(bydate_data_finger$avg, bydate_data_season11$avg))
ad.test(finger_mean_difference_data)
finger_mean_difference_ci = c(quantile(finger_mean_difference_data, c(0.025,0.975))[1], quantile(finger_mean_difference_data, c(0.025,0.975))[2])
## confidence interval contains 0, so at a 0.05 significance level there is not enough evidence to reject the null hypothesis that Kobe had the same field goal percentage before and after his finger injury
## note that although Kobe Bryant played all 82 games, dataset only includes 55 total games from the 2007-2008 season.