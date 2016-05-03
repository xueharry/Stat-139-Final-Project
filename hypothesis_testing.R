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