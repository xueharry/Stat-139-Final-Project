mydata = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/cleaned.csv", header = T)
clutch_shots = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/clutch_shots.csv", header = T)
bydate_data = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/by_date.csv", header = T)
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

