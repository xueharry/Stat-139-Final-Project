# models.R
# logit and regression models

cleaned = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/cleaned.csv", header = T)
clutch_shots = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/clutch_shots.csv", header = T)
by_date = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/by_date.csv", header = T)
# subset data
myvars=c("game_number","loc_x","loc_y","minutes_remaining","period","playoffs","seconds_remaining","shot_distance","shot_made_flag","win","home","three_pointer","jump_shot","dunk","tip_shot","hook_shot","bank_shot")
data=cleaned[myvars]

# correlation matrix and corrplot
m=cor(data)
corrplot(m,method="circle")

# identified shot_distance and loc_y as having a correlation above .8
cor(data$shot_distance,data$loc_y)

# checked for autocorrelation by looking at the plot
plot(cleaned$game_number,cleaned$shot_made_flag,xlab="Game Number",ylab="Shot Made")

# linear regression 
fit=lm(cleaned$shot_made_flag~cleaned$game_number+cleaned$loc_x+cleaned$minutes_remaining+cleaned$period+cleaned$playoffs+cleaned$seconds_remaining+cleaned$shot_distance+cleaned$win+cleaned$home+cleaned$three_pointer+cleaned$jump_shot+cleaned$dunk+cleaned$tip_shot+cleaned$hook_shot+cleaned$bank_shot)

# backward stepwise 
stepwise(fit,direction="backward",criterion="AIC")
stepwise(fit,direction="backward",criterion="BIC")
# model after removing least significant
fit=lm(cleaned$shot_made_flag~cleaned$period+cleaned$seconds_remaining+cleaned$shot_distance+cleaned$win+cleaned$three_pointer+cleaned$jump_shot+cleaned$dunk+cleaned$tip_shot+cleaned$bank_shot)

# checked diagnostics but you fail most of them becuase our Y variable is binary and we are trying to fit a linear model to it

# logit
fit=glm(cleaned$shot_made_flag~cleaned$game_number+cleaned$loc_x+cleaned$minutes_remaining+cleaned$period+cleaned$playoffs+cleaned$seconds_remaining+cleaned$shot_distance+cleaned$win+cleaned$home+cleaned$three_pointer+cleaned$jump_shot+cleaned$dunk+cleaned$tip_shot+cleaned$hook_shot+cleaned$bank_shot,family=binomial(logit))
stepwise(fit,direction="backward",criterion="AIC")
stepwise(fit,direction="backward",criterion="BIC")
# removing least significant
fit=glm(cleaned$shot_made_flag~cleaned$period+cleaned$seconds_remaining+cleaned$shot_distance+cleaned$win+cleaned$three_pointer+cleaned$jump_shot+cleaned$dunk+cleaned$tip_shot+cleaned$bank_shot,family=binomial(logit))


# tested for stationarity
adf.test(cleaned$avg, alternative = "stationary")
adf.test(by_date$win, alternative = "stationary")

# logit regression for win-loss
# Add column for percentage in non-clutch situations
# to create two independent categories for clutch and non-clutch
fit=glm(win~clutch_perc +non_clutch_perc,data=by_date, na.action=na.omit)
summary(fit)
