cleaned = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/cleaned.csv", header = T)
clutch_shots = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/clutch_shots.csv", header = T)
by_date = read.csv("https://raw.githubusercontent.com/xueharry/Stat-139-Final-Project/master/by_date.csv", header = T)

# Graph Kobe's average shooting percentage over the course of the periods
period.1.avg = sum(subset(cleaned, period == 1 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 1)$shot_made_flag)
period.2.avg = sum(subset(cleaned, period == 2 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 2)$shot_made_flag)
period.3.avg = sum(subset(cleaned, period == 3 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 3)$shot_made_flag)
period.4.avg = sum(subset(cleaned, period == 4 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 4)$shot_made_flag)
period.5.avg = sum(subset(cleaned, period == 5 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 5)$shot_made_flag)
period.6.avg = sum(subset(cleaned, period == 6 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 6)$shot_made_flag)
period.7.avg = sum(subset(cleaned, period == 7 & shot_made_flag == 1)$shot_made_flag)/ length(subset(cleaned, period == 7)$shot_made_flag)

barplot(c(period.1.avg, period.2.avg, period.3.avg, period.4.avg, period.5.avg, period.6.avg, period.7.avg), main="Kobe's Shooting Percentage Over Period", col="darkorchid", names.arg=c(1,2,3,4,5,6,7), xlab="Period", ylab="Average Shooting Percentage")

# Breakdown of Kobe's shooting by shot type

