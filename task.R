library('ggplot2')
source('plotThemes.R')
events = c(5, 10, 50)
nEvents = length(events)
probs = c(0.1, 0.1, 0.8);
#probs = c(0.3, 0.3, 0.4);
xTicks = seq(0, 50, by = 0.1)

pdf = rep(0, length = length(xTicks))
for(i in 1 : nEvents){
  pdf[which(xTicks == events[i])] = probs[i]
}

plot(pdf)
cdf = cumsum(pdf)
plot(cdf)

# 
waitExp = cumsum(pdf * xTicks) / cdf
rewardRates = 30 * cdf / (cdf * waitExp + (1 - cdf) * xTicks + 2)
plotData = data.frame(xTicks = xTicks, rewardRates = rewardRates) 

# plot
ggplot(plotData, aes(xTicks, rewardRates)) + geom_point() +
  ylab('Reward rate (cent / s)') + xlab('Time(s)') + saveTheme

xTicks[which.max(rewardRates)]
#####################
meanWait1 = sum(events * probs)
meanWait2 = sum(events[2:3] * probs[2:3]) / sum(probs[2:3])
meanWait3 = events[3] 




