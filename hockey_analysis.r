
View(s1314finalcorr[order(s1314finalcorr$wins2, decreasing = TRUE),])
# View(s1213finalcorr[order(s1213finalcorr$wins2, decreasing = TRUE),])
View(s1112finalcorr[order(s1112finalcorr$wins2, decreasing = TRUE),])
View(s1011finalcorr[order(s1011finalcorr$wins2, decreasing = TRUE),])  
View(s0910finalcorr[order(s0910finalcorr$wins2, decreasing = TRUE),]) 
View(s0809finalcorr[order(s0809finalcorr$wins2, decreasing = TRUE),]) 
View(s0708finalcorr[order(s0708finalcorr$wins2, decreasing = TRUE),])

View(s1314corr)
# View(s1213corr)
View(s1112corr)
View(s1011corr)
View(s0910corr)
View(s0809corr)
View(s0708corr)



s1314finalcorr$features = rownames(s1314finalcorr)
s1112finalcorr$features = rownames(s1112finalcorr)
s1011finalcorr$features = rownames(s1011finalcorr)
s0910finalcorr$features = rownames(s0910finalcorr)
s0809finalcorr$features = rownames(s0809finalcorr)
s0708finalcorr$features = rownames(s0708finalcorr)


# select variables if has over .3 correlation by season 

s1314topfeatures = select(filter(s1314finalcorr, abs(wins2) > .3), features, wins2)
s1112topfeatures = select(filter(s1112finalcorr, abs(wins2) > .3), features, wins2)
s1011topfeatures = select(filter(s1011finalcorr, abs(wins2) > .3), features, wins2)
s0910topfeatures = select(filter(s0910finalcorr, abs(wins2) > .3), features, wins2)
s0809topfeatures = select(filter(s0809finalcorr, abs(wins2) > .3), features, wins2)
s0708topfeatures = select(filter(s0708finalcorr, abs(wins2) > .3), features, wins2)

topfeaturescombined = rbind_all(list(s1314topfeatures, s1112topfeatures, s1011topfeatures, s0910topfeatures, s0809topfeatures, s0708topfeatures))

View(arrange(topfeaturescombined %>%
group_by(features) %>%
summarize(seasons_top = n(), avg_corr = mean(wins2), max_corr = max(wins2), min_corr = min(wins2)), desc(seasons_top)))


featurescombined = rbind_all(list(s1314finalcorr, s1112finalcorr, s1011finalcorr, s0910finalcorr, s0809finalcorr, s0708finalcorr))

View(arrange(featurescombined %>%
group_by(features) %>%
summarize(seasons_top = n(), avg_corr = mean(wins2), max_corr = max(wins2), min_corr = min(wins2)), desc(avg_corr)))

# so we can see what matters the most.  would be cool to look over time to though to see if corsi is out of favor
# .. but corsi close seems to win


# regression time 
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf

seasonsteamscombined = rbind_all(list(s1314corr, s1112corr, s1011corr, s0910corr, s0809corr, s0708corr))

attach(seasonsteamscombined)

lm.fit2=lm(wins2 ~ wins1)
summary(lm.fit2)
names(lm.fit2)
coef(lm.fit2)
confint(lm.fit2)

predicted = as.data.frame(predict(lm.fit2, wins1=wins1, interval = "confidence"))
predict(lm.fit2, data.frame(wins1=(c(.3, .5, .8))), interval = "prediction")

plot(wins1, wins2)
abline(lm.fit2)

plot(wins1, wins2, pch=19)
abline(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)


par(mfrow=c(1,1))
plot(predict(lm.fit2), residuals(lm.fit2))
plot(predict(lm.fit2), rstudent(lm.fit2))

lm.fit=lm(wins2 ~ wins1 + goals1 + goalsfirstfor1 + penaltycallratio1)
summary(lm.fit)

summary(lm.fit)$r.sq # r-squared
summary(lm.fit)$sigma # rse

library("car")

vif(lm.fit) # vif 

anova(lm.fit, lm.fit2)



model1 = lm(wins2 ~ wins1) 
model2 = lm(wins2 ~ wins1 + goals1)
model3 = lm(wins2 ~ corsi1)
model4 = lm(wins2 ~ corsi1 + corsievenclose1 + corsiclose1 + corsitied1 + offevents1 + offeventsclose1 + goals1 + corsieven1 + shots1 + goalsfirst1 + corsi25close1 + goalsclose1 + winsexcstart1 + wins1 + wins31 + winsregot1 + wins500pct1 + penaltycallratio1 + goalsfirstagainst1+ goalsagainst1)
model5 = lm(wins2 ~ corsi1 + corsievenclose1 + corsiclose1 + offevents1 + offeventsclose1 + goals1 + shots1 + goalsfirst1 + corsi25close1 + winsexcstart1 + wins1 + wins31 + winsregot1 + wins500pct1 + penaltycallratio1 + goalsagainst1)
model6 = lm(wins2 ~ corsi1 + corsievenclose1 + offevents1 + goals1 + shots1 + goalsfirst1 + corsi25close1 + winsexcstart1 + wins31 + winsregot1 + wins500pct1 + penaltycallratio1 + goalsagainst1)
model7 = lm(wins2 ~ corsievenclose1 + offevents1 + goals1 + shots1 + goalsfirst1 + corsi25close1 + winsexcstart1 + wins31 + winsregot1 + wins500pct1 + penaltycallratio1 + goalsagainst1)

seasonsteamscombinedtrim = seasonsteamscombined[-grep("2$", names(seasonsteamscombined))]
seasonsteamscombinedtrim$wins2 = seasonsteamscombined$wins2  

model8 = lm(wins2 ~. -team, data = seasonsteamscombinedtrim)

summary(model8)
coef(model8)
confint(model8)
par(mfrow=c(2,2))
plot(model8)
summary(model8)$r.sq # r-squared
summary(model8)$sigma # rse
library("car")
vif(model8) # vif 
anova(model1, model2, model3, model4, model5, model6, model7, model8)

attach(seasonsteamscombined)

predicted8 = as.data.frame(predict(model8, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted7 = as.data.frame(predict(model7, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted6 = as.data.frame(predict(model6, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted5 = as.data.frame(predict(model5, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted4 = as.data.frame(predict(model4, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted3 = as.data.frame(predict(model3, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted2 = as.data.frame(predict(model2, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))
predicted1 = as.data.frame(predict(model1, seasonsteamscombinedtrim=select(seasonsteamscombinedtrim, -team), interval = "prediction"))

seasonsteamscombinedpredict = seasonsteamscombined
seasonsteamscombinedpredict$predicted1 = predicted1$fit
seasonsteamscombinedpredict$predicted2 = predicted2$fit
seasonsteamscombinedpredict$predicted3 = predicted3$fit
seasonsteamscombinedpredict$predicted4 = predicted4$fit
seasonsteamscombinedpredict$predicted5 = predicted5$fit
seasonsteamscombinedpredict$predicted6 = predicted6$fit
seasonsteamscombinedpredict$predicted7 = predicted7$fit
seasonsteamscombinedpredict$predicted8 = predicted8$fit

plot(seasonsteamscombinedpredict$predicted8, seasonsteamscombinedpredict$wins2)

seasonsteamscombinedpredicttrim = select(seasonsteamscombinedpredict, team, wins2, wins1, predicted1, predicted2, predicted3, predicted4, predicted5, predicted6, predicted7, predicted8)

seasonsteamscombinedpredicttrim$diffwins1 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$wins1
seasonsteamscombinedpredicttrim$diffpredicted1 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted1
seasonsteamscombinedpredicttrim$diffpredicted2 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted2
seasonsteamscombinedpredicttrim$diffpredicted3 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted3
seasonsteamscombinedpredicttrim$diffpredicted4 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted4
seasonsteamscombinedpredicttrim$diffpredicted5 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted5
seasonsteamscombinedpredicttrim$diffpredicted6 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted6
seasonsteamscombinedpredicttrim$diffpredicted7 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted7
seasonsteamscombinedpredicttrim$diffpredicted8 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted8

sqrt(mean((seasonsteamscombinedpredicttrim$diffwins1^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted1^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted2^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted3^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted4^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted5^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted6^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted7^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted8^2)))


sqrt(median((seasonsteamscombinedpredicttrim$diffwins1^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted1^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted2^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted3^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted4^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted5^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted6^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted7^2)))
sqrt(median((seasonsteamscombinedpredicttrim$diffpredicted8^2)))


mean(abs(seasonsteamscombinedpredicttrim$diffwins1))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted1))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted2))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted3))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted4))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted5))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted6))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted7))
mean(abs(seasonsteamscombinedpredicttrim$diffpredicted8))


# model8 wins.  so what would it have told us?  how accurate is it?
# from above we know that the avg diff in win pct is about 6-8% .. how many games it that?

seasonsteamscombinedpredicttrim$wins1actual = seasonsteamscombinedpredicttrim$wins1*41 
seasonsteamscombinedpredicttrim$predicted8actual = seasonsteamscombinedpredicttrim$predicted8*41
seasonsteamscombinedpredicttrim$wins2actual = seasonsteamscombinedpredicttrim$wins2*41
seasonsteamscombinedpredicttrim$wins1windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$wins1actual))
seasonsteamscombinedpredicttrim$predicted8windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted8actual))
seasonsteamscombinedpredicttrim$bigdiffwin1win2 = ifelse(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$wins1actual) > 5, 1, 0)

seasonsteamscombinedpredicttrim$predicted8within3 = ifelse(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted8actual) < 4, 1, 0)
seasonsteamscombinedpredicttrim$predicted8below3 = ifelse(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted8actual > 3, 1, 0)
seasonsteamscombinedpredicttrim$predicted8above3 = ifelse(seasonsteamscombinedpredicttrim$predicted8actual - seasonsteamscombinedpredicttrim$wins2actual > 3, 1, 0)
seasonsteamscombinedpredicttrim$rawpredicted8windiff = round(seasonsteamscombinedpredicttrim$predicted8actual - seasonsteamscombinedpredicttrim$wins2actual)


View(arrange(select(seasonsteamscombinedpredicttrim, team, wins1, predicted8, wins2, wins1actual, predicted8actual, wins2actual, wins1windiff, predicted8windiff, bigdiffwin1win2, predicted8within3, predicted8above3, predicted8below3),desc(bigdiffwin1win2), desc(wins1windiff)))

mean(seasonsteamscombinedpredicttrim$wins1windiff)
mean(seasonsteamscombinedpredicttrim$predicted8windiff)
mean(seasonsteamscombinedpredicttrim$rawpredicted8windiff)

seasonsteamscombinedpredicttrim %>%
summarize(correct = sum(predicted8within3), above = sum(predicted8above3), below = sum(predicted8below3))

#beautiful -- just slightly underpredicts, but basically no directional bias 
hist(seasonsteamscombinedpredicttrim$rawpredicted8windiff)


# overfitting: http://blog.minitab.com/blog/adventures-in-statistics/multiple-regession-analysis-use-adjusted-r-squared-and-predicted-r-squared-to-include-the-correct-number-of-variables



# notice that on big changes, it gets direction but not magnitude right.  how to fix that?
# play with variable inclusion, interaction, weighting, functional form 

# check to see how coefficients change when you add new variables to regressions

# include season team combos as unique id

# make dataframe or columns with actuals and predicted from each model and analyze abs diffs, mean, median, max, rank, spot check, etc
# look at regression libraries in r




# add categorical predictors
# consider transformations of variables 
# consider interaction effects 

 # sports analyst venn diagram (like drew's) - stats, hockey knowledge, hacking skills, communication/empathy with gm/coaches)



# pdo
# shooting percentage
# save percentage
# shot quality 
# shooting percentage by shot location/quality 
# odd/even to account for tread deadline and thing like that
# pick teams and see if sample size is issue for specific teams
# show lots of example of teams 
# throw lots of other variable in and interact them to get higher r-squared
# do actual regressions for prediction instead of just correlations 
# look at this on player level to see how things change
# exclude blocked shots and missed shots to just have shots on goal and goals 
# think bigger picture - what is corsi supposed to measure - what is it actually measuring 
# should we use corsi to evaluate players or teams in a holistic way?  could be it be a smaller piece?  
# what questions do we want to answer?  
# predict team demise or success 
# predict player demise or succeess 
# understand what teams need to do to win games -- does corsi cause wins or just kind of predict them?
# how do we evaluate a player -- consider differences between defensive players vs offense and shooters vs playermakers vs hustlers
# expected probablity of scoring/winning and how it changes after each event 
# north/south vs east/west -- dont have that data yet
# can we predict a player's future scoring
# consider opponent competition, zone starts, linemates, game situtions for ice time, total ice time, 
# also general player location, how often they come down your side and success rate, and actual possession data, obviously
# quantify luck vs skill 
# look at effects of travelling, division, periods, time of day, conference, players/reg season, game number, injuries, 
# evaluate player synergies on various team types and systems to make trades or signings 
# think about zone entries, zone exits, face offs, open corsi, shot location, passing, rebounds, powerplay effects, etc, etc





# predictive power: http://objectivenhl.blogspot.com/2022/03/loose-ends-part-i-predictive-validity.html

# good stuff here: http://statsportsconsulting.com/2024/02/26/out-of-the-ice-age-some-hockey-analytics-highlights-since-ssac2022/

# looks like some good shit here: http://hockeyanalysis.com/tag/corsi/

# maybe this: http://www.sloansportsconference.com/wp-content/uploads/2023/Total%20Hockey%20Rating%20(THoR)%20A%20comprehensive%20statistical%20rating%20of%20National%20Hockey%20League%20forwards%20and%20defensemen%20based%20upon%20all%20on-ice%20events.pdf

# maybe this: http://www.boysonthebus.com/2023/22/26/testing-the-predictive-value-of-expected-goals-vs-other-metrics/

# maybe this: http://www.hockeyprospectus.com/exploring-the-theoretical-limits-of-shot-quality/

# maybe http://nhlnumbers.com/2024/7/29/dcorsi-introductions

# maybe http://www.hockeyabstract.com/playerusagecharts

# this looks cool : http://www.arcticicehockey.com/2022/2/2/2674263/a-deeper-look-into-in-game-expected-points

# http://www.acthomas.ca/howto-use-nhlscrapr-to-collect-nhl-rtss-data/





