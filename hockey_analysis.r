
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

library("car")

model1 = lm(wins2 ~ wins1) 
model2 = lm(wins2 ~ corsi1)

seasonsteamscombinedtrim = seasonsteamscombined[-grep("2$", names(seasonsteamscombined))]
seasonsteamscombinedtrim$wins2 = seasonsteamscombined$wins2  
seasonsteamscombinedtrim$pctgameshome2 = seasonsteamscombined$pctgameshome2  
seasonsteamscombinedtrim$games5002 = seasonsteamscombined$games5002  


model3 = lm(wins2 ~. -team ,data = seasonsteamscombinedtrim)

summary(model3)
coef(model3)
confint(model3)
par(mfrow=c(2,2))
plot(model3)
plot(predict(model3), residuals(model3))
plot(predict(model3), rstudent(model3))
summary(model3)$sigma # rse
summary(model3)$r.sq # r-squared
summary(model3)$adj.r.squared# adj r-squared
summary(model3)$aliased # show which variables are aliased (basically an r term for perfect collinearity)
vif(model3) # vif 
anova(model1, model2, model3)



summary(model3)$r.sq 
summary(model2)$r.sq 
summary(model1)$r.sq 

summary(model3)$adj.r.squared
summary(model2)$adj.r.squared
summary(model1)$adj.r.squared


library(MASS)
step <- stepAIC(model3, direction="both")
step$anova # display results

# winner from above:

model4 = lm(wins2 ~ corsi1 + corsiclose1 + corsi25close1 + wins1 + goalsfor1 + 
    goalsfirst1 + goalssecondfor1 + goalsthirdagainst1 + pctgoalstipfor1 + 
    pctgoalsslapfor1 + pctgoalswristfor1 + pctgoals25against1 + 
    pctshottipagainst1 + pctshotslapfor1 + pctshotwristfor1 + 
    pctshotwristagainst1 + shots1 + pctgoalforpp1 + wins11 + 
    pctgameshome2)


anova(model1, model2, model3, model4)


summary(model4)$r.sq 
summary(model3)$r.sq 
summary(model2)$r.sq 
summary(model1)$r.sq 

summary(model4)$adj.r.squared
summary(model3)$adj.r.squared
summary(model2)$adj.r.squared
summary(model1)$adj.r.squared


# library(leaps)
# model3 = lm(wins2 ~. -team ,data = seasonsteamscombinedtrim)
# leaps<-regsubsets(wins2 ~., data=seasonsteamscombinedtrim[, names(seasonsteamscombinedtrim) != "team"], nbest=1, method="forward", really.big=FALSE)
# summary(leaps)
# plot(leaps,scale="r2")
# library(car)
# subsets(leaps, statistic="rsq")


# library(relaimpo)
# calc.relimp(model4,type=c("lmg","last","first","pratt"), rela=TRUE)
# boot <- boot.relimp(model4, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
# booteval.relimp(boot) # print result
# plot(booteval.relimp(boot,sort=TRUE)) # plot result


# what i think makes most sense given everything

model5 = lm(wins2 ~ 
  wins1 + wins11 + wins500pct1 + 
  corsi1 + corsiclose1 + corsi25close1 + offevents1 + offeventsclose1 + 
  goalsfor1 + goalsagainst1 + 
  goalsfirst1 + goalssecondfor1 + goalsthirdfor1 + 
  pctgoals25for1 + pctgoals25against1 + 
  shots1 + pctshot25for1 +  pctshot25against1 + 
  pctgoalforpp1 + pctgoalagainstpp1 + 
  penaltycallratio1 + 
  pdo1 + 
  pctgameshome2 + games5002)

summary(model5)

summary(model5)$r.sq 
summary(model4)$r.sq 
summary(model3)$r.sq 
summary(model2)$r.sq 
summary(model1)$r.sq 

summary(model5)$adj.r.squared
summary(model4)$adj.r.squared
summary(model3)$adj.r.squared
summary(model2)$adj.r.squared
summary(model1)$adj.r.squared

summary(model5)$sigma
summary(model4)$sigma
summary(model3)$sigma
summary(model2)$sigma
summary(model1)$sigma

anova(model1, model2, model3, model4, model5)


library(MASS)
step <- stepAIC(model5, direction="both")
step$anova # display results


model6 = lm(wins2 ~ wins1 + corsi1 + corsiclose1 + corsi25close1 + 
                     goalsfirst1 + pctgoals25against1 + shots1 + 
                     pctgoalforpp1 + pctgameshome2)



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

seasonsteamscombinedpredicttrim = dplyr::select(seasonsteamscombinedpredict, team, wins2, wins1, predicted1, predicted2, predicted3, predicted4, predicted5, predicted6)

seasonsteamscombinedpredicttrim$diffwins1 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$wins1
seasonsteamscombinedpredicttrim$diffpredicted1 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted1
seasonsteamscombinedpredicttrim$diffpredicted2 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted2
seasonsteamscombinedpredicttrim$diffpredicted3 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted3
seasonsteamscombinedpredicttrim$diffpredicted4 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted4
seasonsteamscombinedpredicttrim$diffpredicted5 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted5
seasonsteamscombinedpredicttrim$diffpredicted6 = seasonsteamscombinedpredicttrim$wins2 - seasonsteamscombinedpredicttrim$predicted6

# rmse

sqrt(mean((seasonsteamscombinedpredicttrim$diffwins1^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted1^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted2^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted3^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted4^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted5^2)))
sqrt(mean((seasonsteamscombinedpredicttrim$diffpredicted6^2)))


# from above we know that the avg diff in win pct is about 6-8% .. how many games it that?

seasonsteamscombinedpredicttrim$predicted1actual = round(seasonsteamscombinedpredicttrim$predicted1*41)
seasonsteamscombinedpredicttrim$predicted2actual = round(seasonsteamscombinedpredicttrim$predicted2*41)
seasonsteamscombinedpredicttrim$predicted3actual = round(seasonsteamscombinedpredicttrim$predicted3*41)
seasonsteamscombinedpredicttrim$predicted4actual = round(seasonsteamscombinedpredicttrim$predicted4*41)
seasonsteamscombinedpredicttrim$predicted5actual = round(seasonsteamscombinedpredicttrim$predicted5*41)
seasonsteamscombinedpredicttrim$predicted6actual = round(seasonsteamscombinedpredicttrim$predicted6*41)
seasonsteamscombinedpredicttrim$wins1actual = seasonsteamscombinedpredicttrim$wins1*41
seasonsteamscombinedpredicttrim$wins2actual = seasonsteamscombinedpredicttrim$wins2*41
seasonsteamscombinedpredicttrim$predicted1windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted1actual))
seasonsteamscombinedpredicttrim$predicted2windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted2actual))
seasonsteamscombinedpredicttrim$predicted3windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted3actual))
seasonsteamscombinedpredicttrim$predicted4windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted4actual))
seasonsteamscombinedpredicttrim$predicted5windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted5actual))
seasonsteamscombinedpredicttrim$predicted6windiff = round(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted6actual))
seasonsteamscombinedpredicttrim$bigdiffwin1win2 = ifelse(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$wins1actual) > 5, 1, 0)

seasonsteamscombinedpredicttrim$predicted3within3 = ifelse(abs(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted3actual) < 4, 1, 0)
seasonsteamscombinedpredicttrim$predicted3below3 = ifelse(seasonsteamscombinedpredicttrim$wins2actual - seasonsteamscombinedpredicttrim$predicted3actual > 3, 1, 0)
seasonsteamscombinedpredicttrim$predicted3above3 = ifelse(seasonsteamscombinedpredicttrim$predicted3actual - seasonsteamscombinedpredicttrim$wins2actual > 3, 1, 0)

seasonsteamscombinedpredicttrim$rawpredicted1windiff = round(seasonsteamscombinedpredicttrim$predicted1actual - seasonsteamscombinedpredicttrim$wins2actual)
seasonsteamscombinedpredicttrim$rawpredicted2windiff = round(seasonsteamscombinedpredicttrim$predicted2actual - seasonsteamscombinedpredicttrim$wins2actual)
seasonsteamscombinedpredicttrim$rawpredicted3windiff = round(seasonsteamscombinedpredicttrim$predicted3actual - seasonsteamscombinedpredicttrim$wins2actual)
seasonsteamscombinedpredicttrim$rawpredicted3windiff = round(seasonsteamscombinedpredicttrim$predicted4actual - seasonsteamscombinedpredicttrim$wins2actual)
seasonsteamscombinedpredicttrim$rawpredicted5windiff = round(seasonsteamscombinedpredicttrim$predicted5actual - seasonsteamscombinedpredicttrim$wins2actual)
seasonsteamscombinedpredicttrim$rawpredicted6windiff = round(seasonsteamscombinedpredicttrim$predicted6actual - seasonsteamscombinedpredicttrim$wins2actual)

View(arrange(dplyr::select(seasonsteamscombinedpredicttrim, team, wins1, wins2, predicted1, predicted2, predicted3, predicted4, predicted5, predicted6, wins1actual, wins2actual, predicted1actual, predicted2actual, predicted3actual, predicted4actual, predicted5actual, predicted6actual, predicted1windiff, predicted2windiff, predicted3windiff, predicted4windiff, predicted5windiff, predicted6windiff, bigdiffwin1win2), desc(bigdiffwin1win2), desc(abs(wins1actual - wins2actual))))

seasonsteamscombinedpredicttrim %>%
summarize(correct = sum(predicted3within3), above = sum(predicted3above3), below = sum(predicted3below3))

hist(seasonsteamscombinedpredicttrim$rawpredicted1windiff)
hist(seasonsteamscombinedpredicttrim$rawpredicted2windiff)
hist(seasonsteamscombinedpredicttrim$rawpredicted3windiff)
hist(seasonsteamscombinedpredicttrim$rawpredicted4windiff)
hist(seasonsteamscombinedpredicttrim$rawpredicted5windiff)
hist(seasonsteamscombinedpredicttrim$rawpredicted6windiff)

mean(seasonsteamscombinedpredicttrim$predicted1windiff)
mean(seasonsteamscombinedpredicttrim$predicted2windiff)
mean(seasonsteamscombinedpredicttrim$predicted3windiff)
mean(seasonsteamscombinedpredicttrim$predicted4windiff)
mean(seasonsteamscombinedpredicttrim$predicted5windiff)
mean(seasonsteamscombinedpredicttrim$predicted6windiff)

mean(seasonsteamscombinedpredicttrim$rawpredicted1windiff)
mean(seasonsteamscombinedpredicttrim$rawpredicted2windiff)
mean(seasonsteamscombinedpredicttrim$rawpredicted3windiff)
mean(seasonsteamscombinedpredicttrim$rawpredicted4windiff)
mean(seasonsteamscombinedpredicttrim$rawpredicted5windiff)
mean(seasonsteamscombinedpredicttrim$rawpredicted6windiff)



# so models 3 and then 4 predicted most accurately with this dataset

# we know that from this though: 
anova(model1, model2, model3, model4, model5, model6)

# they also both have the best r squared
summary(model6)$r.sq 
summary(model5)$r.sq 
summary(model4)$r.sq 
summary(model3)$r.sq 
summary(model2)$r.sq 
summary(model1)$r.sq 

# but 4, 5, and 6 have much better adjusted r squareds, meaning model 3 could be overfitted..
summary(model6)$adj.r.squared
summary(model5)$adj.r.squared
summary(model4)$adj.r.squared
summary(model3)$adj.r.squared
summary(model2)$adj.r.squared
summary(model1)$adj.r.squared

# model 4 also has the lowest rse, rollowed by model 6
summary(model6)$sigma
summary(model5)$sigma
summary(model4)$sigma
summary(model3)$sigma
summary(model2)$sigma
summary(model1)$sigma


# based on this i would be inclined to use model 4 or 3.  possibly model 6 bc there are less variables.
# but next steps are to consider transformations and interactions of variables, without overfitting..
# and then test all these models on individual seasons to see how they all perform season by season






# check to see how coefficients change when you add new variables to regressions

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




