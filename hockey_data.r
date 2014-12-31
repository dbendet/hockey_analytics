hockey_data = read.csv('/Users/dbendet/Documents/Spreadsheets/hockey_data.csv')

hockey_data['diff'] = hockey_data['wscore'] - hockey_data['lscore']

hockey_data = sqldf('select *, substr(date, 1, 4) as year, substr(date, 6, 2) as month from hockey_data')

hockey_data = sqldf('select *, wscore || "-" || lscore as score from hockey_data;')

# most popular scores
# most popular differentials
# most popular totals
# segment by year, month, ot, playoffs, home

# 6 observations about the nhl in 6 graphs

library(plyr)
library(ggplot2)
library(reshape)
library(gridExtra)

# sqldf('select count(*) from hockey_data') 
# 24144


pop_scores = sqldf('select score, count(*) as count from hockey_data group by 1 order by 2 desc')

pop_scores['pct'] = pop_scores['count']/24144*100

top_pop_scores = sqldf('select * from pop_scores order by pct desc limit 20')

g1 <- ggplot(data=top_pop_scores, aes(x=reorder(score, desc(pct)), y=pct)) + 
  geom_bar(stat="identity",fill="steelblue",width=.7) +
  scale_y_continuous("% of Games", limits=c(0, 15),expand = c(0,0)) + 
  scale_x_discrete("Score") +
  ggtitle("Most Popular NHL Scores Over Past Two Decades") + 
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust=-.75),
    axis.line = element_line(colour = "#666666", size = .2), 
    plot.title=element_text(size=15, vjust=2), 
    panel.border = element_rect(colour = "white"), 
    legend.position=c(.9,.9), 
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"))

ggsave("/Users/dbendet/Documents/top_scores.pdf", width = 4, height = 3)




home_away_goals = sqldf('select year, avg(hscore) as home_score, avg(vscore) as away_score from hockey_data group by 1 order by 1')
home_away_goals = sqldf('select * from home_away_goals where year != "Date" and year > 1993')
home_away_goals_melt = melt(home_away_goals, id.vars=c("year"))

# home_away_goals_melt$year <- as.Date(as.character(home_away_goals_melt$date))
home_away_goals_melt$variable <- as.factor(home_away_goals_melt$variable)
home_away_goals_melt$value <- as.numeric(home_away_goals_melt$value)

g2 <- ggplot(home_away_goals_melt, aes(x = year, y = value, group = variable, colour = variable)) + 
  scale_colour_brewer(palette="Set1") + 
  scale_y_continuous("Goals Per Game", limits=c(0, 4),expand = c(0,0)) + 
  geom_line(aes(color = variable), size = 1, alpha = 1) + 
  scale_x_discrete("Year") +
  ggtitle("Home/Away Goals Per Game Over Time") + 
  # geom_smooth(size = 1, se = FALSE) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust=-.75),
    axis.line = element_line(colour = "#666666", size = .2), 
    plot.title=element_text(size=15, vjust=2), 
    panel.border = element_rect(colour = "white"), 
    legend.position=c(.9,.9), 
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"))

ggsave("/Users/dbendet/Documents/scores_over_time.pdf", width = 4, height = 3)


diffs = sqldf('select year, sum(wscore - lscore) as a, count(*) as b from hockey_data group by 1 order by 1')
diffs = sqldf('select * from diffs where year != "Date" and year > 1993')
diffs['c'] = diffs['a']/diffs['b'] 
diffs = sqldf('select year, c from diffs')
# home_away_goals_melt = melt(home_away_goals, id.vars=c("year"))

# home_away_goals_melt$year <- as.Date(as.character(home_away_goals_melt$date))
# home_away_goals_melt$variable <- as.factor(home_away_goals_melt$variable)
diffs$c <- as.numeric(diffs$c)
qplot(year, c, data=diffs, geom="line")

g3 <- ggplot(diffs, aes(x = year, y = c)) + 
  scale_colour_brewer(palette="Set1") + 
  geom_line(aes(color = 'black'), size = 1, alpha = 1) + 
  scale_y_continuous("Goals Per Game", limits=c(0, 4),expand = c(0,0)) + 
  scale_x_discrete("Year") +
  ggtitle("Home/Away Goals Per Game Over Time") + 
  # geom_smooth(size = 1, se = FALSE) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust=-.75),
    axis.line = element_line(colour = "#666666", size = .2), 
    plot.title=element_text(size=15, vjust=2), 
    panel.border = element_rect(colour = "white"), 
    legend.position=c(.9,.9), 
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"))

ggsave("/Users/dbendet/Documents/scores_over_time.pdf", width = 4, height = 3)









g1 <- ggplot(data=top_pop_scores, aes(x=reorder(score, desc(pct)), y=pct)) + 
  geom_bar(stat="identity",fill="steelblue",width=.7) +
  scale_y_continuous("% of Games", limits=c(0, 15),expand = c(0,0)) + 
  scale_x_discrete("Score") +
  ggtitle("Most Popular NHL Scores Over Past Two Decades") + 
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust=-.75),
    axis.line = element_line(colour = "#666666", size = .2), 
    plot.title=element_text(size=15, vjust=2), 
    panel.border = element_rect(colour = "white"), 
    legend.position=c(.9,.9), 
    legend.title=element_blank(),
    legend.key = element_rect(colour = "white"))

ggsave("/Users/dbendet/Documents/top_scores.pdf", width = 4, height = 3)



