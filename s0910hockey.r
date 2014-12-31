
library(dplyr)

load("/Users/dbendet/Documents/Code/r/hockey_analytics/source-data/nhlscrapr-20092010.RData")

View(grand.data)

s0910 = grand.data

dim(s0910)

head(s0910)

distinct(select(s0910, etype))

s0910 = filter(s0910, gcode < 30000)

# corsi 

s0910 = s0910 %>%
mutate(corsihome = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(corsiaway = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsihome = cumsum(corsihome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsiaway = cumsum(corsiaway))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(reverse_rank = row_number(desc(event)))


s0910 = s0910 %>% 
group_by(gcode, period) %>% 
mutate(reverse_per_rank = row_number(desc(event)))


# corsi close

s0910 = s0910 %>%
mutate(corsiclosehome = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == hometeam & abs(home.score-away.score) < 2, 1, 0))

s0910 = s0910 %>%
mutate(corsicloseaway = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == awayteam & abs(home.score-away.score) < 2, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsiclosehome = cumsum(corsiclosehome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsicloseaway = cumsum(corsicloseaway))

# corsi tied

s0910 = s0910 %>%
mutate(corsitiedhome = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == hometeam & home.score == away.score, 1, 0))

s0910 = s0910 %>%
mutate(corsitiedaway = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == awayteam & home.score == away.score, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsitiedhome = cumsum(corsitiedhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsitiedaway = cumsum(corsitiedaway))

# corsi even

s0910 = s0910 %>%
mutate(corsievenhome = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == hometeam & home.skaters==away.skaters, 1, 0))

s0910 = s0910 %>%
mutate(corsievenaway = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == awayteam & home.skaters==away.skaters, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsievenhome = cumsum(corsievenhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsievenaway = cumsum(corsievenaway))

# corsi even close 

s0910 = s0910 %>%
mutate(corsicloseevenhome = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == hometeam & abs(home.score-away.score) < 2 & home.skaters==away.skaters, 1, 0))

s0910 = s0910 %>%
mutate(corsicloseevenaway = ifelse((etype == 'SHOT' | etype == 'BLOCK' | etype == 'MISS' | etype == 'GOAL') & ev.team == awayteam & abs(home.score-away.score) < 2 & home.skaters==away.skaters, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsicloseevenhome = cumsum(corsicloseevenhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsicloseevenaway = cumsum(corsicloseevenaway))

# powerplay count

s0910 = s0910 %>%
mutate(powerplayhome = ifelse(etype == 'PENL' & ev.team == awayteam, 1, 0))

s0910 = s0910 %>%
mutate(powerplayaway = ifelse(etype == 'PENL' & ev.team == hometeam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamepowerplayhome = cumsum(powerplayhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamepowerplayaway = cumsum(powerplayaway))

# shots 

s0910 = s0910 %>%
mutate(shothome = ifelse((etype == 'SHOT' | etype == 'GOAL') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(shotaway = ifelse((etype == 'SHOT' | etype == 'GOAL') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshothome = cumsum(shothome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshotaway = cumsum(shotaway))

# shots under 25

s0910 = s0910 %>%
mutate(shot25home = ifelse((etype == 'SHOT' | etype == 'GOAL') & distance < 26 & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(shot25away = ifelse((etype == 'SHOT' | etype == 'GOAL') & distance < 26 & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshot25home = cumsum(shot25home))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshot25away = cumsum(shot25away))

# goals 

s0910 = s0910 %>%
mutate(goalhome = ifelse((etype == 'GOAL') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(goalaway = ifelse((etype == 'GOAL') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalhome = cumsum(goalhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalaway = cumsum(goalaway))

# goals under 25

s0910 = s0910 %>%
mutate(goal25home = ifelse((etype == 'GOAL') & distance < 26 & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(goal25away = ifelse((etype == 'GOAL') & distance < 26 & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoal25home = cumsum(goal25home))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoal25away = cumsum(goal25away))


# wrist shots 

s0910 = s0910 %>%
mutate(shotwristhome = ifelse((etype == 'SHOT' | etype == 'GOAL') & (type == 'Wrist' | type == 'Backhand' | type == 'Snap') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(shotwristaway = ifelse((etype == 'SHOT' | etype == 'GOAL') & (type == 'Wrist' | type == 'Backhand' | type == 'Snap') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshotwristhome = cumsum(shotwristhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshotwristaway = cumsum(shotwristaway))


# slap shots 

s0910 = s0910 %>%
mutate(shotslaphome = ifelse((etype == 'SHOT' | etype == 'GOAL') & type == 'Slap' & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(shotslapaway = ifelse((etype == 'SHOT' | etype == 'GOAL') & type == 'Slap' & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshotslaphome = cumsum(shotslaphome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshotslapaway = cumsum(shotslapaway))


# tip shots 

s0910 = s0910 %>%
mutate(shottiphome = ifelse((etype == 'SHOT' | etype == 'GOAL') & (type == 'Tip-In' | type == 'Deflected') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(shottipaway = ifelse((etype == 'SHOT' | etype == 'GOAL') & (type == 'Tip-In' | type == 'Deflected') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshottiphome = cumsum(shottiphome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesshottipaway = cumsum(shottipaway))


# wrist goals 

s0910 = s0910 %>%
mutate(goalwristhome = ifelse((etype == 'GOAL') & (type == 'Wrist' | type == 'Backhand' | type == 'Snap') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(goalwristaway = ifelse((etype == 'GOAL') & (type == 'Wrist' | type == 'Backhand' | type == 'Snap') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalwristhome = cumsum(goalwristhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalwristaway = cumsum(goalwristaway))


# slap goals 

s0910 = s0910 %>%
mutate(goalslaphome = ifelse((etype == 'GOAL') & type == 'Slap' & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(goalslapaway = ifelse((etype == 'GOAL') & type == 'Slap' & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalslaphome = cumsum(goalslaphome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalslapaway = cumsum(goalslapaway))


# tip goals 

s0910 = s0910 %>%
mutate(goaltiphome = ifelse((etype == 'GOAL') & (type == 'Tip-In' | type == 'Deflected') & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(goaltipaway = ifelse((etype == 'GOAL') & (type == 'Tip-In' | type == 'Deflected') & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoaltiphome = cumsum(goaltiphome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoaltipaway = cumsum(goaltipaway))


# powerplay goals 

s0910 = s0910 %>%
mutate(goalpphome = ifelse((etype == 'GOAL') & home.skaters > away.skaters & ev.team == hometeam, 1, 0))

s0910 = s0910 %>%
mutate(goalppaway = ifelse((etype == 'GOAL') & away.skaters > home.skaters & ev.team == awayteam, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalpphome = cumsum(goalpphome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalppaway = cumsum(goalppaway))


# goals close

s0910 = s0910 %>%
mutate(goalsclosehome = ifelse((etype == 'GOAL') & ev.team == hometeam & abs(home.score-away.score) < 3, 1, 0))

s0910 = s0910 %>%
mutate(goalscloseaway = ifelse((etype == 'GOAL') & ev.team == awayteam & abs(home.score-away.score) < 3, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsclosehome = cumsum(goalsclosehome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalscloseaway = cumsum(goalscloseaway))


# first period goals

s0910 = s0910 %>%
mutate(goalsfirsthome = ifelse((etype == 'GOAL') & ev.team == hometeam & period == 1, 1, 0))

s0910 = s0910 %>%
mutate(goalsfirstaway = ifelse((etype == 'GOAL') & ev.team == awayteam & period == 1, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsfirsthome = cumsum(goalsfirsthome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsfirstaway = cumsum(goalsfirstaway))


# second period goals

s0910 = s0910 %>%
mutate(goalssecondhome = ifelse((etype == 'GOAL') & ev.team == hometeam & period == 2, 1, 0))

s0910 = s0910 %>%
mutate(goalssecondaway = ifelse((etype == 'GOAL') & ev.team == awayteam & period == 2, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalssecondhome = cumsum(goalssecondhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalssecondaway = cumsum(goalssecondaway))


# third period goals

s0910 = s0910 %>%
mutate(goalsthirdhome = ifelse((etype == 'GOAL') & ev.team == hometeam & period == 3, 1, 0))

s0910 = s0910 %>%
mutate(goalsthirdaway = ifelse((etype == 'GOAL') & ev.team == awayteam & period == 3, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsthirdhome = cumsum(goalsthirdhome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsthirdaway = cumsum(goalsthirdaway))


# ot/so period goals

s0910 = s0910 %>%
mutate(goalsotsohome = ifelse((etype == 'GOAL') & ev.team == hometeam & (period == 4 | period == 5), 1, 0))

s0910 = s0910 %>%
mutate(goalsotsoaway = ifelse((etype == 'GOAL') & ev.team == awayteam & (period == 4 | period == 5), 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsotsohome = cumsum(goalsotsohome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamesgoalsotsoaway = cumsum(goalsotsoaway))


# events in zone 

s0910 = s0910 %>%
mutate(offeventshome = ifelse(homezone == 'Off' & etype != 'PEND', 1, 0))

s0910 = s0910 %>%
mutate(offeventsaway = ifelse(homezone == 'Def' & etype != 'PEND', 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gameoffeventshome = cumsum(offeventshome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gameoffeventsaway = cumsum(offeventsaway))


# events in zone close 

s0910 = s0910 %>%
mutate(offeventsclosehome = ifelse(homezone == 'Off' & etype != 'PEND' & abs(home.score-away.score) < 2, 1, 0))

s0910 = s0910 %>%
mutate(offeventscloseaway = ifelse(homezone == 'Def' & etype != 'PEND' & abs(home.score-away.score) < 2, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gameoffeventsclosehome = cumsum(offeventsclosehome))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gameoffeventscloseaway = cumsum(offeventscloseaway))


# corsi close within 25

s0910 = s0910 %>%
mutate(corsiclose25home = ifelse((etype == 'SHOT' | etype == 'MISS' | etype == 'GOAL') & distance < 26 & ev.team == hometeam & abs(home.score-away.score) < 2, 1, 0))

s0910 = s0910 %>%
mutate(corsiclose25away = ifelse((etype == 'SHOT' | etype == 'MISS' | etype == 'GOAL') & distance < 26 & ev.team == awayteam & abs(home.score-away.score) < 2, 1, 0))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsiclose25home = cumsum(corsiclose25home))

s0910 = s0910 %>% 
group_by(gcode) %>% 
mutate(gamescorsiclose25away = cumsum(corsiclose25away))



# shot distance histogram
View(arrange(s0910 %>% group_by(distance) %>% summarize(count = n()), desc(count)))

# shot success rate by section
View(arrange(filter(s0910, etype == 'SHOT' | etype == 'GOAL') %>% group_by(new.loc.section) %>% summarize(distance = mean(distance, na.rm = TRUE), shotrate = sum(ifelse(etype == 'GOAL', 1, 0))/sum(ifelse(etype == 'GOAL' | etype == 'SHOT', 1, 0)), counts = n()), desc(shotrate)))



# adding all above into results table 

s0910results = select(filter(s0910, 
  reverse_rank == 1), 
  season, 
  gcode, 
  period, 
  hometeam, 
  awayteam, 
  home.score, 
  away.score, 
  gamescorsihome, 
  gamescorsiaway, 
  gamescorsievenhome, 
  gamescorsievenaway, 
  gamescorsicloseevenhome, 
  gamescorsicloseevenaway, 
  gamescorsiclosehome,
  gamescorsicloseaway, 
  gamescorsitiedhome, 
  gamescorsitiedaway, 
  gamepowerplayhome, 
  gamepowerplayaway,
  gamesshothome,
  gamesshotaway,
  gamesshot25home,
  gamesshot25away,
  gamesgoalhome,
  gamesgoalaway,
  gamesgoal25home,
  gamesgoal25away,
  gamesshotwristhome,
  gamesshotwristaway,
  gamesshotslaphome,
  gamesshotslapaway,
  gamesshottiphome,
  gamesshottipaway,
  gamesgoalwristhome,
  gamesgoalwristaway,
  gamesgoalslaphome,
  gamesgoalslapaway,
  gamesgoaltiphome,
  gamesgoaltipaway,
  gamesgoalpphome,
  gamesgoalppaway,
  gamesgoalsclosehome,
  gamesgoalscloseaway,
  gamesgoalsfirsthome,
  gamesgoalsfirstaway,
  gamesgoalssecondhome,
  gamesgoalssecondaway,
  gamesgoalsthirdhome,
  gamesgoalsthirdaway,
  gamesgoalsotsohome,
  gamesgoalsotsoaway,
  gameoffeventshome,
  gameoffeventsaway,
  gameoffeventsclosehome,
  gameoffeventscloseaway,
  gamescorsiclose25home,
  gamescorsiclose25away)



# s0910results = s0910results %>%
# mutate(gamewinner = ifelse(home.score > away.score, hometeam, ifelse(home.score < away.score, awayteam, 'TIE')))

# s0910results = s0910results %>%
# mutate(corsiwinner = ifelse(gamescorsihome > gamescorsiaway, hometeam, ifelse(gamescorsihome < gamescorsiaway, awayteam, 'TIE')))

# s0910results = s0910results %>%
# mutate(corsievenwinner = ifelse(gamescorsievenhome > gamescorsievenaway, hometeam, ifelse(gamescorsievenhome < gamescorsievenaway, awayteam, 'TIE')))

# s0910results = s0910results %>%
# mutate(corsiclosewinner = ifelse(gamescorsiclosehome > gamescorsicloseaway, hometeam, ifelse(gamescorsiclosehome < gamescorsicloseaway, awayteam, 'TIE')))

# s0910results = s0910results %>%
# mutate(corsicloseevenwinner = ifelse(gamescorsicloseevenhome > gamescorsicloseevenaway, hometeam, ifelse(gamescorsicloseevenhome < gamescorsicloseevenaway, awayteam, 'TIE')))

# s0910results = s0910results %>%
# mutate(penaltiesdrawnwinner = ifelse(gamepowerplayhome > gamepowerplayaway, hometeam, ifelse(gamepowerplayhome < gamepowerplayaway, awayteam, 'TIE')))


# s0910results = s0910results %>%
# mutate(corsimatchwinner = ifelse(corsiwinner == gamewinner, 1, 0))

# s0910results = s0910results %>%
# mutate(corsievenmatchwinner = ifelse(corsievenwinner == gamewinner, 1, 0))

# s0910results = s0910results %>%
# mutate(corsiclosematchwinner = ifelse(corsiclosewinner == gamewinner, 1, 0))

# s0910results = s0910results %>%
# mutate(corsicloseevenmatchwinner = ifelse(corsicloseevenwinner == gamewinner, 1, 0))

# s0910results = s0910results %>%
# mutate(penaltiesdrawnmatchwinner = ifelse(penaltiesdrawnwinner == gamewinner, 1, 0))

# s0910results %>% 
# group_by(corsimatchwinner) %>% 
# summarise(counts = n())

# s0910results %>% 
# group_by(corsievenmatchwinner) %>% 
# summarise(counts = n())

# s0910results %>% 
# group_by(corsiclosematchwinner) %>% 
# summarise(counts = n())

# s0910results %>% 
# group_by(corsicloseevenmatchwinner) %>% 
# summarise(counts = n())

# s0910results %>% 
# group_by(penaltiesdrawnmatchwinner) %>% 
# summarise(counts = n())



# taking all that into account, so far it looks like corsi close is way more predictive 
# than corsi or corsi even or corsi even close. further, it looks like having more powerplays 
# actually hurts your chances of winning the game.  really??  crazy!

# group by team, split into first half and second half as well as even and odd games 


s0910home = s0910results

s0910home = mutate(s0910home, team = hometeam)

s0910away = s0910results

s0910away = mutate(s0910away, team = awayteam)

s0910teamresults = rbind_all(list(s0910home, s0910away))

View(arrange(s0910teamresults, gcode))

s0910teamresults = s0910teamresults %>%  
group_by(team) %>%
mutate(gamerank = row_number(gcode))

s0910teamresults = s0910teamresults %>%  
mutate(win = ifelse(team == hometeam, ifelse(home.score > away.score, 1,0), ifelse(away.score > home.score, 1,0)))

s0910teamresults = s0910teamresults %>%  
group_by(team) %>%
arrange(team, gamerank) %>%
mutate(seasonwins = cumsum(win))

s0910teamresults = s0910teamresults %>%  
mutate(winpct = seasonwins/gamerank)


s0910teamresults = s0910teamresults %>%  
group_by(gcode) %>%
mutate(gamecoderank = row_number(gcode))


s0910teamresults = s0910teamresults %>%  
group_by(gcode) %>%
arrange(gcode, gamecoderank) %>%
mutate(winpctopp = ifelse(gamecoderank == 1, lead(winpct), lag(winpct)))






# win pct excluding first 10 games and then second half strength of schedule by win pct
# wins against over 500 winpct teams
# games against over 500 winpct teams
# wins excluding first 10 games of season
# wins including only last 10 of first half of season

# break out for/against to include defensive stats 

# percents of team-for for goal stats intead of just ratio of for/against
# see which variables have lowest (highest) rank and correlation when adding all seasons tegether
# do odd/even games as well and 21/61 & 61/21 splits 
# predict playoffs 
# try predicting goals, not wins 
# do regression 
# pick single game winners 

# home/away games 
# longest win streak
# longest losing streak
# games per day
# period wins 


# control for outliers 
# double check all data quality and code 
# get rid of variables with small sample sizes
# think about which variables should be percents vs corsi style percents vs raw values 


# now segment, open corsi, corsi magnitude not just direction, closed corsi, spot check games and look out for missing data
# build automated scouting report for any game, django app or something




x = filter(s0910teamresults, gamerank < 42)
y = filter(s0910teamresults, gamerank > 42)

x = x %>% group_by(team) %>%
  mutate(corsifor1 = ifelse(team == hometeam, gamescorsihome, gamescorsiaway)) %>%
  mutate(corsiforeven1 = ifelse(team == hometeam, gamescorsievenhome, gamescorsievenaway)) %>%
  mutate(corsiforclose1 = ifelse(team == hometeam, gamescorsiclosehome, gamescorsicloseaway)) %>%
  mutate(corsifor25close1 = ifelse(team == hometeam, gamescorsiclose25home, gamescorsiclose25away)) %>%
  mutate(corsifortied1 = ifelse(team == hometeam, gamescorsitiedhome, gamescorsitiedaway)) %>%
  mutate(corsiforevenclose1 = ifelse(team == hometeam, gamescorsicloseevenhome, gamescorsicloseevenaway)) %>%
  mutate(offeventsfor1 = ifelse(team == hometeam, gameoffeventshome, gameoffeventsaway)) %>%
  mutate(offeventsclosefor1 = ifelse(team == hometeam, gameoffeventsclosehome, gameoffeventscloseaway)) %>%
  mutate(goalsfor1 = ifelse(team == hometeam, gamesgoalhome, gamesgoalaway)) %>%
  mutate(goalsotsofor1 = ifelse(team == hometeam, gamesgoalsotsohome, gamesgoalsotsoaway)) %>%
  mutate(goalsthirdfor1 = ifelse(team == hometeam, gamesgoalsthirdhome, gamesgoalsthirdaway)) %>%
  mutate(goalssecondfor1 = ifelse(team == hometeam, gamesgoalssecondhome, gamesgoalssecondaway)) %>%
  mutate(goalsfirstfor1 = ifelse(team == hometeam, gamesgoalsfirsthome, gamesgoalsfirstaway)) %>%
  mutate(goalsclosefor1 = ifelse(team == hometeam, gamesgoalsclosehome, gamesgoalscloseaway)) %>%
  mutate(goalsppfor1 = ifelse(team == hometeam, gamesgoalpphome, gamesgoalppaway)) %>%
  mutate(goalstipfor1 = ifelse(team == hometeam, gamesgoaltiphome, gamesgoaltipaway)) %>%
  mutate(goalsslapfor1 = ifelse(team == hometeam, gamesgoalslaphome, gamesgoalslapaway)) %>%
  mutate(goalswristfor1 = ifelse(team == hometeam, gamesgoalwristhome, gamesgoalwristaway)) %>%
  mutate(goals25for1 = ifelse(team == hometeam, gamesgoal25home, gamesgoal25away)) %>%
  mutate(shottipfor1 = ifelse(team == hometeam, gamesshottiphome, gamesshottipaway)) %>%
  mutate(shotslapfor1 = ifelse(team == hometeam, gamesshotslaphome, gamesshotslapaway)) %>%
  mutate(shotwristfor1 = ifelse(team == hometeam, gamesshotwristhome, gamesshotwristaway)) %>%
  mutate(shot25for1 = ifelse(team == hometeam, gamesshot25home, gamesshot25away)) %>%
  mutate(shotsfor1 = ifelse(team == hometeam, gamesshothome, gamesshotaway)) %>%

  mutate(corsiagainst1 = ifelse(team == hometeam, gamescorsiaway, gamescorsihome)) %>%
  mutate(corsiagainsteven1 = ifelse(team == hometeam, gamescorsievenaway, gamescorsievenhome)) %>%
  mutate(corsiagainstclose1 = ifelse(team == hometeam, gamescorsicloseaway, gamescorsiclosehome)) %>%
  mutate(corsiagainst25close1 = ifelse(team == hometeam, gamescorsiclose25away, gamescorsiclose25home)) %>%
  mutate(corsiagainsttied1 = ifelse(team == hometeam, gamescorsitiedaway, gamescorsitiedhome)) %>%
  mutate(corsiagainstevenclose1 = ifelse(team == hometeam, gamescorsicloseevenaway, gamescorsicloseevenhome)) %>%
  mutate(offeventsagainst1 = ifelse(team == hometeam, gameoffeventsaway, gameoffeventshome)) %>%
  mutate(offeventscloseagainst1 = ifelse(team == hometeam, gameoffeventscloseaway, gameoffeventsclosehome)) %>%
  mutate(goalsagainst1 = ifelse(team == hometeam, gamesgoalaway, gamesgoalhome)) %>%
  mutate(goalsotsoagainst1 = ifelse(team == hometeam, gamesgoalsotsoaway, gamesgoalsotsohome)) %>%
  mutate(goalsthirdagainst1 = ifelse(team == hometeam, gamesgoalsthirdaway, gamesgoalsthirdhome)) %>%
  mutate(goalssecondagainst1 = ifelse(team == hometeam, gamesgoalssecondaway, gamesgoalssecondhome)) %>%
  mutate(goalsfirstagainst1 = ifelse(team == hometeam, gamesgoalsfirstaway, gamesgoalsfirsthome)) %>%
  mutate(goalscloseagainst1 = ifelse(team == hometeam, gamesgoalscloseaway, gamesgoalsclosehome)) %>%
  mutate(goalsppagainst1 = ifelse(team == hometeam, gamesgoalppaway, gamesgoalpphome)) %>%
  mutate(goalstipagainst1 = ifelse(team == hometeam, gamesgoaltipaway, gamesgoaltiphome)) %>%
  mutate(goalsslapagainst1 = ifelse(team == hometeam, gamesgoalslapaway, gamesgoalslaphome)) %>%
  mutate(goalswristagainst1 = ifelse(team == hometeam, gamesgoalwristaway, gamesgoalwristhome)) %>%
  mutate(goals25against1 = ifelse(team == hometeam, gamesgoal25away, gamesgoal25home)) %>%
  mutate(shottipagainst1 = ifelse(team == hometeam, gamesshottipaway, gamesshottiphome)) %>%
  mutate(shotslapagainst1 = ifelse(team == hometeam, gamesshotslapaway, gamesshotslaphome)) %>%
  mutate(shotwristagainst1 = ifelse(team == hometeam, gamesshotwristaway, gamesshotwristhome)) %>%
  mutate(shot25against1 = ifelse(team == hometeam, gamesshot25away, gamesshot25home)) %>%
  mutate(shotsagainst1 = ifelse(team == hometeam, gamesshotaway, gamesshothome)) %>%

  mutate(corsi1 = ifelse(team == hometeam, gamescorsihome/(gamescorsihome + gamescorsiaway), gamescorsiaway/(gamescorsihome+gamescorsiaway))) %>%
  mutate(corsieven1 = ifelse(team == hometeam, gamescorsievenhome/(gamescorsievenhome + gamescorsievenaway), gamescorsievenaway/(gamescorsievenhome + gamescorsievenaway))) %>%
  mutate(corsiclose1 = ifelse(team == hometeam, gamescorsiclosehome/(gamescorsiclosehome + gamescorsicloseaway), gamescorsicloseaway/(gamescorsiclosehome + gamescorsicloseaway))) %>%
  mutate(corsi25close1 = ifelse(team == hometeam, gamescorsiclose25home/(gamescorsiclose25home + gamescorsiclose25away), gamescorsiclose25away/(gamescorsiclose25home + gamescorsiclose25away))) %>%
  mutate(corsitied1 = ifelse(team == hometeam, gamescorsitiedhome/(gamescorsitiedhome + gamescorsitiedaway), gamescorsitiedaway/(gamescorsitiedhome+gamescorsitiedaway))) %>%
  mutate(corsievenclose1 = ifelse(team == hometeam, gamescorsicloseevenhome/(gamescorsicloseevenhome + gamescorsicloseevenaway), gamescorsicloseevenaway/(gamescorsicloseevenhome + gamescorsicloseevenaway))) %>%
  mutate(offevents1 = ifelse(team == hometeam, gameoffeventshome/(gameoffeventshome + gameoffeventsaway), gameoffeventsaway/(gameoffeventshome+gameoffeventsaway))) %>%
  mutate(offeventsclose1 = ifelse(team == hometeam, gameoffeventsclosehome/(gameoffeventsclosehome + gameoffeventscloseaway), gameoffeventscloseaway/(gameoffeventsclosehome+gameoffeventscloseaway))) %>%
  mutate(goals1 = ifelse(team == hometeam, gamesgoalhome - gamesgoalaway, gamesgoalaway - gamesgoalhome)) %>%
  mutate(goalsotso1 = ifelse(team == hometeam, gamesgoalsotsohome - gamesgoalsotsoaway, gamesgoalsotsoaway - gamesgoalsotsohome)) %>%
  mutate(goalsthird1 = ifelse(team == hometeam, gamesgoalsthirdhome - gamesgoalsthirdaway, gamesgoalsthirdaway - gamesgoalsthirdhome)) %>%
  mutate(goalssecond1 = ifelse(team == hometeam, gamesgoalssecondhome - gamesgoalssecondaway, gamesgoalssecondaway - gamesgoalssecondhome)) %>%
  mutate(goalsfirst1 = ifelse(team == hometeam, gamesgoalsfirsthome - gamesgoalsfirstaway, gamesgoalsfirstaway - gamesgoalsfirsthome)) %>%
  mutate(goalsclose1 = ifelse(team == hometeam, gamesgoalsclosehome - gamesgoalscloseaway, gamesgoalscloseaway - gamesgoalsclosehome)) %>%
  mutate(goalspp1 = ifelse(team == hometeam, gamesgoalpphome - gamesgoalppaway, gamesgoalppaway - gamesgoalpphome)) %>%
  mutate(goalstip1 = ifelse(team == hometeam, gamesgoaltiphome - gamesgoaltipaway, gamesgoaltipaway - gamesgoaltiphome)) %>%
  mutate(goalsslap1 = ifelse(team == hometeam, gamesgoalslaphome - gamesgoalslapaway, gamesgoalslapaway - gamesgoalslaphome)) %>%
  mutate(goalswrist1 = ifelse(team == hometeam, gamesgoalwristhome - gamesgoalwristaway, gamesgoalwristaway - gamesgoalwristhome)) %>%
  mutate(goals251 = ifelse(team == hometeam, gamesgoal25home - gamesgoal25away, gamesgoal25away - gamesgoal25home)) %>%
  mutate(shottip1 = ifelse(team == hometeam, gamesshottiphome - gamesshottipaway, gamesshottipaway - gamesshottiphome)) %>%
  mutate(shotslap1 = ifelse(team == hometeam, gamesshotslaphome - gamesshotslapaway, gamesshotslapaway - gamesshotslaphome)) %>%
  mutate(shotwrist1 = ifelse(team == hometeam, gamesshotwristhome - gamesshotwristaway, gamesshotwristaway - gamesshotwristhome)) %>%
  mutate(shot251 = ifelse(team == hometeam, gamesshot25home - gamesshot25away, gamesshot25away - gamesshot25home)) %>%
  mutate(shots1 = ifelse(team == hometeam, gamesshothome - gamesshotaway, gamesshotaway - gamesshothome)) %>%
  mutate(wins1 = ifelse(team == hometeam, ifelse(home.score > away.score, 1, 0), ifelse(away.score > home.score, 1, 0))) %>%
  mutate(wins11 = ifelse(team == hometeam, ifelse(home.score > away.score & abs(home.score - away.score) == 1, 1, 0), ifelse(away.score > home.score & abs(home.score - away.score) == 1, 1, 0))) %>%
  mutate(wins21 = ifelse(team == hometeam, ifelse(home.score > away.score & abs(home.score - away.score) == 2, 1, 0), ifelse(away.score > home.score & abs(home.score - away.score) == 2, 1, 0))) %>%
  mutate(wins31 = ifelse(team == hometeam, ifelse(home.score > away.score & abs(home.score - away.score) > 2, 1, 0), ifelse(away.score > home.score & abs(home.score - away.score) > 2, 1, 0))) %>%
  mutate(winsreg1 = ifelse(team == hometeam, ifelse(home.score > away.score & period == 3, 1, 0), ifelse(away.score > home.score & period == 3, 1, 0))) %>%
  mutate(winsregot1 = ifelse(team == hometeam, ifelse(home.score > away.score & period < 5, 1, 0), ifelse(away.score > home.score & period < 5, 1, 0))) %>%
  mutate(penaltydrawn1 = ifelse(team == hometeam, gamepowerplayhome, gamepowerplayaway)) %>%
  mutate(penaltytaken1 = ifelse(team == hometeam, gamepowerplayaway, gamepowerplayhome)) %>%
  mutate(wins5001 = ifelse(team == hometeam, ifelse(home.score > away.score & winpctopp > .5, 1, 0), ifelse(away.score > home.score & winpctopp > .5, 1, 0))) %>%
  mutate(winsexcstart1 = ifelse(team == hometeam, ifelse(home.score > away.score & gamerank > 10, 1, 0), ifelse(away.score > home.score & gamerank > 10, 1, 0))) %>%
  mutate(winsrecent1 = ifelse(team == hometeam, ifelse(home.score > away.score & gamerank > 31, 1, 0), ifelse(away.score > home.score & gamerank > 31, 1, 0))) %>%
  mutate(wins500excstart1 = ifelse(team == hometeam, ifelse(home.score > away.score & winpctopp > .5 & gamerank > 10, 1, 0), ifelse(away.score > home.score & winpctopp > .5 & gamerank > 10, 1, 0))) %>%
  mutate(games5001 = ifelse(team == hometeam, ifelse(winpctopp > .5, 1, 0), ifelse(winpctopp > .5, 1, 0))) %>%
  collect

y = y %>% group_by(team) %>%
  mutate(corsifor2 = ifelse(team == hometeam, gamescorsihome, gamescorsiaway)) %>%
  mutate(corsiforeven2 = ifelse(team == hometeam, gamescorsievenhome, gamescorsievenaway)) %>%
  mutate(corsiforclose2 = ifelse(team == hometeam, gamescorsiclosehome, gamescorsicloseaway)) %>%
  mutate(corsifor25close2 = ifelse(team == hometeam, gamescorsiclose25home, gamescorsiclose25away)) %>%
  mutate(corsifortied2 = ifelse(team == hometeam, gamescorsitiedhome, gamescorsitiedaway)) %>%
  mutate(corsiforevenclose2 = ifelse(team == hometeam, gamescorsicloseevenhome, gamescorsicloseevenaway)) %>%
  mutate(offeventsfor2 = ifelse(team == hometeam, gameoffeventshome, gameoffeventsaway)) %>%
  mutate(offeventsclosefor2 = ifelse(team == hometeam, gameoffeventsclosehome, gameoffeventscloseaway)) %>%
  mutate(goalsfor2 = ifelse(team == hometeam, gamesgoalhome, gamesgoalaway)) %>%
  mutate(goalsotsofor2 = ifelse(team == hometeam, gamesgoalsotsohome, gamesgoalsotsoaway)) %>%
  mutate(goalsthirdfor2 = ifelse(team == hometeam, gamesgoalsthirdhome, gamesgoalsthirdaway)) %>%
  mutate(goalssecondfor2 = ifelse(team == hometeam, gamesgoalssecondhome, gamesgoalssecondaway)) %>%
  mutate(goalsfirstfor2 = ifelse(team == hometeam, gamesgoalsfirsthome, gamesgoalsfirstaway)) %>%
  mutate(goalsclosefor2 = ifelse(team == hometeam, gamesgoalsclosehome, gamesgoalscloseaway)) %>%
  mutate(goalsppfor2 = ifelse(team == hometeam, gamesgoalpphome, gamesgoalppaway)) %>%
  mutate(goalstipfor2 = ifelse(team == hometeam, gamesgoaltiphome, gamesgoaltipaway)) %>%
  mutate(goalsslapfor2 = ifelse(team == hometeam, gamesgoalslaphome, gamesgoalslapaway)) %>%
  mutate(goalswristfor2 = ifelse(team == hometeam, gamesgoalwristhome, gamesgoalwristaway)) %>%
  mutate(goals25for2 = ifelse(team == hometeam, gamesgoal25home, gamesgoal25away)) %>%
  mutate(shottipfor2 = ifelse(team == hometeam, gamesshottiphome, gamesshottipaway)) %>%
  mutate(shotslapfor2 = ifelse(team == hometeam, gamesshotslaphome, gamesshotslapaway)) %>%
  mutate(shotwristfor2 = ifelse(team == hometeam, gamesshotwristhome, gamesshotwristaway)) %>%
  mutate(shot25for2 = ifelse(team == hometeam, gamesshot25home, gamesshot25away)) %>%
  mutate(shotsfor2 = ifelse(team == hometeam, gamesshothome, gamesshotaway)) %>%

  mutate(corsiagainst2 = ifelse(team == hometeam, gamescorsiaway, gamescorsihome)) %>%
  mutate(corsiagainsteven2 = ifelse(team == hometeam, gamescorsievenaway, gamescorsievenhome)) %>%
  mutate(corsiagainstclose2 = ifelse(team == hometeam, gamescorsicloseaway, gamescorsiclosehome)) %>%
  mutate(corsiagainst25close2 = ifelse(team == hometeam, gamescorsiclose25away, gamescorsiclose25home)) %>%
  mutate(corsiagainsttied2 = ifelse(team == hometeam, gamescorsitiedaway, gamescorsitiedhome)) %>%
  mutate(corsiagainstevenclose2 = ifelse(team == hometeam, gamescorsicloseevenaway, gamescorsicloseevenhome)) %>%
  mutate(offeventsagainst2 = ifelse(team == hometeam, gameoffeventsaway, gameoffeventshome)) %>%
  mutate(offeventscloseagainst2 = ifelse(team == hometeam, gameoffeventscloseaway, gameoffeventsclosehome)) %>%
  mutate(goalsagainst2 = ifelse(team == hometeam, gamesgoalaway, gamesgoalhome)) %>%
  mutate(goalsotsoagainst2 = ifelse(team == hometeam, gamesgoalsotsoaway, gamesgoalsotsohome)) %>%
  mutate(goalsthirdagainst2 = ifelse(team == hometeam, gamesgoalsthirdaway, gamesgoalsthirdhome)) %>%
  mutate(goalssecondagainst2 = ifelse(team == hometeam, gamesgoalssecondaway, gamesgoalssecondhome)) %>%
  mutate(goalsfirstagainst2 = ifelse(team == hometeam, gamesgoalsfirstaway, gamesgoalsfirsthome)) %>%
  mutate(goalscloseagainst2 = ifelse(team == hometeam, gamesgoalscloseaway, gamesgoalsclosehome)) %>%
  mutate(goalsppagainst2 = ifelse(team == hometeam, gamesgoalppaway, gamesgoalpphome)) %>%
  mutate(goalstipagainst2 = ifelse(team == hometeam, gamesgoaltipaway, gamesgoaltiphome)) %>%
  mutate(goalsslapagainst2 = ifelse(team == hometeam, gamesgoalslapaway, gamesgoalslaphome)) %>%
  mutate(goalswristagainst2 = ifelse(team == hometeam, gamesgoalwristaway, gamesgoalwristhome)) %>%
  mutate(goals25against2 = ifelse(team == hometeam, gamesgoal25away, gamesgoal25home)) %>%
  mutate(shottipagainst2 = ifelse(team == hometeam, gamesshottipaway, gamesshottiphome)) %>%
  mutate(shotslapagainst2 = ifelse(team == hometeam, gamesshotslapaway, gamesshotslaphome)) %>%
  mutate(shotwristagainst2 = ifelse(team == hometeam, gamesshotwristaway, gamesshotwristhome)) %>%
  mutate(shot25against2 = ifelse(team == hometeam, gamesshot25away, gamesshot25home)) %>%
  mutate(shotsagainst2 = ifelse(team == hometeam, gamesshotaway, gamesshothome)) %>%

  mutate(corsi2 = ifelse(team == hometeam, gamescorsihome/(gamescorsihome + gamescorsiaway), gamescorsiaway/(gamescorsihome+gamescorsiaway))) %>%
  mutate(corsieven2 = ifelse(team == hometeam, gamescorsievenhome/(gamescorsievenhome + gamescorsievenaway), gamescorsievenaway/(gamescorsievenhome + gamescorsievenaway))) %>%
  mutate(corsiclose2 = ifelse(team == hometeam, gamescorsiclosehome/(gamescorsiclosehome + gamescorsicloseaway), gamescorsicloseaway/(gamescorsiclosehome + gamescorsicloseaway))) %>%
  mutate(corsi25close2 = ifelse(team == hometeam, gamescorsiclose25home/(gamescorsiclose25home + gamescorsiclose25away), gamescorsiclose25away/(gamescorsiclose25home + gamescorsiclose25away))) %>%
  mutate(corsitied2 = ifelse(team == hometeam, gamescorsitiedhome/(gamescorsitiedhome + gamescorsitiedaway), gamescorsitiedaway/(gamescorsitiedhome+gamescorsitiedaway))) %>%
  mutate(corsievenclose2 = ifelse(team == hometeam, gamescorsicloseevenhome/(gamescorsicloseevenhome + gamescorsicloseevenaway), gamescorsicloseevenaway/(gamescorsicloseevenhome + gamescorsicloseevenaway))) %>%
  mutate(offevents2 = ifelse(team == hometeam, gameoffeventshome/(gameoffeventshome + gameoffeventsaway), gameoffeventsaway/(gameoffeventshome+gameoffeventsaway))) %>%
  mutate(offeventsclose2 = ifelse(team == hometeam, gameoffeventsclosehome/(gameoffeventsclosehome + gameoffeventscloseaway), gameoffeventscloseaway/(gameoffeventsclosehome+gameoffeventscloseaway))) %>%
  mutate(goals2 = ifelse(team == hometeam, gamesgoalhome - gamesgoalaway, gamesgoalaway - gamesgoalhome)) %>%
  mutate(goalsotso2 = ifelse(team == hometeam, gamesgoalsotsohome - gamesgoalsotsoaway, gamesgoalsotsoaway - gamesgoalsotsohome)) %>%
  mutate(goalsthird2 = ifelse(team == hometeam, gamesgoalsthirdhome - gamesgoalsthirdaway, gamesgoalsthirdaway - gamesgoalsthirdhome)) %>%
  mutate(goalssecond2 = ifelse(team == hometeam, gamesgoalssecondhome - gamesgoalssecondaway, gamesgoalssecondaway - gamesgoalssecondhome)) %>%
  mutate(goalsfirst2 = ifelse(team == hometeam, gamesgoalsfirsthome - gamesgoalsfirstaway, gamesgoalsfirstaway - gamesgoalsfirsthome)) %>%
  mutate(goalsclose2 = ifelse(team == hometeam, gamesgoalsclosehome - gamesgoalscloseaway, gamesgoalscloseaway - gamesgoalsclosehome)) %>%
  mutate(goalspp2 = ifelse(team == hometeam, gamesgoalpphome - gamesgoalppaway, gamesgoalppaway - gamesgoalpphome)) %>%
  mutate(goalstip2 = ifelse(team == hometeam, gamesgoaltiphome - gamesgoaltipaway, gamesgoaltipaway - gamesgoaltiphome)) %>%
  mutate(goalsslap2 = ifelse(team == hometeam, gamesgoalslaphome - gamesgoalslapaway, gamesgoalslapaway - gamesgoalslaphome)) %>%
  mutate(goalswrist2 = ifelse(team == hometeam, gamesgoalwristhome - gamesgoalwristaway, gamesgoalwristaway - gamesgoalwristhome)) %>%
  mutate(goals252 = ifelse(team == hometeam, gamesgoal25home - gamesgoal25away, gamesgoal25away - gamesgoal25home)) %>%
  mutate(shottip2 = ifelse(team == hometeam, gamesshottiphome - gamesshottipaway, gamesshottipaway - gamesshottiphome)) %>%
  mutate(shotslap2 = ifelse(team == hometeam, gamesshotslaphome - gamesshotslapaway, gamesshotslapaway - gamesshotslaphome)) %>%
  mutate(shotwrist2 = ifelse(team == hometeam, gamesshotwristhome - gamesshotwristaway, gamesshotwristaway - gamesshotwristhome)) %>%
  mutate(shot252 = ifelse(team == hometeam, gamesshot25home - gamesshot25away, gamesshot25away - gamesshot25home)) %>%
  mutate(shots2 = ifelse(team == hometeam, gamesshothome - gamesshotaway, gamesshotaway - gamesshothome)) %>%
  mutate(wins2 = ifelse(team == hometeam, ifelse(home.score > away.score, 1, 0), ifelse(away.score > home.score, 1, 0))) %>%
  mutate(wins12 = ifelse(team == hometeam, ifelse(home.score > away.score & abs(home.score - away.score) == 1, 1, 0), ifelse(away.score > home.score & abs(home.score - away.score) == 1, 1, 0))) %>%
  mutate(wins22 = ifelse(team == hometeam, ifelse(home.score > away.score & abs(home.score - away.score) == 2, 1, 0), ifelse(away.score > home.score & abs(home.score - away.score) == 2, 1, 0))) %>%
  mutate(wins32 = ifelse(team == hometeam, ifelse(home.score > away.score & abs(home.score - away.score) > 2, 1, 0), ifelse(away.score > home.score & abs(home.score - away.score) > 2, 1, 0))) %>%
  mutate(winsreg2 = ifelse(team == hometeam, ifelse(home.score > away.score & period == 3, 1, 0), ifelse(away.score > home.score & period == 3, 1, 0))) %>%
  mutate(winsregot2 = ifelse(team == hometeam, ifelse(home.score > away.score & period < 5, 1, 0), ifelse(away.score > home.score & period < 5, 1, 0))) %>%
  mutate(penaltydrawn2 = ifelse(team == hometeam, gamepowerplayhome, gamepowerplayaway)) %>%
  mutate(penaltytaken2 = ifelse(team == hometeam, gamepowerplayaway, gamepowerplayhome)) %>%
  mutate(wins5002 = ifelse(team == hometeam, ifelse(home.score > away.score & winpctopp > .5, 1, 0), ifelse(away.score > home.score & winpctopp > .5, 1, 0))) %>%
  mutate(winsexcstart2 = ifelse(team == hometeam, ifelse(home.score > away.score & gamerank > 10, 1, 0), ifelse(away.score > home.score & gamerank > 10, 1, 0))) %>%
  mutate(winsrecent2 = ifelse(team == hometeam, ifelse(home.score > away.score & gamerank > 31, 1, 0), ifelse(away.score > home.score & gamerank > 31, 1, 0))) %>%
  mutate(wins500excstart2 = ifelse(team == hometeam, ifelse(home.score > away.score & winpctopp > .5 & gamerank > 10, 1, 0), ifelse(away.score > home.score & winpctopp > .5 & gamerank > 10, 1, 0))) %>%
  mutate(games5002 = ifelse(team == hometeam, ifelse(winpctopp > .5, 1, 0), ifelse(winpctopp > .5, 1, 0))) %>%
  collect

s0910teamresults = rbind_all(list(x, y))


s0910corr = s0910teamresults %>%
group_by(team) %>%
summarise(corsi1 = sum(corsi1, na.rm=TRUE)/41,
          corsi2 = sum(corsi2, na.rm=TRUE)/41,
          corsieven1 = sum(corsieven1, na.rm=TRUE)/41,
          corsieven2 = sum(corsieven2, na.rm=TRUE)/41,
          corsiclose1 = sum(corsiclose1, na.rm=TRUE)/41,
          corsiclose2 = sum(corsiclose2, na.rm=TRUE)/41,
          corsi25close1 = sum(corsi25close1, na.rm=TRUE)/41,
          corsi25close2 = sum(corsi25close2, na.rm=TRUE)/41,
          corsitied1 = sum(corsitied1, na.rm=TRUE)/41,
          corsitied2 = sum(corsitied2, na.rm=TRUE)/41,
          corsievenclose1 = sum(corsievenclose1, na.rm=TRUE)/41,
          corsievenclose2 = sum(corsievenclose2, na.rm=TRUE)/41,
          offevents1 = sum(offevents1, na.rm=TRUE)/41,
          offevents2 = sum(offevents2, na.rm=TRUE)/41,          
          offeventsclose1 = sum(offeventsclose1, na.rm=TRUE)/41,
          offeventsclose2 = sum(offeventsclose2, na.rm=TRUE)/41,              
          wins1 = sum(wins1, na.rm=TRUE)/41,
          wins2 = sum(wins2, na.rm=TRUE)/41,
          goals1 = sum(goalsfor1, na.rm=TRUE)/sum(goalsagainst1, na.rm=TRUE),
          goals2 = sum(goalsfor2, na.rm=TRUE)/sum(goalsagainst2, na.rm=TRUE),
          goalsfor1 = sum(goalsfor1, na.rm=TRUE),
          goalsfor2 = sum(goalsfor2, na.rm=TRUE),
          goalsagainst1 = sum(goalsagainst1, na.rm=TRUE),
          goalsagainst2 = sum(goalsagainst2, na.rm=TRUE),
          goalsfirst1 = sum(goalsfirstfor1, na.rm=TRUE)/sum(goalsfirstagainst1, na.rm=TRUE),
          goalsfirst2 = sum(goalsfirstfor2, na.rm=TRUE)/sum(goalsfirstagainst2, na.rm=TRUE),
          goalsfirstfor1 = sum(goalsfirstfor1, na.rm=TRUE),
          goalsfirstfor2 = sum(goalsfirstfor2, na.rm=TRUE),
          goalsfirstagainst1 = sum(goalsfirstagainst1, na.rm=TRUE),
          goalsfirstagainst2 = sum(goalsfirstagainst2, na.rm=TRUE),
          goalssecond1 = sum(goalssecondfor1, na.rm=TRUE)/sum(goalssecondagainst1, na.rm=TRUE),
          goalssecond2 = sum(goalssecondfor2, na.rm=TRUE)/sum(goalssecondagainst2, na.rm=TRUE),
          goalssecondfor1 = sum(goalssecondfor1, na.rm=TRUE),
          goalssecondfor2 = sum(goalssecondfor2, na.rm=TRUE),
          goalssecondagainst1 = sum(goalssecondagainst1, na.rm=TRUE),
          goalssecondagainst2 = sum(goalssecondagainst2, na.rm=TRUE),
          goalsthird1 = sum(goalsthirdfor1, na.rm=TRUE)/sum(goalsthirdagainst1, na.rm=TRUE),
          goalsthird2 = sum(goalsthirdfor2, na.rm=TRUE)/sum(goalsthirdagainst2, na.rm=TRUE),  
          goalsthirdfor1 = sum(goalsthirdfor1, na.rm=TRUE),
          goalsthirdfor2 = sum(goalsthirdfor2, na.rm=TRUE),
          goalsthirdagainst1 = sum(goalsthirdagainst1, na.rm=TRUE),
          goalsthirdagainst2 = sum(goalsthirdagainst2, na.rm=TRUE), 
          goalsclose1 = sum(goalsclosefor1, na.rm=TRUE)/sum(goalscloseagainst1, na.rm=TRUE),
          goalsclose2 = sum(goalsclosefor2, na.rm=TRUE)/sum(goalscloseagainst2, na.rm=TRUE),  
          goalspp1 = sum(goalsppfor1, na.rm=TRUE)/sum(goalsppagainst1, na.rm=TRUE),
          goalspp2 = sum(goalsppfor2, na.rm=TRUE)/sum(goalsppagainst2, na.rm=TRUE), 
          pctgoalstipfor1 = sum(goalstipfor1, na.rm=TRUE)/sum(goalsfor1, na.rm=TRUE),
          pctgoalstipfor2 = sum(goalstipfor2, na.rm=TRUE)/sum(goalsfor2, na.rm=TRUE), 
          pctgoalstipagainst1 = sum(goalstipagainst1, na.rm=TRUE)/sum(goalsagainst1, na.rm=TRUE),
          pctgoalstipagainst2 = sum(goalstipagainst2, na.rm=TRUE)/sum(goalsagainst2, na.rm=TRUE),
          pctgoalsslapfor1 = sum(goalsslapfor1, na.rm=TRUE)/sum(goalsfor1, na.rm=TRUE),
          pctgoalsslapfor2 = sum(goalsslapfor2, na.rm=TRUE)/sum(goalsfor2, na.rm=TRUE), 
          pctgoalsslapagainst1 = sum(goalsslapagainst1, na.rm=TRUE)/sum(goalsagainst1, na.rm=TRUE),
          pctgoalsslapagainst2 = sum(goalsslapagainst2, na.rm=TRUE)/sum(goalsagainst2, na.rm=TRUE),
          pctgoalswristfor1 = sum(goalswristfor1, na.rm=TRUE)/sum(goalsfor1, na.rm=TRUE),
          pctgoalswristfor2 = sum(goalswristfor2, na.rm=TRUE)/sum(goalsfor2, na.rm=TRUE), 
          pctgoalswristagainst1 = sum(goalswristagainst1, na.rm=TRUE)/sum(goalsagainst1, na.rm=TRUE),
          pctgoalswristagainst2 = sum(goalswristagainst2, na.rm=TRUE)/sum(goalsagainst2, na.rm=TRUE),
          pctgoals25for1 = sum(goals25for1, na.rm=TRUE)/sum(goalsfor1, na.rm=TRUE),
          pctgoals25for2 = sum(goals25for2, na.rm=TRUE)/sum(goalsfor2, na.rm=TRUE), 
          pctgoals25against1 = sum(goals25against1, na.rm=TRUE)/sum(goalsagainst1, na.rm=TRUE),
          pctgoals25against2 = sum(goals25against2, na.rm=TRUE)/sum(goalsagainst2, na.rm=TRUE),
          pctshottipfor1 = sum(shottipfor1, na.rm=TRUE)/sum(shotsfor1, na.rm=TRUE),
          pctshottipfor2 = sum(shottipfor2, na.rm=TRUE)/sum(shotsfor2, na.rm=TRUE), 
          pctshottipagainst1 = sum(shottipagainst1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE),
          pctshottipagainst2 = sum(shottipagainst2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE),
          pctshotslapfor1 = sum(shotslapfor1, na.rm=TRUE)/sum(shotsfor1, na.rm=TRUE),
          pctshotslapfor2 = sum(shotslapfor2, na.rm=TRUE)/sum(shotsfor2, na.rm=TRUE), 
          pctshotslapagainst1 = sum(shotslapagainst1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE),
          pctshotslapagainst2 = sum(shotslapagainst2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE),      
          pctshotwristfor1 = sum(shotwristfor1, na.rm=TRUE)/sum(shotsfor1, na.rm=TRUE),
          pctshotwristfor2 = sum(shotwristfor2, na.rm=TRUE)/sum(shotsfor2, na.rm=TRUE), 
          pctshotwristagainst1 = sum(shotwristagainst1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE),
          pctshotwristagainst2 = sum(shotwristagainst2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE),          
          pctshot25for1 = sum(shot25for1, na.rm=TRUE)/sum(shotsfor1, na.rm=TRUE),
          pctshot25for2 = sum(shot25for2, na.rm=TRUE)/sum(shotsfor2, na.rm=TRUE), 
          pctshot25against1 = sum(shot25against1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE),
          pctshot25against2 = sum(shot25against2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE),
          shots1 = sum(shotsfor1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE),
          shots2 = sum(shotsfor2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE),
          pctgoalforpp1 = sum(goalsppfor1, na.rm=TRUE)/sum(goalsfor1, na.rm=TRUE),
          pctgoalforpp2 = sum(goalsppfor2, na.rm=TRUE)/sum(goalsfor2, na.rm=TRUE),
          pctgoalagainstpp1 = sum(goalsppagainst1, na.rm=TRUE)/sum(goalsagainst1, na.rm=TRUE),
          pctgoalagainstpp2 = sum(goalsppagainst2, na.rm=TRUE)/sum(goalsagainst2, na.rm=TRUE),
          shootingpctfor1 = sum(goalsfor1, na.rm=TRUE)/sum(shotsfor1, na.rm=TRUE),
          shootingpctfor2 = sum(goalsfor2, na.rm=TRUE)/sum(shotsfor2, na.rm=TRUE),
          shootingpctagainst1 = sum(goalsagainst1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE),
          shootingpctagainst2 = sum(goalsagainst2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE),
          pdo1 = (sum(goalsfor1, na.rm=TRUE)/sum(shotsfor1, na.rm=TRUE)) + (1 - (sum(goalsagainst1, na.rm=TRUE)/sum(shotsagainst1, na.rm=TRUE))),
          pdo2 = (sum(goalsfor2, na.rm=TRUE)/sum(shotsfor2, na.rm=TRUE)) + (1 - (sum(goalsagainst2, na.rm=TRUE)/sum(shotsagainst2, na.rm=TRUE))),
          wins11 = sum(wins11, na.rm=TRUE),
          wins12 = sum(wins12, na.rm=TRUE),
          wins21 = sum(wins21, na.rm=TRUE),
          wins22 = sum(wins22, na.rm=TRUE),
          wins31 = sum(wins31, na.rm=TRUE),
          wins32 = sum(wins32, na.rm=TRUE),
          winsreg1 = sum(winsreg1, na.rm=TRUE),
          winsreg2 = sum(winsreg2, na.rm=TRUE),
          winsregot1 = sum(winsregot1, na.rm=TRUE),
          winsregot2 = sum(winsregot2, na.rm=TRUE),
          wins5001 = sum(wins5001, na.rm=TRUE),
          wins5002 = sum(wins5002, na.rm=TRUE),
          wins500pct1 = sum(wins5001, na.rm=TRUE)/sum(games5001, na.rm=TRUE),
          wins500pct2 = sum(wins5002, na.rm=TRUE)/sum(games5002, na.rm=TRUE),
          winsexcstart1 = sum(winsexcstart1, na.rm=TRUE),
          winsexcstart2 = sum(winsexcstart2, na.rm=TRUE),
          winsrecent1 = sum(winsrecent1, na.rm=TRUE),
          winsrecent2 = sum(winsrecent2, na.rm=TRUE),
          wins500excstart1 = sum(wins500excstart1, na.rm=TRUE),
          wins500excstart2 = sum(wins500excstart2, na.rm=TRUE),
          games5001 = sum(games5001, na.rm=TRUE),
          games5002 = sum(games5002, na.rm=TRUE),
          penaltycallratio1 = sum(penaltydrawn1, na.rm=TRUE)/sum(penaltytaken1, na.rm=TRUE),
          penaltycallratio2 = sum(penaltydrawn2, na.rm=TRUE)/sum(penaltytaken2, na.rm=TRUE))




s0910finalcorr = as.data.frame(cor(select(
  s0910corr, 
  wins2, 
  wins1, 
  wins11,
  wins21,
  wins31,
  winsreg1,
  winsregot1,
  wins5001,
  wins500pct1,
  winsexcstart1,
  winsrecent1,
  wins500excstart1,
  games5001,
  games5002,
  penaltycallratio1,
  goals1, 
  goalsfor1,
  goalsagainst1,
  penaltycallratio2,
  corsi1, 
  corsieven1, 
  corsiclose1, 
  corsitied1, 
  corsievenclose1,
  corsi25close1,
  offevents1,
  offeventsclose1,
  goalsfirst1,
  goalsfirstfor1,
  goalsfirstagainst1,
  goalssecond1,
  goalssecondfor1,
  goalssecondagainst1,
  goalsthird1,
  goalsthirdfor1,
  goalsthirdagainst1,
  goalsclose1,
  goalspp1,
  pctgoalstipfor1,
  pctgoalstipagainst1,
  pctgoalsslapfor1,
  pctgoalsslapagainst1,
  pctgoalswristfor1,
  pctgoalswristagainst1,
  pctgoals25for1,
  pctgoals25against1,
  pctshottipfor1,
  pctshottipagainst1,
  pctshotslapfor1,
  pctshotslapagainst1,
  pctshotwristfor1,
  pctshotwristagainst1,
  pctshot25for1,
  pctshot25against1,
  shots1,
  pctgoalforpp1,
  pctgoalagainstpp1,
  shootingpctfor1,
  shootingpctagainst1,
  pdo1,
  pdo2)))












