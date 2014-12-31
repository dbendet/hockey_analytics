# http://www.acthomas.ca/howto-use-nhlscrapr-to-collect-nhl-rtss-data/

# set working directory 
setwd("~/Documents/Code/r")

## Install and load the package.

# install.packages("nhlscrapr", repos="http://cran.rstudio.com/")
library(nhlscrapr)

## compile.all.games()

all.games <- full.game.database()
these.games <- subset(all.games, season == 20132014)

## View(these.games)

## Now, run the compile.all.games with this.

compile.all.games(new.game.table=these.games)

## You can load an individual game file to see what's in it:
# load("nhlr-data/20132014-21201-processed.RData")
# summary(game.info)
# head(game.info$playbyplay)

## But really, the action is here:
# load ("nhlscrapr-probs.RData")
# load("/Users/dbendet/source-data/nhlscrapr-20132014.RData")

## Want to save the individual parts as CSV files?
write.csv (games, "nhlgames.csv")
write.csv (grand.data, "nhlpbp.csv")
write.csv (roster.master, "nhlroster.csv")

