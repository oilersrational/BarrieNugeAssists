##Did Barrie Steal Nuge's Assists? h/t @SportsThor
##How do we operationalize this question?
##My first thought was to find a way to scrape NHL's play-by-play data and count how many times Nuge
##touched the puck in build-up but didn't get an assist. This was wrong for a number of reasons
##Instead, I think the question is: what percentage of the goals for which Nuge was on the ice did he
##record an assist on? And is that percentage lower than his career average? Not sure, but might also be 
##helpful to get the NHL or team average, if possible
library(dplyr)
library(reshape2)
library(stats)
library(boot)
library(effectsize)
library(yarrr)
library(rvest)
library(data.table)

##So, I think I should start with every year Nuge has played.
setwd("~/Desktop/HockeyStuff/NugeAssists")
data = read.csv("Nuge.csv")

#First off, to make this easier to work with I'll collapse across all situations
data = data %>%
  filter(data$situation == "all")

#I'm left with 655 career games which is one off of the 656 wikipedia has, so I'm pretty satisfied with that
#What is the goal?
#1. get all the games Nuge has been on the ice for at least one goal for
#2. for each season, calculate his assists per on-ice goal
#3. compare that to this season when he's playing with Barrie
#4. I'll have to look at their WOWY, but it might be technically correct to add Nuge's TOI w/o Barrie this year
##to the comparison group and only use his TOI with Barrie as the test group

#points per game
#First, to make life easy
data[,10] = 1
colnames(data)[10] = "games"
ppgdata = aggregate(cbind(I_F_points, games) ~ season, data, sum)

ppgdata = ppgdata %>%
  mutate(PPG = I_F_points/games)

ppgdata$season = factor(ppgdata$season)

ggplot(data = ppgdata, aes(x = season, y = PPG, group = 1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  labs(x = 'Season', y = 'Points per Game', title = 'Ryan Nugent-Hopkins Points per Game by Season') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        title = element_text(size = 12))

#anyway,
data = data %>%
  filter(data$OnIce_F_goals > 0)

#So that leaves 440/655 games, or ~67% in which Nuge has been on the ice for a goal for




#let's create assists per goal
data = data %>%
  mutate(I_F_assists = I_F_primaryAssists + I_F_secondaryAssists)

data = data %>%
  mutate(APG = I_F_assists/OnIce_F_goals)

data = data %>%
  mutate(SAPG = I_F_secondaryAssists/OnIce_F_goals)

x = split(data, data$season)
data2011 = x$`2011`
data2012 = x$`2012`
data2013 = x$`2013`
data2014 = x$`2014`
data2015 = x$`2015`
data2016 = x$`2016`
data2017 = x$`2017`
data2018 = x$`2018`
data2019 = x$`2019`
data2020 = x$`2020`
##So now I have one data frame per year and the relevant variables

#If we plot the full data, you see something weird where Nuge sometimes gets more assists than goals for which he 
#was on the ice. This likely means he made a pass and maybe changed before thee goal went in. My first
#instinct was to remove these (after all, the whole entire point is comparing his assists to ON ICE goals)
#however, I decided not to remove them because there is almost certainly some games where he:
#1. had an assist on an off-ice goal, AND
#2. did not have more than one assist per goal
#In other words, there's probably more of thesee hidden in the data, so let's leave them and hope they wash
#each other out.
#There's also the possibility vis-a-vis the Barrie question that he scooped some of theese specific ones 
#from Nuge anyway, so I think it's best to leave them
plot(data$APG)

#So, for each season, we have Nuge's assists and secondary assists per game.
#The question is how to compare them scientifically. Maybe it's best to just visualize them first - for 
#example, if NUge had a much higher assists-per-goal this year than any other, we could pretty much dismiss 
#that hypothesis out of hand
plotdataAPG = aggregate(APG ~ season, data, mean)
plotdataSAPG = aggregate(SAPG ~ season, data, mean)
plotdataAPG$season = factor(plotdataAPG$season)
plotdataSAPG$season = factor(plotdataSAPG$season)

#Here's regular assists
ggplot(data = plotdataAPG, aes(x = season, y = APG, group = 1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  labs(x = 'Season', y = 'Assists per On-Ice Goal', title = 'Ryan Nugent-Hopkins Assists per On-Ice Goal by Season') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        title = element_text(size = 12))

#And secondary assists
#If it were the case that Barrie was 'interceoting' an otherwise RNH secondary assist, this is where we
#would have seen the impact
#Based on this, very very unlikely that is what was happening (RNH has two Barrie-less worse years than this one)
ggplot(data = plotdataSAPG, aes(x = season, y = SAPG, group = 1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  labs(x = 'Season', y = '2nd Assists per On-Ice Goal', title = 'Ryan Nugent-Hopkins 2nd Assists per On-Ice Goal by Season') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        title = element_text(size = 12))

#The first of these, however, is troubling. This season was RNH's lowest APG by a mile.
#So, the mechanism might be something like: Barrie gets the puck immediately to a skilled player who
#is not Nuge, and the puck goes in the net before Nuge can touch it.

#Initially, seems plausible that Barrie ruined Nuge's assists-per-goal this year, but this alone can't
#isolate Barrie as the problem. In fact, I'm not sure of an analysis that can. 
#But, I can think of at least three that would rule Barrie out as the problem:
#1. Nuge's plus Barrie's APG is still sub-normal for Nuge (cause then Barrie couldn't make up the dif)
#2. Nuge's drop is mostly attributable to goals scored when Barrie is not on the ice.
#3. This year's drop isn't large enough to be certain that it isn't due to chance


#1. I'll just get Barrie's, and add it to the graph
data2 = read.csv("Barrie.csv")
data2 = data2 %>%
  filter(data2$situation == "all")
data2 = data2 %>%
  filter(data2$OnIce_F_goals > 0)

data2 = data2 %>%
  mutate(I_F_assists = I_F_primaryAssists + I_F_secondaryAssists)

data2 = data2 %>%
  mutate(APG = I_F_assists/OnIce_F_goals)

data2 = data2 %>%
  mutate(SAPG = I_F_secondaryAssists/OnIce_F_goals)
BarrieAPG = aggregate(APG ~ season, data2, mean)

#removing 2011 where he had 0 points in 10 games
BarrieAPG = BarrieAPG %>%
  filter(BarrieAPG$season != 2011)

plotdata = NULL
plotdataAPG[,3] = "Nuge"
BarrieAPG[,3] = "Barrie"
plotdata = rbind(plotdata, plotdataAPG, BarrieAPG)



ggplot(data = plotdata, aes(x = season, y = APG, group = V3)) +
  geom_line(aes(color = V3, linetype = V3))+
  geom_point(aes(color = V3)) +
  theme_minimal() +
  labs(x = 'Season', y = 'Assists per On-Ice Goal', title = 'RNH and Barrie Assists per On-Ice Goal by Season') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        title = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
  
#That certainly is suspicious

#2. I think here I'll just first see how much TOI Nuge has without Barrie (if not enough, I'll leave it theree)
#So, from Natural Stat Trick, Nuge has 454:55 with Barrie this year and 625:15 without, so we should be able to work with that
#47 GF with, 30 without
#Unfortunately, I don't know how to get Nuge's assists per on-ice goal with and without Barrie, so I can't answer this one
#maybe I can actually dig out my scraper and see what I can get from the NHL's play-by-play stats
#Can't get the Evolving-Wild scraper to work, let's see if I can use my shitty one instead
#This section is commented out becuase I cannot get my scraper to work, but I wanted to leave the code 
#here in case someone wants to try

# games_vec = c(as.character(seq(20001, 20673)))
# a = "http://www.nhl.com/scores/htmlreports/20202021/PL0"
# b = ".HTM"
# 
# fulldf = NULL
# 
# #This is the essencee of my scraper - read every play-by-play file the NHL has for this season and extract 
# #the events I'm looking for (in this case, Oiler goals)
# for(i in games_vec){
#   page = paste(a, i, b, sep = "")
#   pbp2020 = read_html(page)
#   
#   extract <- as.data.frame(pbp2020 %>% 
#     html_nodes("tr") %>% 
#     html_text())
#   colnames(extract) = "coilumn"
#   index = nchar(extract$coilumn) > 200
#   extract = as.data.frame(extract[index,])
#   colnames(extract) = "coilumn"
#   extract = as.data.frame(extract[extract$coilumn %like% "GOAL\r\nEDM",])
#   fulldf = rbind(fulldf, extract)
# }
# 
# fulldf[100,]
# colnames(fulldf) = "coilumn"



##3. Is the decrease in Nuge's APG larger than might be expected by chance?
#Luckily, my background makes this easy
#from here, we can see that neither of the years we want to compare (2019 & 2020) has a normal distribution
plot(density(data2019$APG))
plot(density(data2020$APG))
#ErikKarlssonNotEvenClose.gif

#And one of my favourite ways to depict data where it's helpful to show the distribution:
#first bind the data in a usable way
vpdata = rbind(data2019, data2020)
vpdata$season = factor(vpdata$season)

pirateplot(formula = APG ~ season, 
           data = vpdata,
           main = "RNH Assists per On-Ice Goal by Season",
           xlab = "Season",
           ylab = "Assists per On-Ice Goal")

#You might be able to see why it's difficult to compare hese groups.
#Very few acual data points are near the mean
#The majority of the points are at the extremes (especially near 0)
#So 2020 is certainly lower, but enough to read into it?
#This is also a good example of why traditional mean comparison is maybe inappropriate here

#Here's how we would compare them if they were normally distributed
t.test(data2019$APG,
       data2020$APG,
       alternative = "g",
       paired = F)

#You can see there's no enough evidence to rejecet the notion that the last two years are different only 
#because of chance

#Beecause they're nonnormal, it gets a little hairy to compare them using normal methods like a t test
#Here's a nonparametric test that doesn't assume our variables (Nuge's 2019 and 2020 APG) are normally
#distributed
wilcox.test(data2019$APG,
            data2020$APG,
            alternative = "g")
#closer, but the same thing. (We can't reject the hypothesis
#that Nuge's APG was equal in the two seasons)

#maybe one last hail mary lets try it with a regression
data[161] = "NoBarrie"
colnames(data)[161] = "cond"
data = within(data, cond[season == "2020"] <- 'Barrie')

testmod = lm(APG ~ cond, data)
summary(testmod)
#Again, inconclusive
#I could add more variables to this modeel to scoop up some error variance, but that seems like
#it would be against the spirit of what I'm trying to do here

#It's a little bit hard to square these results with the apparent size of that difference. Why?
#Because the distribution of the data is weird, and as such the spread of that data is large,
#and the difference between the means (~.36 vs ~.24)isn't very large in relation to that spread
#So, let's use a little bit more sensitive method. One that can take into account the average difference 
#in APG for every player in the league from last year to this. (or in other words, one that can factor in our prior knowledege
#about what size this difference might reasonably be)
skaters19 = read.csv("skaters19.csv")
skaters20 = read.csv("skaters20.csv")

skaters19 = skaters19 %>%
  filter(skaters19$situation == "all")
skaters20 = skaters20 %>%
  filter(skaters20$situation == "all")

#Will be less noisy if we only include players with more than say 30 games played in both seasons
skaters19 = skaters19 %>%
  filter(skaters19$games_played >= 30)
skaters20 = skaters20 %>%
  filter(skaters20$games_played >= 30)

#Now let's keep only the columns we need
skaters19 = skaters19[,c(2, 3, 28, 29, 94)]
skaters20 = skaters20[,c(2, 3, 28, 29, 94)]

#and do the same mutation
skaters19 = skaters19 %>%
  mutate(I_F_assists = I_F_primaryAssists + I_F_secondaryAssists)
skaters20 = skaters20 %>%
  mutate(I_F_assists = I_F_primaryAssists + I_F_secondaryAssists)

skaters19 = skaters19 %>%
  mutate(APG = I_F_assists/OnIce_F_goals)
skaters20 = skaters20 %>%
  mutate(APG = I_F_assists/OnIce_F_goals)

bayesData = merge(skaters19, skaters20, by = "name", suffixes = c("2019", "2020"))
bayesData = bayesData %>%
  mutate(priorDist = APG2019 - APG2020)

#You can see here that these data are *pretty* normal
plot(density(bayesData$priorDist))

cohens_d(skaters19$APG,
         skaters20$APG,
         pooled_sd = T)
#Here's the average number of standard deviations NHL players' APG changed from 19 to 20.
#Specifically, I want to know what width parameter to apply to my Bayesian prior such that 80% of the prior
#cauchy distribution is contained within the body 
scale = sd(data2019$APG)
scale
#here we make the assumption that all players have roughly equal variance in their APG variable
bayesData$priorDist = bayesData$priorDist/scale
#These aren't Z-scores, they're estimated effect sizes for each NHLer
#They're around 0 because there is approximately an effect size of 0
#They're scaled to our estimate of each individual's APG variance, which we assume is roughly
#Equal to RNH's
#let's see what effect sizes correspond to the middle 80% of NHLers
#let's get the dimensions of that column of data
qmean = mean(bayesData$priorDist)
qmean
qsd = sd(bayesData$priorDist)
qsd

#and we can now find what the middle 80% of effects are
#In other words, the population of 2019 -> 2020 APG effects has a mean of -0.01307278 and a standard
#deviation of 0.2550507, so we can figuree out what are the most likely effects and see how different
#Nuge's is from those.

#qnorm is our friend here
#because we want the middle 80%, we're actually interested in the top and bottom ten percent, and the
#value, which in this case are actually effect sizes, that corresponds to each
effect80top = qnorm(0.9, mean = qmean, sd = qsd)
effect80top
effect80bot = qnorm(0.1, mean = qmean, sd = qsd)
effect80bot
#so, let's split the difference and say 80% of the distribution of effects is between -.15 and .15

#Some trial and error to get this to equal.8
rwidth = pcauchy(.15, 0, 0.05) - pcauchy(-.15, 0, 0.05)
rwidth
#close enough.
#now, if we conduct a Bayesian analysis of Nuge's 2019-2020 APG drop, we do so with the prior that
#these effects are really small, and our corresponding r width parameter is .05, which is extremely 
#thin as far as priors go
#this is a Bayesian t-test
ttestBF(data2019$APG,
        data2020$APG,
        rscale = .05)
#So, all that work for an analysis that is almost perfectly inconclusive
#womp womp

#For the visualization of this, I'll cheat and use JASP
x = as.data.frame(rbind(data2019, data2020))
x = x[,c(2, 159)]
write.csv(x, "NugeBayes.csv")




















