#####Senate Election Forecast 2022 Using Markov Chain Monte Carlo Sampling and Metropolis-Hastings#######
getwd()
setwd('/Users/91799/Downloads/Senate_election_forecast_2022-master/senate_election_forecast_2022-master')
rm(list=ls())

# Get data from csv
polls <- read.csv("all_polls.csv")

dim(polls)
str(polls)


#Get the summary of the data
summary(polls)

lvpolls <- polls[polls[,5]=='100% likely' | polls[,5]=='Extremely likely',]
dlvpolls <- lvpolls[lvpolls[,6]=='Democratic'|lvpolls[,6]=='Republican',]

library(ggplot2)
library(ggmap)
# Get unique dates

#FIlter by male voters

male_count <- filter(polls, Question_3 == 'Male')
male_count
library(dplyr)

df_male <- data.frame(table(male_count$Question_2))
df_male <- df_male %>% slice(-c(1))
df_male
plot(df_male$Freq, main = "Choice of MLmale Voters", xlab = "parties", ylab = "votes", type = "o")

ggplot(aes(x = Var1, y = Freq), data = df_male) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of Male Voters')


#FIlter by male voters

female_count <- filter(polls, Question_3 == 'Female')
female_count

df_female <- data.frame(table(female_count$Question_2))
df_female <- df_female %>% slice(-c(1))
df_female
ggplot(aes(x = Var1, y = Freq), data = df_female) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of Female Voters')

#Filter by Age Groups 18-24
age_1 <- filter(polls, Question_4 == '18-24')
age_1 <- data.frame(table(age_1$Question_2))
age_1 <- age_1%>% slice(-c(1))

ggplot(aes(x = Var1, y = Freq), data = age_1) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of age Groups 18-24')

#Filter by Age Groups 25-34
age_2 <- filter(polls, Question_4 == '25-34')
age_2 <- data.frame(table(age_2$Question_2))
age_2 <- age_2%>% slice(-c(1))

ggplot(aes(x = Var1, y = Freq), data = age_2) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of age Groups 25-34')

#Filter by Age Groups 35-44
age_3 <- filter(polls, Question_4 == '35-44')
age_3 <- data.frame(table(age_3$Question_2))
age_3 <- age_3%>% slice(-c(1))

ggplot(aes(x = Var1, y = Freq), data = age_3) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of age Groups 35-44')

#Filter by Age Groups 65+
age_6 <- filter(polls, Question_4 == '65+')
age_6 <- data.frame(table(age_6$Question_2))
age_6 <- age_6%>% slice(-c(1))

ggplot(aes(x = Var1, y = Freq), data = age_6) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of age Groups 65+')


#Filter by Age Groups 45-54
age_4 <- filter(polls, Question_4 == '45-54')
age_4 <- data.frame(table(age_4$Question_2))
age_4
age_4 <- age_4%>% slice(-c(1))

ggplot(aes(x = Var1, y = Freq), data = age_4) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of age Groups 45-54')

#Filter by Age Groups 55-64
age_5 <- filter(polls, Question_4 == '55-64')
age_5 <- data.frame(table(age_5$Question_2))
age_5
age_5 <- age_5%>% slice(-c(1))

ggplot(aes(x = Var1, y = Freq), data = age_5) + geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('parties') +
  ylab('Vote Count') +
  ggtitle('Choice of age Groups 55-64')


dates=unique(polls[,1])
npolls=length(dates) # number of distinct polls

#Get the Info about Unique Values
gender <- unique(polls[["Question_3"]])
table(polls$Question_3)

#Get the info about the age groups 
age <- unique(polls["Question_4"])
table(polls$Question_4)
#In total there are 6 age groups starting from 18 to 65+

#Likeliness
likes <- unique(polls["Question_1"])
table(polls$Question_1)
#Based on the likeliness the weightage is assigned by the survey

#parties
party <- unique(polls["Question_2"])
table(polls$Question_2)
#Accoriding to the above data people have been classified into 4 groups Democratic, Republican, Undecided, Other


# Get unique states, alphabetical
states = sort(unique(polls[,2]))
nstates = length(states) # number of states + DC

# Make vector with number of electoral votes in each state
evotes = c(3,9,6,11,55,9,7,3,3,29,16,4,6,4,20,11,6,8,8,
           11,10,4,16,10,10,6,3,15,3,5,4,14,5,6,29,18,7,
           7,20,4,9,3,11,38,6,13,3,12,10,5,3)

# Get outcome of polls
nn = matrix(-99,nstates,npolls) # Record total decided likely voters here
outcomes = matrix(-99,nstates,npolls) # Record successes here
for ( ii in 1:length(states) ) {
  for ( jj in 1:length(dates) ) {
    n = sum(dlvpolls$Date == dates[jj] & dlvpolls$Geography == states[ii])
    y = sum(dlvpolls$Date == dates[jj] & dlvpolls$Geography == states[ii] &
              dlvpolls$Question..2.Answer == 'Democratic')
    nn[ii,jj] = n
    outcomes[ii,jj] = y
  }
}

# Define prior on p_i
p.hypers <- matrix(2,nstates,2) # column 1 is shape, col 2 is rate for beta dist
prev_elections_table <- 
  read.csv("modern_results_by_state.csv")
npe <- dim(prev_elections_table)[2] -1 # Number of previous elections
prev_elections <- prev_elections_table[,2:(npe+1)]
# dem_rep_vics will hold number of dem and rep victories in previous elections
dem_rep_vics <- matrix(-99,nstates,2)
for ( i in 1:nstates ) { 
  dem_rep_vics[i,] = c(sum(prev_elections[i,]), npe-sum(prev_elections[i,]) )
}
#repvics = rep(10,nstates) - demvics
p.hypers = p.hypers + dem_rep_vics

# MCMC settings
M = 1e5 # Total number of samples (including burn-in)
burn_in = 5e3 # floor(M/3)

# MCMC loop function
forecast <- function(a0, 
                     M,
                     burnIn = 0,
                     p=rep(0.5,51), 
                     ns = nn, 
                     poll.outcomes = outcomes, 
                     e.votes = evotes, 
                     p.hyper = p.hypers, 
                     sigma = 1) {
  
  # Get some initial settings
  alpha0  = p.hyper[,1]
  beta0   = p.hyper[,2]
  nstates = dim(poll.outcomes)[1]
  a0.vec  = rep(a0,npolls)^seq(npolls-1,0) # Get vector of decreasing powers of a0
  g0      = log(a0/(1-a0)) # logit transform to eliminate boundary constraints
  # Get parameters for initial beta draw
  alpha   = alpha0 + poll.outcomes %*% a0.vec
  beta    = beta0 + (ns - poll.outcomes) %*% a0.vec
  
  # Set up vehicles for records:
  a0.rec = rep(-99,M)
  accept.rec = rep(-99,M)
  r.rec = rep(-99,M)
  p.rec = matrix(-99,M,nstates)
  state.results.rec = matrix(-99,M,nstates)
  forecast.rec = rep(-99,M)
  
  for (ii in 1:M) {
    
    ### MH step to get a0
    g0.s = rnorm(1,g0,sigma) # Draw new g0 candidate
    a0.s = exp(g0.s)/(1+exp(g0.s)) # Reverse logit trans
    a0.s.vec = rep(a0.s,npolls)^seq(npolls-1,0)
    alpha.s = alpha0 + poll.outcomes %*% a0.s.vec
    beta.s  = beta0 + (ns - poll.outcomes) %*% a0.s.vec
    
    mh.lnum <- sum( dbeta(p, alpha.s, beta.s, log = T) + log(a0.s*(1-a0.s)) ) # log of numerator of MH acceptance ratio
    mh.lden <- sum( dbeta(p, alpha, beta, log = T) + log(a0*(1-a0)) ) # log of denominator of MH acceptance ratio
    r = exp(mh.lnum - mh.lden) # acceptance ratio
    
    accept = 0 # logical to tell us whether new candidate accepted
    if (runif(1) < r) {
      a0 = a0.s
      alpha = alpha.s
      beta = beta.s
      g0 = g0.s
      a0.vec = a0.s.vec
      accept = 1
    }
    
    ### Draw p_i from conditional dist for each state
    p = rbeta(nstates, alpha, beta)
    
    ### Generate state outcomes
    state.results = rbinom(nstates,1,p)
    
    ### Generate election outcome
    forecast=0
    hrc.evotes = state.results %*% e.votes
    if (hrc.evotes >= 270) {forecast = 1} # Democratic victory
    
    ### Record things
    a0.rec[ii] = a0
    accept.rec[ii] = accept
    r.rec[ii] = r
    p.rec[ii,] = p
    state.results.rec[ii,] = state.results
    forecast.rec[ii] = forecast
    
    ### Get periodic update
    if (ii %% 1000 == 0) {
      par(mfrow=c(2,2))
      plot(a0.rec[1:ii])
      plot(p.rec[1:ii,10])
      plot(state.results.rec[(ii-100):ii,40])
      plot(forecast.rec[(ii-100):ii])
    }
  }
  
  nonBurnIn = M - burnIn
  return(list("forecasts"       = tail(forecast.rec,nonBurnIn), 
              "state.forecasts" = tail(p.rec,nonBurnIn),
              "a0"              = tail(a0.rec,nonBurnIn), 
              "accept"          = tail(accept.rec,nonBurnIn), 
              "state.results"   = tail(state.results.rec,nonBurnIn)))
  
}

# Run MCMC and get prediction
res <- forecast(.9,M,burn_in)
prediction <- sum(res$forecasts)/length(res$forecasts)
prediction # This is the estimated probability of Democratic victory


library(coda)

# Get results for each state
state.predictions <- apply(res$state.forecasts,2,mean)
pred.mcmc = as.mcmc(res$state.forecasts) # Coerce the vector into a MCMC object
# pred.mcmc.hpd gives a HPD interval for the probability of each state
# going for Democratic.
pred.mcmc.hpd = round(HPDinterval(pred.mcmc, prob = 0.9),3)                  # Find 95% HPD interval for theta using the CODA function
# The following loop makes a table with one line per state, along with
# expected victor in that state, estimated prob. of Democratic victory, 
# and .9 HPD interval of Democratic victory. In addition, if a state's 
# .9 HPD interval inclues .5, then that state is labelled as a 
# swing state.
for (ii in 1:nstates) {
  safety = '  Swing  ' 
  if (0.5 > pred.mcmc.hpd[ii,2]) {safety = '         '}
  if (0.5 < pred.mcmc.hpd[ii,1]) {safety = '         '}
  winner = '  Democratic  '
  if (state.predictions[ii]<0.5) {winner = '  Republican    '}
  print( paste( states[ii], 
                winner,
                formatC(state.predictions[ii],format='f',digits=3),
                safety, 
                formatC(pred.mcmc.hpd[ii,1],format='f',digits=3), 
                formatC(pred.mcmc.hpd[ii,2],format='f',digits=3)),
         quote=F)
}

par(mfrow=c(1,1))

## Examine senate prediction
pres.mcmc = as.mcmc(res$forecasts)
#Check autocorrelation of senate prediction
autocorr.plot(pres.mcmc)
# Check effective sample size of senate prediction
effectiveSize(pres.mcmc)

## Examine state predictions
plot(pred.mcmc, ask = FALSE)
# Check autocorrelation of state p_i
autocorr.plot(pred.mcmc, ask = FALSE)
# Check effective sample size of state p_i
effectiveSize(pred.mcmc)
# Now let's do that for some particular states:
plot(pred.mcmc[,41]) 
# California Data
autocorr.plot(pred.mcmc[,5]) 
# Florida Data
effectiveSize(pred.mcmc[,10]) 

## Examine a0
a0.mcmc = as.mcmc(res$a0)
# Check autocorrelation of a0
autocorr.plot(a0.mcmc)
# Check effective sample size of a0
effectiveSize(a0.mcmc)
mean(a0.mcmc)
HPDinterval(a0.mcmc,prob=0.95)

plot(res$a0, typ = 'l')
plot(res$state.forecasts[,10], typ = 'l')