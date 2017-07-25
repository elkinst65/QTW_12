#
# 1. Consider other summary statistics for the branching process.
# 2. Incorporate them into exptOne().
# 3. Carry out a simulation study and create a visualization of the
# simulation that uses these statistics.
# 4. Do they confirm the earlier findings?
# 5. Do they offer any new insights?
#
setwd(".")
options(error=recover, digits=4)
source(file="MSDS7333-Case12-fx.r")
library(lattice)

seed1 = 12062013
seed2 = 12212013

# generate kids
set.seed(seed1)
births = 1:3
deaths = c(3, 10, 15)
df = genKidsV(births, deaths, parentID=letters[1:3])
df
summary(df)
bwplot(births ~ parentID, data=df, layout=c(1,1), pch="|", main="Birth Distribution by Parent",
       ylab="Births", xlab="Parent")
histogram(~births|parentID, data=df, main="Histogram Plot by Parent", xlab="Births")

# tree
set.seed(seed2)
tree = familyTree(lambda=1, kappa=.5, maxGen=100, maxOffspring=1000)
length(tree) # number of generations
sapply(tree, nrow)
sum(sapply(tree, nrow)) # number of jobs
sapply(tree, function(gen) range(gen$births)) # min, max in birth time for each generation
sapply(tree, function(gen) range(gen$completes)) # min, max in death time for each generation

#
#Austin

mean(sapply(tree, function(gen) mean(gen$births))) # mean birth time for tree
dfb <- sapply(tree, function(gen) mean(gen$births))
dfb

# What does a boxplot of this look like? 
boxplot(dfb) # Yeah buddy.


# Time to look into applying the various lambda and kappas. 
# What do we want? 
# A box plot for each value of kappa and lambda
# loop through kappa, display each value in the title (Is this possible?)
# Then loop through Lambda the same way. We will want to do this for both births *and* deaths

dfb <- sapply(tree, function(gen) mean(gen$births))
dfb
# ^This generates the average birth rate
# lambda = .1 to 3. 
lambda2 <- seq(0.1, 3, by = 0.1)

# kappa = .1 to 5. 
kappa2 <- seq(0.1, 5, by = 0.1)

# Let's first set up a for-loop to get through lambda
for (i in 1:length(lambda)){
  print(lambda[i])
}

# now, we want to set up the value in tree to equal i
for (i in 1:length(lambda)){
  tree = familyTree(lambda = lambda2[i], kappa = .5, maxGen = 100, maxOffspring = 1000)
  dfb <- sapply(tree, function(gen) mean(gen$births))
  boxplot(dfb)
}


# Now that it works, let's focus on the parameters for the boxplot. Let's figure out the main title
lambda2 <- seq(0.1, 3, by = 0.1)
tree = familyTree(lambda = lambda2[1], kappa = .5, maxGen = 100, maxOffspring = 1000)
dfb <- sapply(tree, function(gen) mean(gen$births))
boxplot(dfb, ylim = c(0,15), main = paste("Kappa = .5, Lambda = ", paste(lambda2[1])))

# Boom, shakalaka! Now. Let's pull it all together. 
ylims <- c(0, 20)
lambda2 <- seq(0.1, 3, by = 0.1)
kappa2 <- seq(0.1, 5, by = 0.1)
color <- "cyan2"

# births with fixed kappa
for (i in 1:length(lambda2)){
  tree = familyTree(lambda = lambda2[i], kappa = 1, maxGen = 100, maxOffspring = 1000)
  dfb <- sapply(tree, function(gen) mean(gen$births))
  png(file = paste("./animate1/births",lambda2[i]*10, ".png", sep = ""))
  boxplot(dfb, ylim = ylims, main = paste("Births: Kappa = 1, Lambda = ", paste(lambda2[i])), col = color)
  dev.off()
}

color2 = "green"
# births with fixed lambda
for (i in 1:length(kappa2)){
  tree = familyTree(lambda = 1, kappa = kappa2[i], maxGen = 100, maxOffspring = 1000)
  dfb <- sapply(tree, function(gen) mean(gen$births))
  png(file = paste("./animate2/births",kappa2[i]*10, ".png", sep = ""))
  boxplot(dfb, ylim = ylims, main = paste("Births: Lambda = 1, Kappaa = ", paste(kappa2[i])), col = color2)
  dev.off()
}

# deaths with fixed kappa
for (i in 1:length(lambda2)){
  tree = familyTree(lambda = lambda2[i], kappa = 1, maxGen = 100, maxOffspring = 1000)
  dfd <- sapply(tree, function(gen) mean(gen$completes))
  png(file = paste("./completes1/deaths",lambda2[i]*10, ".png", sep = ""))
  boxplot(dfb, ylim = ylims, main = paste("Births: Kappa = 1, Lambda = ", paste(lambda2[i])), col = color)
  dev.off()
}

color2 = "green"
# deaths with fixed lambda
for (i in 1:length(kappa2)){
  tree = familyTree(lambda = 1, kappa = kappa2[i], maxGen = 100, maxOffspring = 1000)
  dfb <- sapply(tree, function(gen) mean(gen$births))
  png(file = paste("./completes2/births",kappa2[i]*10, ".png", sep = ""))
  boxplot(dfb, ylim = ylims, main = paste("Births: Lambda = 1, Kappaa = ", paste(kappa2[i])), col = color2)
  dev.off()
}

mean(sapply(tree, function(gen) mean(gen$completes))) # mean death time for tree
dfd <- sapply(tree, function(gen) mean(gen$completes))
dfd
boxplot(dfd)

#Austin
#

# tree generation wrapper
set.seed(seed2)
exptOne(1, 0.5, 100, 1000)
set.seed(seed2)
exptOne2(1, 0.5, 100, 1000)

# simulator
set.seed(seed2)
trialKappas = c(0.1, 10, 0.1, 10)
trialLambdas = c(0.1, 0.1, 10, 10)
trialParams = matrix(c(trialLambdas, trialKappas), ncol=2)
MCBA(params=trialParams, repeats=2, mG=100, mO=10000)

##### exploratory simulation ####
set.seed(seed2)
trialKappas = c(0.1, 10, 0.1, 10)
trialLambdas = c(0.1, 0.1, 10, 10)
trialParams = matrix(c(trialLambdas, trialKappas), ncol=2)
# param combinations (0.1, 0.1), (0.1, 10), (10, 0.1), (10, 10)
# repeat this exercise 100 times or simulations for each param combination
# 100,000 max children, 200 max generations
mcTrialOutput = MCBA(params=trialParams, repeats=100, mG=200, mO=100000)
# range of variables for plotting purposes
print("generations range")
sapply(mcTrialOutput, function(combo) range(combo[1,]))
print("jobs range")
sapply(mcTrialOutput, function(combo) range(combo[2,]))
print("births range")
sapply(mcTrialOutput, function(combo) range(combo[3,]))
print("deaths range")
sapply(mcTrialOutput, function(combo) range(combo[4,]))
# save off data
save(mcTrialOutput, file="mcTrialOutput.rda")

#### exploratory results ####
# jump here if you don't want to re-run trial data
load(file="mcTrialOutput.rda")

# generations v jobs plot
par(mfrow=c(2, 2), mai=c(.75, .75, .2, .2), oma=c(0, 0, 1, 0))
mapply(function(oneSet, lambda, kappa) {
  plot(x=oneSet[2, ], y=jitter(oneSet[1,], 1), log="x", ylim=c(1, 20), xlim=c(1, 10^8), pch=19, cex=0.6,
       xlab="jobs", ylab="generations")
  text(x=20, y=15, bquote(paste(lambda==.(lambda))))
  text(x=20, y=13, bquote(paste(kappa==.(kappa))))},
  mcTrialOutput,
  lambda=trialLambdas,
  kappa=trialKappas)
mtext("Jobs Vs. Generations per Lambda/Kappa", outer=TRUE)

# avg birth v avg death
par(mfrow=c(2, 2), mai=c(.75, .75, .2, .2), oma=c(0, 0, 1, 0))
mapply(function(oneSet, lambda, kappa) {
  plot(x=oneSet[3, ], y=jitter(oneSet[4,], 1), ylim=c(1, 200), xlim=c(1, 10^2), pch=19, cex=0.6,
       xlab="births", ylab="deaths")
  text(x=10, y=100, bquote(paste(lambda==.(lambda))))
  text(x=10, y=85, bquote(paste(kappa==.(kappa))))
  },
  mcTrialOutput,
  lambda=trialLambdas,
  kappa=trialKappas)
mtext("Avg Births Vs. Avg Deaths per Lambda/Kappa", outer=TRUE)

###################### TODO #############################

#### actual simulation ####
# adjust lambda and kappa
lambdas = c(seq(0.1, 0.6, by=0.1), seq(0.8, 2, by=0.2), seq(2.25, 3, by=0.25))
kappas = c(lambdas, 3.25, 3.50, 3.75, 4.00, 4.50, 5.00)

paramGrid = as.matrix(expand.grid(lambdas, kappas))
mcGrid = MCBA(params=paramGrid, repeats=400, mG=20, mO=1000)
save(mcGrid, file = "mcGridOutput.rda")

#### actual simulation results ####
# jump here if you don't want to re-run simulation data
load(file="mcGridOutput.rda")

# visualize it
head(mcGrid[[1]][,1]) #list 1, result 1
