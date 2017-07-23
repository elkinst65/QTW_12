genKidsV = function(bTimes, cTimes, parentID, lambda=0.5, kappa=0.3) {
  # Determine how many children each job has
  parentAge = cTimes - bTimes
  numKids = rpois(n=length(parentAge), lambda=lambda * parentAge)

  if (sum(numKids) == 0)
    return(NULL)

  # Determine the birth times of the children
  kidStats =mapply(function(n, min, max) {
    births = sort(runif(n, min, max))
    runtimes = rexp(n, rate=kappa)
    completes = rep(max, n) + runtimes
    data.frame(births, completes)},
    n=numKids, min=bTimes, max=cTimes, SIMPLIFY=FALSE)


  return(data.frame(parentID=rep(parentID, numKids),
                    kidID=1:sum(numKids),
                    births=unlist(lapply(kidStats, "[[", "births")),
                    completes=unlist(lapply(kidStats, "[[", "completes"))))
}

familyTree = function(lambda=0.5, kappa=0.3, maxGen=10, maxOffspring=1000) {
  # Return value - a list with 1 data frame per generation.
  allGens = vector(mode="list", length=maxGen)

  # Generate root of the tree
  allGens[[1]] = data.frame(parentID=NA, kidID=1, births=0, completes=rexp(1, rate=kappa))

  currentNumOffspring = 0

  # Generate future generations, one at a time.
  for (i in 2:maxGen) {
    nextGen = genKidsV(bTimes=allGens[[(i - 1)]]$births,
                       cTimes=allGens[[(i - 1)]]$completes,
                       parentID=allGens[[(i - 1)]]$kidID,
                       lambda=lambda,
                       kappa=kappa)
    if (is.null(nextGen))
      return(allGens[1:(i - 1)])
    allGens[[i]] = nextGen
    currentNumOffspring = currentNumOffspring + nrow(nextGen)
    if (currentNumOffspring > maxOffspring)
      return(allGens[1:i])
  }
  return(allGens)
}

exptOne = function(l, k, mG, mO) {
  # Helper function to call familyTree
  # Returns - summary statistics for analysis,

  aTree = familyTree(lambda=l, kappa=k, maxGen=mG, maxOffspring=mO)
  numGen = length(aTree)
  numJobs = sum(sapply(aTree, nrow))
  return(c(numGen, numJobs))
}

exptOne2 = function(l, k, mG, mO) {
  # Helper function to call familyTree
  # Returns - summary statistics for analysis,

  aTree = familyTree(lambda=l, kappa=k, maxGen=mG, maxOffspring=mO)
  numGen = length(aTree)
  numJobs = sum(sapply(aTree, nrow))
  avgBirth = mean(sapply(aTree, function(gen) mean(gen$births))) # mean birth time for tree
  avgDeath = mean(sapply(aTree, function(gen) mean(gen$completes))) # mean death time for tree
  return(c(numGen, numJobs, avgBirth, avgDeath))
}

MCBA = function(params, repeats=5, mG=10, mO=1000) {
  # params: matrix columns of lambda and kappa values
  # For each lambda and kappa pair, run "repeats" times

  n = nrow(params)
  mcResults = vector("list", length=n)

  for (i in 1:n) {
    cat(paste("param set is", i, ": l =", params[i, 1], ": k = ", params[1, 2]), "\n")
    mcResults[[i]] = replicate(repeats, exptOne2(l=params[i, 1], k=params[i, 2], mG=mG, mO=mO))
  }
  return(mcResults)
}
