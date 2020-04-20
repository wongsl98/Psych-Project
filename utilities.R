library(tidyverse)

# Implements Moran or inverted Moran process with selection strength 's'
moran <- function(x, s = 1, invert = FALSE) {
  if (invert) {
    x <- -x - max(-x)
    return((1 - exp(s * x) / sum(exp(s * x))) / (length(x) - 1))
  } else {
    x <- x - max(x)
    return(exp(s * x) / sum(exp(s * x)))
  }
}

# Fitness function using draws from normal distribution
getFitness <- function(pop) -abs(rnorm(length(pop), pop, 1 - pop))

# Impelments selection with mutation rate 'm'
#   strat.round: number of digits to which to round genotype
evolve <- function(nAgents, nGens, s = 1, m = .01, strat.round = 1, invert = FALSE) {

  # vectors to store average & modal strategy each generation
  avg_strat <- rep(NA, nGens)
  modal_strat <- rep(NA, nGens)

  # begin with random population of values in [0, 1]
  pop <- round(runif(nAgents), strat.round)

  for (gen in 1:nGens) {
    deathInd <- sample.int(nAgents, 1) # pick a random agent to die

    if (runif(1) < m) { # mutation happens
      pop[deathInd] <- round(runif(1), strat.round)
    } else {            # selection happens
      reproduceInd <- sample(
        nAgents, 1,
        prob = getFitness(pop) %>% moran(s, invert)
      ) # agent to replace dead agent
      pop[deathInd] <- pop[reproduceInd]
    }

    avg_strat[gen] <- mean(pop) # store average strat this generation
    modal_strat[gen] <- as.numeric(names(sort(table(pop), TRUE))[1])
  }

  return(tibble(gen = 1:nGens, average = avg_strat, mode = modal_strat))
}





