set.seed(100)
x <- seq(0,1,0.01)
count.x <- matrix(, nrow = 101, ncol = 101)

for (i in 1:100){

  for (j in 1:101){
    count <- 0
    for (k in 1:101){
      if (x[k] == (j-1)/100){
        count <- count + 1
      }
    }
    count.x[i,j] <- count
  }

  # AB: don't need to initialize vectors
  # phenotype <- vector(length = length(x))

  # AB: don't need to use a for-loop for this; see below
  # for (i in 1:length(x)){
  #   phenotype[i] <- rnorm(1,mean = x[i], sd = 1-x[i])
  # }
  phenotype <- rnorm(length(x), mean = x, sd = 1 - x)

  fitness <- -abs(phenotype)

  # AB: in future, you might want to write a function to do this
  # since we'll want to play around with different ones
  repro <- exp(fitness)/(sum(exp(fitness)))

  dead.person <- (sample(x,1))*100 + 1
  new.person <- sample(x,1, prob = repro)

  x[dead.person] <- new.person
}

for (j in 1:101){
  count <- 0
  for (k in 1:101){
    if (x[k] == j){
      count <- count + 1
    }
  }
  count.x[101,j] <- count
}

repetition <- seq(1,101,1)
plot(repetition,count.x[1,],ylim = c(0,10), type = "l")
for (i in 1:100){
  lines(repetition,count.x[i+1,], col = i)
}

