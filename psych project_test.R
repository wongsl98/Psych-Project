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
  phenotype <- vector(length = length(x))
  
  for (i in 1:length(x)){
    phenotype[i] <- rnorm(1,mean = x[i], sd = 1-x[i])
  }
  
  fitness <- -abs(phenotype)
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

