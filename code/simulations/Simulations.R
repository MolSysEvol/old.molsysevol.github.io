# Created on 22/11/16 by jdutheil
# Modified on 15/12/21 by jdutheil (add selection)
# Simulate populations with genetic drift

create.population <- function(size, state) {
  pop <- rep(state, size)
  attr(pop, "fitness") <- rep(1, size)
  return(pop)
}

pop0 <- create.population(10, "A")
pop0

evolve.population <- function(pop, mutation.rate, relative.fitness) {
  w <- attr(pop, "fitness")
  mean_w <- mean(w)
  w <- w / mean(w)
  pop <- sample(pop, replace = TRUE, prob = w)
  r <- (runif(length(pop), min = 0, max = 1) <= mutation.rate)
  s <- sample(c("A", "C", "G", "T"), size = sum(r), replace = TRUE)
  pop[r] <- s
  w[r] <- mean_w * relative.fitness
  attr(pop, "fitness") <- w
  return(pop)
}

evolve.population(pop0, 0.1)

evolve.population.multi <- function(pop, mutation.rate, relative.fitness, nb.gen) {
  f.a <- numeric(nb.gen + 1)
  f.c <- numeric(nb.gen + 1)
  f.g <- numeric(nb.gen + 1)
  f.t <- numeric(nb.gen + 1)
  pb <- txtProgressBar(0, nb.gen + 1, style = 3)
  for (i in 1:(nb.gen + 1)) {
    setTxtProgressBar(pb, i)
    f.a[i] <- sum(pop == "A")
    f.c[i] <- sum(pop == "C")
    f.g[i] <- sum(pop == "G")
    f.t[i] <- sum(pop == "T")
    pop <- evolve.population(pop, mutation.rate, relative.fitness)
  }
  return(data.frame(fA = f.a, fC = f.c, fG = f.g, fT = f.t))
}

#Test
#sim <- evolve.population.multi(pop0, 0.0001, 100000)

plot.sim <- function(sim) {
  n <- sum(sim[1,])
  plot(sim$fA * 100 / n ~ I(1:nrow(sim)), type = "l", col = "red", ylim = c(0, 100),
       ylab = "Percent", xlab = "Generations")
  lines(sim$fC * 100 / n ~ I(1:nrow(sim)), type = "l", col = "yellow")
  lines(sim$fG * 100 / n ~ I(1:nrow(sim)), type = "l", col = "green")
  lines(sim$fT * 100 / n ~ I(1:nrow(sim)), type = "l", col = "blue")
}

#Test
#plot.sim(sim)



###
### Now with mutation
###

# We start from a homogeneous population

pop1 <- create.population(10000, "A")

sim1 <- evolve.population.multi(pop = pop1, mutation.rate = 0.00001, relative.fitness = 100, nb.gen = 10000)
par(mar=rep(0,4))
plot.sim(sim1)

dev.print(png, "../../images/adaptive_substitutions_banner.png", width = 1152, height = 326)
