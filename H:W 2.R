rolls <- 20
sim_rolls <- sample(1:6, rolls, replace = TRUE)
for (indx in 1:49) {
  iterated_sim_rolls <- sample(1:6, rolls, replace = TRUE)
  df <- data.frame(sim_rolls, iterated_sim_rolls)
}
sims <- 50
more_sim_rolls <- sample(1:6, rolls*sims, replace = TRUE) # vector version
mean(sim_rolls)
mean(1:6)
sixes <- as.numeric(sim_rolls == 6)
more_sixes <- as.numeric(more_sim_rolls == 6)
sixes_as_binary <- (sim_rolls == 6)
mean(sixes)
mean(more_sixes)
plot(more_sim_rolls, 1:1000)
