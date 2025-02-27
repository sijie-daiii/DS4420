# Discussion
# A normal distribution is unbounded on both sides, which conflicts with the 
# fact that song durations can never be negative and often cluster within a 
# certain range. Furthermore, song durations tend to exhibit some degree of 
# right skew, whereas the normal distribution assumes symmetry around the 
# mean, suggesting the normal model may not be the most suitable for 
# this feature.

df <- read.csv("ds4420_spotify.csv")  
durations <- df$duration_s           

# MLEs for Normal(µ, σ)
mu_hat <- mean(durations)
sigma_hat <- sd(durations)

cat("Estimated mu (seconds):    ", mu_hat, "\n")
cat("Estimated sigma (seconds): ", sigma_hat, "\n")

# Calculate probabilities
# (a) Probability duration > 4 minutes (240 seconds)
p_longer_than_4 <- 1 - pnorm(240, mean = mu_hat, sd = sigma_hat)

# (b) Probability duration is between 2 (120s) and 5 (300s) minutes
p_between_2_5 <- pnorm(300, mean = mu_hat, sd = sigma_hat) - 
  pnorm(120, mean = mu_hat, sd = sigma_hat)

# (c) Probability duration < 1 minute (60 seconds)
p_less_than_1 <- pnorm(60, mean = mu_hat, sd = sigma_hat)

cat("P(X > 4 min)         =", p_longer_than_4, "\n")
cat("P(2 min < X < 5 min) =", p_between_2_5,   "\n")
cat("P(X < 1 min)         =", p_less_than_1,   "\n")
