# Discussion
# An exponential distribution generally assumes a “memoryless” 
# property (i.e., the probability of continuing does not depend on how long 
# something has lasted), which does not align with how songs are typically 
# structured. In reality, song durations often cluster around three to four
# minutes, have both practical minimum lengths (e.g., intros, refrain) and 
# typical maximum lengths, so an exponential model is unlikely to accurately 
# capture this distribution.

df <- read.csv("ds4420_spotify.csv")  
durations <- df$duration_s           

# MLE of θ
theta_hat <- mean(durations)
cat("Estimated θ (seconds):", theta_hat, "\n")

# Compute probabilities

# (a) Probability the duration is longer than 4 minutes (240 seconds)
p_longer_4min <- 1 - pexp(240, rate = 1/theta_hat)

# (b) Probability the duration is between 2 minutes (120s) and 5 minutes (300s)
p_between_2_5min <- pexp(300, rate = 1/theta_hat) - pexp(120, rate = 1/theta_hat)

# (c) Probability the duration is less than 1 minute (60 seconds)
p_less_1min <- pexp(60, rate = 1/theta_hat)

cat("P(X > 4 min):         ", p_longer_4min, "\n")
cat("P(2 min < X < 5 min): ", p_between_2_5min, "\n")
cat("P(X < 1 min):         ", p_less_1min, "\n")
