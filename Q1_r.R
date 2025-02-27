df <- read.csv("ds4420_spotify.csv")

# Compute the MLE for p
p_hat <- mean(df$explicit)

# Probability that Y >= 10 for n = 50
p_at_least_10 <- 1 - pbinom(9, size = 50, prob = p_hat)

cat("MLE for p:", p_hat, "\n")
cat("Probability that at least 10 out of 50 songs are explicit:", 
    p_at_least_10, "\n")

# Both X (whether a song is explicit) and Y (the number of explicit songs 
# in a playlist of size n) naturally fit Bernoulli and Binomial distributions,
# respectively. Since X can only be 0 (not explicit) or 1 (explicit), 
# it follows a Bernoulli distribution with parameter p (the probability that 
# a given song is explicit). Meanwhile, Y is the sum of n such independent 
# Bernoulli trials, so it follows a Binomial( n, p) distribution, representing 
# the count of explicit songs among the n total songs.