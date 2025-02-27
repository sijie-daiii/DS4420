df <- read.csv("ds4420_spotify.csv")

# Calculate the MLEs for each p_i
counts <- table(df$release_month)
total_songs <- nrow(df)
p_hat <- numeric(12)
for (i in 1:12) {
  p_hat[i] <- ifelse(i %in% names(counts), 
                     counts[as.character(i)] / total_songs, 0)
}

# Probability of exactly one song per month in n=12
prob_each_month_once <- factorial(12) * prod(p_hat)

cat("MLE for p_i:", p_hat, "\n")
cat("Probability that each month is represented exactly once:", 
    prob_each_month_once, "\n")

# One reason to examine the release month of songs is to detect seasonal 
# trends or patterns that might influence a song’s popularity. For instance, 
# if certain months tend to generate higher listener engagement or coincide 
# with specific events (e.g., summer break or holiday releases), artists 
# and record labels can use this information to optimize their release 
# schedules and promotional efforts.