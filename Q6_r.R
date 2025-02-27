df <- read.csv("ds4420_spotify.csv") 
durations <- df$duration_s           

# Fit Normal(µ, σ)
mu_hat <- mean(durations)
sigma_hat <- sd(durations)

# Fit Exponential(θ) by MLE
theta_hat <- mean(durations)

cat("Normal MLE Estimates: mu =", mu_hat, ", sigma =", sigma_hat, "\n")
cat("Exponential MLE Estimate: theta =", theta_hat, "\n")

# Create histogram and overlay both distributions
hist(durations,
     freq = FALSE,                    
     breaks = "FD",                    
     col = "skyblue",
     border = "black",
     main = "Histogram with Normal and Exponential Overlays",
     xlab = "Song Duration (seconds)")

# Create a grid of x-values for plotting the pdf curves
x_vals <- seq(0, max(durations), length.out = 300)

# Normal pdf
pdf_norm <- dnorm(x_vals, mean = mu_hat, sd = sigma_hat)
lines(x_vals, pdf_norm, col = "red", lwd = 2)

# Exponential pdf
pdf_exp <- dexp(x_vals, rate = 1/theta_hat)
lines(x_vals, pdf_exp, col = "green", lwd = 2)

legend("topright",
       legend = c("Normal PDF", "Exponential PDF"),
       col = c("red", "green"),
       lwd = 2)
# Discussion
# Visual Fit: Neither the normal nor the exponential curve perfectly matches 
# the histogram. The normal at least peaks near ~200 seconds, while the 
# exponential over‐predicts very short songs.     

# Intuition (from parts 4(a) & 5(a)): This confirms our earlier reasoning 
# that neither distribution is ideal—exponential ignores typical song 
# structure, and normal fails to account for the positive skew and 
# zero‐lower‐bound.      

# Probability Comparison: The distributions give notably different values. 
# For example, P(X > 4) is about 0.36 (exponential) vs. 0.47 (normal), 
# and P(X < 1 ) is about 0.23 vs. 0.02, illustrating how each model 
# handles the tails differently.  

# Suitability: Neither model is very effective for song durations; a skewed, 
# positive distribution (like lognormal or gamma) typically fits better.
