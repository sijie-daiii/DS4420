# A reasonable distribution to model X, the number of points Dr. Gerber 
# scored in a single game, is the Poisson distribution, which is commonly 
# used to model discrete counts of “rare” events within a fixed period. Points 
# in a basketball game can be viewed as relatively infrequent, independent 
# scoring “events” that occur with some average rate λ per game. Although
# the Poisson assumptions are not exact, this distribution often approximates 
# low-count data reasonably well, making it a natural choice for modeling 
# Dr. Gerber’s point totals.

df <- read.csv("drg_points.csv")
points <- df$points

# Compute MLE
lambda_hat <- mean(points)

# Histogram and overlay the Poisson PMF
hist(points, 
     freq = FALSE,                
     breaks = 0:(max(points) + 1),    
     col = "skyblue", 
     xlab = "Points scored per game", 
     main = "Histogram of Observed Points with Poisson Overlay")

# Poisson PMF values
x_vals <- 0:max(points)
pmf_vals <- dpois(x_vals, lambda_hat)

lines(x_vals, pmf_vals, 
      type = "b",          
      pch = 19,           
      col = "red")

# From the histogram and Poisson overlay, we can see that there is a large 
# spike at 0 points—which the Poisson model with λ≈2.19 underestimates—and 
# there is also a right‐tail outlier around 15 points that the Poisson pmf 
# nearly ignores. In principle, the Poisson assumption (mean = variance) 
# does not quite match these data: the empirical variance is higher than the 
# mean (suggesting overdispersion). Visually, most of the middle part of the 
# distribution is reasonably approximated, but the Poisson curve is too low 
# at 0 points and too low in the right tail. Thus, although the Poisson is 
# not a terrible first attempt, the mismatch at zero and the high outlier 
# indicate that a more flexible model might be a better fit.
