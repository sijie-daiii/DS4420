library(mvtnorm)
df <- read.csv("ds4420_spotify.csv")   
X <- na.omit(df[, c("artist_pop", "track_pop")]) 

mu_hat <- colMeans(X)       
Sigma_hat <- cov(X)         

cat("MLE mean vector (mu_hat):\n")
print(mu_hat)
cat("\nMLE covariance matrix (Sigma_hat):\n")
print(Sigma_hat)

x1_vals <- seq(min(X$artist_pop), max(X$artist_pop), length.out=50)
x2_vals <- seq(min(X$track_pop),  max(X$track_pop),  length.out=50)
grid <- expand.grid(x1 = x1_vals, x2 = x2_vals)

# Evaluate MVN pdf
z_vals <- dmvnorm(grid, mean=mu_hat, sigma=Sigma_hat)

# Convert to matrix form for persp()
Z <- matrix(z_vals, nrow=50, ncol=50)

persp(x1_vals, x2_vals, Z,
      theta = 35, phi = 20, 
      col="lightblue", shade=0.5,
      xlab="artist_pop", ylab="track_pop", zlab="Density",
      main="3D MVN Density Surface")

plot(X, pch=16, col=rgb(0,0,1,0.4),
     xlab="artist_pop", ylab="track_pop",
     main="Observed Data with MVN Contours")

contour(x1_vals, x2_vals, Z,
        add=TRUE, col="red")

# (a)
# The covariance matrix has a large positive off‐diagonal entry, indicating 
# a strong positive correlation between artist and track popularity: as one 
# increases, the other tends to do so as well. This means that in the 
# bivariate Gaussian framework, we are dealing with an elliptical (rather 
# than circular) contour shape that slants upward, reflecting the positive 
# linear relationship between these two variables.

# (b)
# The peak of the 3D multivariate Gaussian surface occurs at the estimated 
# mean vector, around (artist_pop ≈ 71.46, track_pop ≈ 61.27). Values far 
# from this central point—particularly those reflecting extremely low or 
# extremely high popularity for either artist or track—correspond to very 
# small densities on the surface, suggesting they are much less likely 
# under this model.

# (c)
# A bivariate Gaussian can capture the general elliptical cloud and positive 
# correlation in the data, but it may not perfectly represent extremes or 
# potential boundaries (e.g., both popularity measures are bounded 
# between 0 and 100). If the scatter appears roughly centralized and 
# “elliptical” without strong skew or multiple clusters, the multivariate 
# Gaussian can be a decent approximation; otherwise, alternative 
# distributions (e.g., bounded or skewed models) might offer a better fit.