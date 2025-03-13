library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

getwd()


# Saving and preperaing data for visualization and Exploration
prepare_data <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data$Date <- as.Date(data$Date)
  
  if(any(is.na(data$Date)) || any(is.na(data$Close))) {
    data <- data[!is.na(data$Date) & !is.na(data$Close), ]
  }
  
  return(data)
}

# Saving var's
KROP_data <- prepare_data("KROP.csv")
PBJ_data <- prepare_data("PBJ.csv")

dim(KROP_data)
head(KROP_data)

length(ncol(KROP_data))

#Plotting Graph
krop_plot <- ggplot(KROP_data, aes(x = Date, y = Close)) +
  geom_line(color = "#1F77B4", linewidth = 1) +
  labs(
    title = "KROP ETF Close Price",
    x = "Date",
    y = "Price (USD)"
  ) +
  theme_minimal()

print(krop_plot)


PBJ_plot <- ggplot(PBJ_data, aes(x = Date, y = Close)) +
  geom_line(color = "#1F77B4", linewidth = 1) +
  labs(
    title = "KROP ETF Close Price",
    x = "Date",
    y = "Price (USD)"
  ) +
  theme_minimal()

print(PBJ_plot)

krop_close <- KROP_data$Close
pbj_close <- PBJ_data$Close


krop_stats <- c(
  Mean   = mean(krop_close),
  Median = median(krop_close),
  StdDev = sd(krop_close),
  Min    = min(krop_close),
  Max    = max(krop_close)
)

pbj_stats <- c(
  Mean   = mean(pbj_close),
  Median = median(pbj_close),
  StdDev = sd(pbj_close),
  Min    = min(pbj_close),
  Max    = max(pbj_close)
)
stats_df <- data.frame(
  Statistic = names(krop_stats),
  KROP      = round(krop_stats, 4),
  PBJ       = round(pbj_stats, 4)
)

print(stats_df)

# Standardize each price series (convert to "z-scores")
krop_z <- scale(krop_close)  # (x - mean) / sd
pbj_z  <- scale(pbj_close)

# Make them plain numeric vectors for convenience
krop_z <- as.numeric(krop_z)
pbj_z  <- as.numeric(pbj_z)

# Histogram of KROP z-scores
hist(
  krop_z,
  main = "KROP Distribution (Standardized)",
  xlab = "Standard Deviations from Mean"
)

# Histogram of PBJ z-scores
hist(
  pbj_z,
  main = "PBJ Distribution (Standardized)",
  xlab = "Standard Deviations from Mean"
)

# Compute a range covering both datasets
all_z <- c(krop_z, pbj_z)

hist(
  krop_z,
  main = "KROP vs. PBJ (Standardized)",
  xlab = "Standard Deviations from Mean",
  xlim = range(all_z),
  breaks = "FD",     # 'FD' or a specific number can help with bin widths
  freq = FALSE,      # or freq=TRUE if you prefer counts
  col = rgb(1, 0, 0, 0.5)  # translucent red (just an example)
)

hist(
  pbj_z,
  breaks = "FD",
  freq = FALSE,
  col = rgb(0, 0, 1, 0.5), # translucent blue (just an example)
  add = TRUE
)

# KROP DISTB PRICE

# Calculate mean and sd
krop_mean <- mean(krop_close)
krop_sd   <- sd(krop_close)

# Standardize (z-scores): (price - mean) / sd
krop_z <- scale(krop_close)  # This is now in "std from mean"

#------------------------------------------------
# 1) Histogram in z-space
#------------------------------------------------
hist(
  krop_z,
  main   = "KROP Distribution",
  xlab   = "Price (dollars $)",
  freq   = FALSE,         # Probability density (or use freq=TRUE for counts)
  breaks = "FD",          # 'FD' gives a decent bin-width guess
  col    = "lightgray",   # or any color you prefer
  axes   = FALSE          # We'll draw a custom axis
)

#------------------------------------------------
# 2) Custom x-axis labeling in "price" terms
#------------------------------------------------
# Choose nice z-values for ticks, e.g. every integer z
z_min  <- floor(min(krop_z))
z_max  <- ceiling(max(krop_z))
z_ticks <- seq(z_min, z_max, by = 1)

# Convert each z-tick back to the price scale: price = mean + z * sd
price_ticks <- krop_mean + z_ticks * krop_sd

# Draw the x-axis:
axis(
  side   = 1,
  at     = z_ticks,             # positions in z-space
  labels = round(price_ticks,2) # show the original price
)

# Draw the y-axis and box
axis(2)
box()

# PBJ DISTB PRIE

# 1) Calculate mean and sd
pbj_mean <- mean(pbj_close)
pbj_sd   <- sd(pbj_close)

# 2) Standardize (z-scores): (price - mean) / sd
pbj_z <- scale(pbj_close)  # This is now in "std from mean"
pbj_z <- as.numeric(pbj_z) # Convert from matrix type if needed

# 3) Histogram in z-space
hist(
  pbj_z,
  main   = "PBJ Distribution",
  xlab   = "Price (Dollars$)",
  freq   = FALSE,     # or freq=TRUE if you prefer counts
  breaks = "FD",      # 'FD' or a specific number for the bin width
  col    = "lightgray",
  axes   = FALSE      # We'll draw a custom axis next
)

# 4) Custom x-axis labeled in original PBJ prices
z_min   <- floor(min(pbj_z))
z_max   <- ceiling(max(pbj_z))
z_ticks <- seq(z_min, z_max, by = 1)

# Convert each z-tick back to the price scale: price = mean + z * sd
price_ticks <- pbj_mean + z_ticks * pbj_sd

# Draw the x-axis with original-price labels
axis(
  side   = 1,
  at     = z_ticks,              # positions in z-space
  labels = round(price_ticks, 2) # show the original price
)

# Draw the y-axis and box
axis(2)
box()

# SCATTER PLOT KROP WITH LINE OF BEST FIT
# Example KROP data (replace with your actual values)

# 1) Create a data frame with time and prices
df_krop <- data.frame(
  Time  = seq_along(krop_close),  # 1, 2, 3, ...
  Price = krop_close
)

# 2) Scatter plot
plot(
  df_krop$Time, 
  df_krop$Price,
  xlab = "Time (Daily)",
  ylab = "KROP Price",
  main = "KROP Price Scatter Plot with Best-Fit Line",
  pch  = 19  # filled circle
)

# 3) Fit a linear model: Price ~ Time
fit_krop <- lm(Price ~ Time, data = df_krop)

# 4) Add the best-fit line
abline(fit_krop, col = "blue", lwd = 2)

# SCATTER PLOT PBJ WITH LINE OF BEST FIT

# Create a data frame with time and prices
df <- data.frame(
  Time  = seq_along(pbj_close),  # 1, 2, 3, ...
  Price = pbj_close
)

# Scatter plot
# Example PBJ data (replace with your actual values)
pbj_close <- c(25, 24, 26, 26, 27, 25)

# Create a data frame with time and prices
df <- data.frame(
  Time  = seq_along(pbj_close),  # 1, 2, 3, ...
  Price = pbj_close
)

# Scatter plot
plot(
  df$Time, 
  df$Price,
  xlab = "Time (Day)",
  ylab = "PBJ Price (Dollars)",
  main = "PBJ Price Scatter Plot with Best-Fit Line",
  pch  = 19  # filled circle
)

# Fit a linear model: Price ~ Time
fit <- lm(Price ~ Time, data = df)

# Add the best-fit line
abline(fit, col = "blue", lwd = 2)


# Convert to a matrix for plotting
stats_matrix <- as.matrix(stats_df[ , c("KROP","PBJ")])
# Give each row a name so we see them on the axis
rownames(stats_matrix) <- stats_df$Statistic

# Create a grouped bar chart
barplot(
  stats_matrix,
  beside = TRUE,          # side-by-side bars instead of stacked
  main   = "KROP vs PBJ: Summary Statistics",
  xlab   = "Statistic",
  ylab   = "Value (dollar)",
  legend = TRUE           # show a legend for KROP & PBJ
)

# Suppose krop_close and pbj_close are your raw numeric vectors

# Compute densities
dens_krop <- density(krop_close)
dens_pbj  <- density(pbj_close)

# Plot KROP's density
plot(
  dens_krop,
  main = "KROP vs PBJ:  Density Estimates",
  xlab = "Price",
  ylab = "Density",
  xlim = range(c(dens_krop$x, dens_pbj$x)), # ensure x-axis covers both
  ylim = range(c(dens_krop$y, dens_pbj$y))
)

# Add PBJ's density
lines(dens_pbj, lty = 2)  # or use a different line type
legend(
  "topright",
  legend = c("KROP", "PBJ"),
  lty    = c(1, 2)
)

#QQ Plot (Distribution vs. Distribution)

qqplot(
  x = krop_close,
  y = pbj_close,
  main = "Q-Q Plot of KROP vs PBJ",
  xlab = "KROP Quantiles",
  ylab = "PBJ Quantiles"
)
abline(a = 0, b = 1, col = "red")  # reference line

#3) Compare Differences or Ratios (New Distribution)
price_diff  <- pbj_close - krop_close
price_ratio <- pbj_close / krop_close
hist(price_diff, main = "Distribution of PBJ - KROP", xlab = "Price Difference (Dollars)")
hist(price_ratio, main = "Distribution of PBJ / KROP", xlab = "Price Ratio")

#4) Rolling (Time-Varying) Distributions
library(dplyr)
library(zoo) # for rollapply if you like

# Combine data into one frame if it's separate
df <- data.frame(
  Date = seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = length(krop_close)),
  KROP = krop_close,
  PBJ  = pbj_close
)

# Example: 10-day rolling standard deviation for KROP/PBJ
roll_length <- 10

df_rolled <- df %>%
  mutate(
    KROP_sd = zoo::rollapply(KROP, width = roll_length, FUN = sd, fill = NA, align = "right"),
    PBJ_sd  = zoo::rollapply(PBJ,  width = roll_length, FUN = sd, fill = NA, align = "right")
  )

# Then plot them over time
plot(df_rolled$Date, df_rolled$KROP_sd, type = "l", 
     main = "Rolling 10-day Standard Deviation",
     xlab = "Date", ylab = "Std Dev")
lines(df_rolled$Date, df_rolled$PBJ_sd, lty = 2)
legend("topright", legend=c("KROP", "PBJ"), lty = c(1,2))

# Violin distriution
library(ggplot2)
library(dplyr)

# Put data in a single tidy frame
df_krop <- data.frame(Symbol = "KROP", Price = krop_close)
df_pbj  <- data.frame(Symbol = "PBJ",  Price = pbj_close)
df_all  <- bind_rows(df_krop, df_pbj)

# Violin plot with boxplot overlay
ggplot(df_all, aes(x = Symbol, y = Price)) +
  geom_violin(trim = FALSE) +    # shape from the entire distribution
  geom_boxplot(width = 0.1) +    # overlay boxplot
  labs(
    title = "Violin Plot of KROP vs PBJ",
    x     = "Symbol",
    y     = "Price"
  )

#Theoretical Porbability distributions
library(fitdistrplus)  # for fitting distributions

# Fit distributions to KROP prices
fit_krop_norm  <- fitdist(krop_close, "norm")
fit_krop_lnorm <- fitdist(krop_close, "lnorm")
fit_krop_gamma <- fitdist(krop_close, "gamma")

# Create a histogram of the empirical data
hist(
  krop_close,
  probability = TRUE,   # Scale to probability density
  main = "KROP Price Distribution ",
  xlab = "KROP Price ($)",
  col = "lightgray",
  border = "black"
)

# Generate fitted density curves
x_vals <- seq(min(krop_close), max(krop_close), length.out = 100)
y_norm  <- dnorm(x_vals, mean = fit_krop_norm$estimate["mean"], sd = fit_krop_norm$estimate["sd"])
y_lnorm <- dlnorm(x_vals, meanlog = fit_krop_lnorm$estimate["meanlog"], sdlog = fit_krop_lnorm$estimate["sdlog"])
y_gamma <- dgamma(x_vals, shape = fit_krop_gamma$estimate["shape"], rate = fit_krop_gamma$estimate["rate"])

# Add fitted density lines to histogram
lines(x_vals, y_norm, col = "red", lwd = 2, lty = 1)    # Normal (solid red)
lines(x_vals, y_lnorm, col = "blue", lwd = 2, lty = 2)   # Lognormal (dashed blue)
lines(x_vals, y_gamma, col = "green", lwd = 2, lty = 3)  # Gamma (dotted green)

# Add legend
legend(
  "topright",
  legend = c("Normal", "Lognormal", "Gamma"),
  col = c("red", "blue", "green"),
  lwd = 2,
  lty = c(1, 2, 3)
)



# PBJ prob distributions
library(fitdistrplus)  # for fitting distributions

# Fit distributions to PBJ prices
fit_pbj_norm  <- fitdist(pbj_close, "norm")
fit_pbj_lnorm <- fitdist(pbj_close, "lnorm")
fit_pbj_gamma <- fitdist(pbj_close, "gamma")

# Create a histogram of the empirical data
hist(
  pbj_close,
  probability = TRUE,   # Scale to probability density
  main = "PBJ Price Distribution's",
  xlab = "PBJ Price ($)",
  col = "lightgray",
  border = "black"
)

# Generate fitted density curves
x_vals <- seq(min(pbj_close), max(pbj_close), length.out = 100)
y_norm  <- dnorm(x_vals, mean = fit_pbj_norm$estimate["mean"], sd = fit_pbj_norm$estimate["sd"])
y_lnorm <- dlnorm(x_vals, meanlog = fit_pbj_lnorm$estimate["meanlog"], sdlog = fit_pbj_lnorm$estimate["sdlog"])
y_gamma <- dgamma(x_vals, shape = fit_pbj_gamma$estimate["shape"], rate = fit_pbj_gamma$estimate["rate"])

# Add fitted density lines to histogram
lines(x_vals, y_norm, col = "red", lwd = 2, lty = 1)    # Normal (solid red)
lines(x_vals, y_lnorm, col = "blue", lwd = 2, lty = 2)   # Lognormal (dashed blue)
lines(x_vals, y_gamma, col = "green", lwd = 2, lty = 3)  # Gamma (dotted green)

# Add legend
legend(
  "topright",
  legend = c("Normal", "Lognormal", "Gamma"),
  col = c("red", "blue", "green"),
  lwd = 2,
  lty = c(1, 2, 3)
)

