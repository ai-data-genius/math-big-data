#Question 1

# Load the ggplot2 package for plotting
library(ggplot2)

# Define the periodic versions of the functions f, g, and h
f_periodic <- function(t) {
  t_mod <- (t - pi) %% (2 * pi) - pi
  pi - abs(t_mod)
}

g_periodic <- function(t) {
  t_mod <- (t - pi) %% (2 * pi) - pi
  t_mod^2 - pi^2
}

h_periodic <- function(t) {
  t_mod <- (t - pi) %% (2 * pi) - pi  # Adjusting for correct modulo behavior
  ifelse(t_mod >= 0 & t_mod < pi, exp(-t_mod / pi), 0)
}

# Generate a sequence of t values between -5*pi and 5*pi
t_values <- seq(-5 * pi, 5 * pi, length.out = 1000)

# Calculate function values for each t using the periodic versions
f_values <- sapply(t_values, f_periodic)
g_values <- sapply(t_values, g_periodic)
h_values <- sapply(t_values, h_periodic)

# Create data frames for plotting
df_f <- data.frame(t = t_values, y = f_values)
df_g <- data.frame(t = t_values, y = g_values)
df_h <- data.frame(t = t_values, y = h_values)

# Plot f(t)
ggplot(df_f, aes(x = t, y = y)) +
  geom_line() +
  ggtitle(expression(f(t) == pi - "|" * t * "|")) +
  xlab('t') +
  ylab('f(t)')

# Plot g(t)
ggplot(df_g, aes(x = t, y = y)) +
  geom_line() +
  ggtitle(expression(g(t) == t^2 - pi^2)) +
  xlab('t') +
  ylab('g(t)')

# Plot h(t)
ggplot(df_h, aes(x = t, y = y)) +
  geom_line() +
  ggtitle(expression(h(t))) +
  xlab('t') +
  ylab('h(t)')


------
# Question 2

# Define the functions for Fourier coefficients
calcul_a0 <- function(func) {
  integral <- integrate(func, lower = -pi, upper = pi)
  return((1 / pi) * integral$value)
}

calcul_an <- function(func, n) {
  an_func <- function(t) {
    func(t) * cos(n * t)
  }
  integral <- integrate(an_func, lower = -pi, upper = pi)
  return((1 / pi) * integral$value)
}

calcul_bn <- function(func, n) {
  bn_func <- function(t) {
    func(t) * sin(n * t)
  }
  integral <- integrate(bn_func, lower = -pi, upper = pi)
  return((1 / pi) * integral$value)
}

# Manually set the value of n for demonstration (you can change this)
n <- as.integer(readline(prompt = "Enter the value of n: "))

# Calculate Fourier coefficients for f(t)
a0_f <- calcul_a0(f)
an_f <- calcul_an(f, n)
bn_f <- calcul_bn(f, n)
cn_f <- sqrt(an_f^2 + bn_f^2)

# Calculate Fourier coefficients for g(t)
a0_g <- calcul_a0(g)
an_g <- calcul_an(g, n)
bn_g <- calcul_bn(g, n)
cn_g <- sqrt(an_g^2 + bn_g^2)

# Calculate Fourier coefficients for h(t)
a0_h <- calcul_a0(h)
an_h <- calcul_an(h, n)
bn_h <- calcul_bn(h, n)
cn_h <- sqrt(an_h^2 + bn_h^2)

# Print the results
cat("For f(t):\n")
cat("a0 = ", a0_f, "\nan = ", an_f, "\nbn = ", bn_f, "\ncn = ", cn_f, "\n\n")

cat("For g(t):\n")
cat("a0 = ", a0_g, "\nan = ", an_g, "\nbn = ", bn_g, "\ncn = ", cn_g, "\n\n")

cat("For h(t):\n")
cat("a0 = ", a0_h, "\nan = ", an_h, "\nbn = ", bn_h, "\ncn = ", cn_h, "\n")

# ------------------
# Q3
# Required libraries
library(ggplot2)

# Define the integrands for the coefficients
integrand_a0 <- function(t, func) {
  func(t)
}

integrand_an <- function(t, func, n) {
  func(t) * cos(n * t)
}

integrand_bn <- function(t, func, n) {
  func(t) * sin(n * t)
}

# Define the truncated real and complex series sums
real_series_sum <- function(t_values, func, N) {
  a0 <- (1 / (2 * pi)) * integrate(integrand_a0, -pi, pi, func = func)$value
  sum_values <- rep(a0, length(t_values))
  for (n in 1:N) {
    an <- (1 / pi) * integrate(integrand_an, -pi, pi, func = func, n = n)$value
    bn <- (1 / pi) * integrate(integrand_bn, -pi, pi, func = func, n = n)$value
    sum_values <- sum_values + an * cos(n * t_values) + bn * sin(n * t_values)
  }
  return(sum_values)
}

complex_series_sum <- function(t_values, func, N) {
  a0 <- (1 / (2 * pi)) * integrate(integrand_a0, -pi, pi, func = func)$value
  sum_values <- rep(a0, length(t_values))
  for (n in 1:N) {
    an <- (1 / pi) * integrate(integrand_an, -pi, pi, func = func, n = n)$value
    bn <- (1 / pi) * integrate(integrand_bn, -pi, pi, func = func, n = n)$value
    cn <- sqrt(an^2 + bn^2)
    phase_angle <- -atan2(bn, an)
    sum_values <- sum_values + cn * cos(n * t_values + phase_angle)
  }
  return(sum_values)
}

# Values of t and N
t_values <- seq(-5 * pi, 5 * pi, length.out = 1000)
N_values <- c(2, 3, 8, 20)

# Calculate the series sums for f(t) for each N considering its periodicity
data_list <- list()
data_list_g <- list()
data_list_h <- list()


for (N in N_values) {
  df <- data.frame(
    t = t_values,
    f = sapply(t_values, f_periodic),
    real_sum = real_series_sum(t_values, f_periodic, N),
    complex_sum = complex_series_sum(t_values, f_periodic, N)
  )
  df$N <- N
  data_list[[as.character(N)]] <- df
}
data_combined <- do.call(rbind, data_list)


for (N in N_values) {
  # For g(t)
  df_g <- data.frame(
    t = t_values,
    g = sapply(t_values, g_periodic),
    real_sum = real_series_sum(t_values, g_periodic, N),
    complex_sum = complex_series_sum(t_values, g_periodic, N)
  )
  df_g$N <- N
  data_list_g[[as.character(N)]] <- df_g
  
  # For h(t)
  df_h <- data.frame(
    t = t_values,
    h = sapply(t_values, h_periodic),
    real_sum = real_series_sum(t_values, h_periodic, N),
    complex_sum = complex_series_sum(t_values, h_periodic, N)
  )
  df_h$N <- N
  data_list_h[[as.character(N)]] <- df_h
}

data_combined_g <- do.call(rbind, data_list_g)
data_combined_h <- do.call(rbind, data_list_h)


# Plot with thinner lines
ggplot(data_combined, aes(x = t)) + 
  geom_line(aes(y = f), color = 'black', size = 0.6) +
  geom_line(aes(y = real_sum), color = 'blue', linetype = 'dashed', size = 0.6) +
  geom_line(aes(y = complex_sum), color = 'red', linetype = 'dotted', size = 0.6) +
  facet_wrap(~N, scales = 'free', ncol = 2) + 
  labs(title = "Function f(t) and its Truncated Series Sums",
       y = "Function Value") + 
  theme_minimal()

# Plot for g(t)
ggplot(data_combined_g, aes(x = t)) + 
  geom_line(aes(y = g), color = 'black', size = 0.6) +
  geom_line(aes(y = real_sum), color = 'blue', linetype = 'dashed', size = 0.6) +
  geom_line(aes(y = complex_sum), color = 'red', linetype = 'dotted', size = 0.6) +
  facet_wrap(~N, scales = 'free', ncol = 2) + 
  labs(title = "Function g(t) and its Truncated Series Sums",
       y = "Function Value") + 
  theme_minimal()

# Plot for h(t)
ggplot(data_combined_h, aes(x = t)) + 
  geom_line(aes(y = h), color = 'black', size = 0.6) +
  geom_line(aes(y = real_sum), color = 'blue', linetype = 'dashed', size = 0.6) +
  geom_line(aes(y = complex_sum), color = 'red', linetype = 'dotted', size = 0.6) +
  facet_wrap(~N, scales = 'free', ncol = 2) + 
  labs(title = "Function h(t) and its Truncated Series Sums",
       y = "Function Value") + 
  theme_minimal()

# -------------------------------
#Question 4

# Calculate the Fourier coefficients for n = 1 to 7
N <- 7
a_values <- numeric(N)
b_values <- numeric(N)
t_values <- seq(-pi, pi, length.out = 1000)

plot_fourier_spectra <- function(func_periodic, func_name) {
  # Calculate a0
  a0 <- (1 / (2 * pi)) * integrate(function(t) integrand_a0(t, func_periodic), -pi, pi)$value
  
  # Calculate an and bn
  a_values <- numeric(N)
  b_values <- numeric(N)
  for (n in 1:N) {
    a_values[n] <- (1 / pi) * integrate(function(t) integrand_an(t, func_periodic, n), -pi, pi)$value
    b_values[n] <- (1 / pi) * integrate(function(t) integrand_bn(t, func_periodic, n), -pi, pi)$value
  }
  
  # Calculate the amplitude and phase
  amplitude <- sqrt(a_values^2 + b_values^2)
  phase <- -atan2(b_values, a_values)
  
  # Plot the amplitude spectrum for f(t) - Real Series
  plot_title = paste("Amplitude Spectrum for", func_name, "- Real Series")
  df_amplitude <- data.frame(n = 1:N, amplitude = amplitude)
  print(ggplot(df_amplitude, aes(x = n, y = amplitude)) +
    geom_bar(stat = "identity", fill = 'blue') +
    labs(title = plot_title, x = "Harmonic n", y = "Amplitude") +
    theme_minimal() +
    ylim(0, max(amplitude) + 0.5))
  
  # Plot the phase spectrum for f(t) - Real Series
  plot_title = paste("Phase Spectrum for ",func_name, " - Real Series")
  df_phase <- data.frame(n = 1:N, phase = phase)
  print(ggplot(df_phase, aes(x = n, y = phase)) +
    geom_bar(stat = "identity", fill = 'red') +
    labs(title = plot_title, x = "Harmonic n", y = "Phase (in radians)") +
    theme_minimal())
  
  # Calculate the complex Fourier coefficients
  cn_values <- complex(real = rep(0, length(n_values_complex)), imaginary = rep(0, length(n_values_complex)))
  for (idx in 1:length(n_values_complex)) {
    n <- n_values_complex[idx]
    cn_values[idx] <- (1 / (2 * pi)) * integrate(function(t) Re(integrand_cn(t, func_periodic, n)), -pi, pi)$value +
      1i * (1 / (2 * pi)) * integrate(function(t) Im(integrand_cn(t, func_periodic, n)), -pi, pi)$value
  }
  
  # Extract amplitude from complex coefficients
  amplitude_complex <- Mod(cn_values)
  
  # Plot the amplitude spectrum for f(t) - Complex Series
  plot_title = paste("Amplitude Spectrum for ",func_name, " - Complex Series")
  df_complex <- data.frame(n = n_values_complex, amplitude = amplitude_complex)
  print(ggplot(df_complex, aes(x = n, y = amplitude)) +
    geom_bar(stat = "identity", fill = 'purple') +
    labs(title = plot_title, x = "Harmonic n", y = "Amplitude") +
    theme_minimal())
  
}

# Call the function for each periodic function
plot_fourier_spectra(f_periodic, "f(t)")
plot_fourier_spectra(g_periodic, "g(t)")
plot_fourier_spectra(h_periodic, "h(t)")