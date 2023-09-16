# Libraries
library(pracma)
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

# Define the rect function
rect <- function(u) {
  ifelse(abs(u) <= 0.5, 1, 0)
}

# Define x(t)
x <- function(t) {
  sapply(t, function(ti) rect((ti - pi) / (2 * pi)))
}

# Define y(t)
y <- function(t) {
  (1/sqrt(2*pi)) * exp(-t^2/2)
}

# Define z(t) for various f0 values
z <- function(t, f0) {
  cos(2 * pi * f0 * t)^2
}

# Continuous Fourier Transform (CFT)
cft <- function(signal, t_values, f) {
  transform_values <- numeric(length(f))
  
  for(j in 1:length(f)) {
    freq <- f[j]
    integrand <- signal(t_values) * exp(-1i * 2 * pi * freq * t_values)
    transform_values[j] <- trapz(t_values, integrand)
    #trapz ?
  }
  
  return(transform_values)
}

# Inverse Continuous Fourier Transform (ICFT)
icft <- function(X_f, f_values, t) {
  signal_values <- numeric(length(t))
  
  for(j in 1:length(t)) {
    time <- t[j]
    integrand <- X_f * exp(1i * 2 * pi * f_values * time)
    signal_values[j] <- trapz(f_values, integrand)
  }
  
  return(Re(signal_values))
}

# Time and frequency values
t_values <- seq(-3*pi, 3*pi, length.out=1000)
f_values <- seq(-3*pi, 3*pi, length.out=1000)

# Compute CFT for the signals
X_f_x <- cft(x, t_values, f_values)
X_f_y <- cft(y, t_values, f_values)
X_f_z1 <- cft(function(t) z(t, 1), t_values, f_values)
X_f_z2 <- cft(function(t) z(t, 2), t_values, f_values)
X_f_z3 <- cft(function(t) z(t, 3), t_values, f_values)

# Retrieve signals using ICFT
x_retrieved <- icft(X_f_x, f_values, t_values)
y_retrieved <- icft(X_f_y, f_values, t_values)
z1_retrieved <- icft(X_f_z1, f_values, t_values)
z2_retrieved <- icft(X_f_z2, f_values, t_values)
z3_retrieved <- icft(X_f_z3, f_values, t_values)

# Plot the original and retrieved signals
plot(t_values, x(t_values), type='l', main='Original x(t)')
plot(t_values, x_retrieved, type='l', main='Retrieved x(t) using ICFT')

plot(t_values, y(t_values), type='l', main='Original y(t)')
plot(t_values, y_retrieved, type='l', main='Retrieved y(t) using ICFT')

plot(t_values, z(t_values, 1), type='l', main='Original z(t) with f0=1')
plot(t_values, z1_retrieved, type='l', main='Retrieved z(t) with f0=1 using ICFT')

plot(t_values, z(t_values, 2), type='l', main='Original z(t) with f0=2')
plot(t_values, z2_retrieved, type='l', main='Retrieved z(t) with f0=2 using ICFT')

plot(t_values, z(t_values, 3), type='l', main='Original z(t) with f0=3')
plot(t_values, z3_retrieved, type='l', main='Retrieved z(t) with f0=3 using ICFT')


# Function to plot amplitude and phase spectra
plot_spectra <- function(f, X_f, title) {
  # Amplitude spectrum
  plot(f, Mod(X_f), type='l', main=paste('Amplitude Spectrum of', title), xlab='Frequency (Hz)', ylab='Amplitude')
  
  # Phase spectrum
  plot(f, Arg(X_f), type='l', main=paste('Phase Spectrum of', title), xlab='Frequency (Hz)', ylab='Phase (Radians)')
}

# Plot amplitude and phase spectra for x(t)
plot_spectra(f_values, X_f_x, 'x(t)')

# Plot amplitude and phase spectra for y(t)
plot_spectra(f_values, X_f_y, 'y(t)')

# Plot amplitude and phase spectra for z(t) with f0=1
plot_spectra(f_values, X_f_z1, 'z(t) with f0=1')

# Plot amplitude and phase spectra for z(t) with f0=2
plot_spectra(f_values, X_f_z2, 'z(t) with f0=2')

# Plot amplitude and phase spectra for z(t) with f0=3
plot_spectra(f_values, X_f_z3, 'z(t) with f0=3')


# Define X(f)
X <- function(f) {
  1 / (1 + f^2)
}

# Compute ICFT to get x(t) from X(f)
x_from_Xf <- icft(sapply(f_values, X), f_values, t_values)

# Plot x(t) retrieved from X(f)
plot(t_values, x_from_Xf, type='l', main='x(t) retrieved from X(f)', xlab='Time (t)', ylab='Amplitude')
