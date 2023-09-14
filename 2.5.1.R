# Libraries
library(ggplot2)

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

t_x <- seq(-3*pi, 3*pi, length.out=1000) # Time values

t <- seq(-1, 1, length.out=1000) # Time values

# Plot x(t)
plot(t_x, x(t_x), type='l', main='x(t)')

# Plot y(t)
plot(t, y(t), type='l', main='y(t)')

# Plot z(t) for various f0 values
colors <- c("red", "blue", "green")
f0_values <- c(1, 2, 3)
for(i in 1:3) {
  plot(t, z(t, f0_values[i]), type='l', col=colors[i], main=paste('z(t) with f0=', f0_values[i]))
}

# DFT function
dft <- function(signal, t, f) {
  dt <- t[2] - t[1]
  X_f <- numeric(length(f))
  
  for(i in 1:length(f)) {
    freq <- f[i]
    X_f[i] <- sum(signal * exp(-1i * 2 * pi * freq * t)) * dt
  }
  
  return(X_f)
}

# Frequency values
f_x <- seq(-3*pi, 3*pi, length.out=1000)

f <- seq(-1 ,1 , length.out=1000)

# Compute DFT for the signals
X_f_x <- dft(x(t_x), t_x, f_x)
X_f_y <- dft(y(t), t, f)
X_f_z1 <- dft(z(t, 1), t, f)
X_f_z2 <- dft(z(t, 2), t, f)
X_f_z3 <- dft(z(t, 3), t, f)

plot_spectrums <- function(f, X_f, title, x_f_for_phase=NULL, f_for_phase=NULL) {
  # Amplitude spectrum
  plot(f, Mod(X_f), type='l', main=paste('Amplitude Spectrum of', title), xlab='Frequency (Hz)', ylab='Amplitude')
  
  # Use x_f_for_phase if it's not NULL, otherwise use a default value (e.g., b)
  value_to_use_for_f <- if (!is.null(f_for_phase)) f_for_phase else f
  # Use x_f_for_phase if it's not NULL, otherwise use a default value (e.g., b)
  value_to_use_for_x_f <- if (!is.null(x_f_for_phase)) x_f_for_phase else X_f

  # Phase spectrum
  plot(value_to_use_for_f, Arg(value_to_use_for_x_f), type='l', main=paste('Phase Spectrum of', title), xlab='Frequency (Hz)', ylab='Phase (Radians)')
}

t_xx <-  seq(-3, 3, length.out=1000)
f_xx <- seq(-3, 3, length.out=1000)

# Plot for x(t)
plot_spectrums(f_x, X_f_x,'x(t_x)', dft(x(t_xx), t_xx, f_xx), f_xx)

# Plot for y(t)
plot_spectrums(f, X_f_y, 'y(t)', dft(y(t_xx), t_xx, f_xx), f_xx)

# Plot for z(t) with f0=1
plot_spectrums(f, X_f_z1, 'z(t) with f0=1')

# Plot for z(t) with f0=2
plot_spectrums(f, X_f_z2, 'z(t) with f0=2')

# Plot for z(t) with f0=3
plot_spectrums(f, X_f_z3, 'z(t) with f0=3')
