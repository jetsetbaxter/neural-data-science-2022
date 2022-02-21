library(tidyverse)

set.seed(123)

Fs <- 1000           # Sampling frequency (1kHz)                          
L <- 5000            # Length of signal (5sec)
t <- (0:(L-1))*(1/Fs)  # Time vector (in sec)

sinwaves1 <- sin(2*pi*2*t)
sinwaves2 <- .8*sin(2*pi*8*t)
sinwaves3 <- .6*sin(2*pi*20*t)
sinwaves4 <- .4*sin(2*pi*50*t)
sinwaves5 <- .2*sin(2*pi*120*t)
# pi is already defined in R!

noise <- 2*rnorm(length(t), 0, 1)
# "randn" function in matlab returns a value randomly drawn from standard normal
# which is a normal with mean 0 variance 1

cbind(t, sinwaves1, sinwaves2, sinwaves3, sinwaves4, sinwaves5, noise) %>%
  # cbind sticks the fake data columns next to each other
  as.data.frame() %>% 
  # transform into a data frame 
  pivot_longer(!t, names_to = "dataset", values_to = "value") %>%
  # pivot longer everything but t ("!t") so you have values in "long" format
  mutate(dataset = factor(dataset, levels = c("sinwaves1", "sinwaves2", "sinwaves3",
                                              "sinwaves4", "sinwaves5", "noise"))) %>%
  # just changes dataset into a factor so we can specify order of plots
  ggplot(aes(x = t, y = value)) + geom_line() + 
  facet_wrap(~ dataset, nrow = 2, ncol = 3)
  # a lot of trouble just to be able to put all the plots together like this!

cbind(t, sinwaves1, sinwaves2, sinwaves3, sinwaves4, sinwaves5, noise) %>%
  as.data.frame() %>% pivot_longer(!t, names_to = "dataset", values_to = "value") %>%
  mutate(dataset = factor(dataset, levels = c("sinwaves1", "sinwaves2", "sinwaves3",
                                              "sinwaves4", "sinwaves5", "noise"))) %>%
  ggplot(aes(x = t, y = value)) + geom_line() + xlim(1, 1.5) +
  facet_wrap(~ dataset, nrow = 2, ncol = 3) # just see plots between 1 and 1.5

my_signal <- sinwaves1 + sinwaves2 + sinwaves3 + sinwaves4 + sinwaves5 + noise
# could have stuck these in a matrix
# or a dataframe and done rowSums
# but this works too!

cbind(t, my_signal) %>% as.data.frame() %>%
  ggplot(aes(x = t, y = signal)) + geom_line() + ggtitle("Fake signal")

# bandpass filter
# install.packages("signal")

order <- round(Fs)
Nyquist <- floor(Fs/2)
band <- c(1, 3)

Myfilt <- signal::fir1(order, band/Nyquist, type = "pass")

filt_data <- signal::filtfilt(Myfilt, my_signal)

cbind(t, filt_data) %>% as.data.frame() %>%
  ggplot(aes(x = t, y = filt_data)) + geom_line() + ggtitle("Fake signal filted 1-3 Hz")

cbind(t, filt_data, sinwaves1) %>% as.data.frame() %>%
  pivot_longer(!t, values_to = "value", names_to = "signal") %>%
  ggplot(aes(x = t, y = value)) + geom_line(aes(color = signal)) +
  scale_color_manual(values = c("black", "red"), labels = c("filtered data", "original signal")) +
  xlab("ms") + ylab("")

# filter with 2 sine waves in it

band <- c(1, 9)

Myfilt <- signal::fir1(order, band/Nyquist, type = "pass")

filt_data <- signal::filtfilt(Myfilt, my_signal)

cbind(t, filt_data) %>% as.data.frame() %>%
  ggplot(aes(x = t, y = filt_data)) + geom_line() + ggtitle("Fake signal filted 1-9 Hz")

cbind(t, filt_data, sinwaves1, sinwaves2) %>% as.data.frame() %>%
  pivot_longer(!t, values_to = "value", names_to = "signal") %>%
  ggplot(aes(x = t, y = value)) + geom_line(aes(color = signal)) +
  scale_color_manual(values = c("black", "red", "deepskyblue1"), 
                     labels = c("filtered data", "2 Hz signal", "8 Hz signal")) +
  xlab("ms") + ylab("")

# compute FFT on signal

Y <- fft(my_signal)
PSD <- abs(Y/L)
plot(t, PSD) 
# just quickly, you can see the mirrored halves of the fft 
# and the points at 2, 8, 20, 50, 120!
PSD <- PSD[1:((L/2)+1)]
PSD[2:(L/2)] <- 2*PSD[2:(L/2)] # fix mirror image: save 2501 elements and double elements 2 to 2500
f <- Fs*(0:(L/2))/L

data.frame(frequency = f, power = PSD) %>% ggplot(aes(x = frequency, y = power)) +
  geom_line() + xlim(0,150) + xlab("Frequency (Hz)") + ylab("Power")

# real data

LFP <- read.delim("LFP2.txt", header = FALSE)
events <- read.delim("events2.txt", header = FALSE)

trials <- matrix(0, nrow = length(events$V1), ncol = 5000)

for (k in 1:length(events$V1)) {
  trials[k,] <- LFP$V1[(events$V1[k]-1999):(events$V1[k]+3000)]
}

trials %>% as.data.frame() %>% pivot_longer(V1:V5000, names_to = "time_point", values_to = "LFP") %>%
  mutate(time_point = parse_number(time_point)-2000) %>%
  ggplot(aes(x = time_point, y = LFP)) +
  stat_summary(fun = mean, geom = "line") + xlab("Time from event (ms)") + ylab("") +
  ggtitle("Event related potential")
# basically same code from 0302 demo!

# we can use same Fs, L, t defined above to compute fft across trials

Fs <- 1000           # Sampling frequency (1kHz)                          
L <- 5000            # Length of signal (5sec)
t <- (0:(L-1))*(1/Fs)  # Time vector (in sec)

Y <- fft(trials) 
dim(Y) # produces an FFT for each trial
# use matrix multiplication to add them up
# you have 450 rows and 5000 columns, so one row per trial
# you want to add down those 450 rows
# easiest way probably to "premultiply" by a row vector of 1's
# 1 x 450 matrix multiplies 450 x 5000 matrix -> 1 x 5000 matrix 
Y <- t(matrix(rep(1,nrow(Y)))) %*% Y
dim(Y)
Y <- as.vector(Y)
PSD <- abs(Y/L)
PSD <- PSD[1:((L/2)+1)]
PSD[2:(L/2)] <- 2*PSD[2:(L/2)] # fix mirrored halves
PSD <- PSD/nrow(trials) # not clear this is necessary - but need to scale?
f <- Fs*(0:(L/2))/L
# just copied from above

data.frame(frequency = f, power = PSD) %>% ggplot(aes(x = frequency, y = power)) +
  geom_line() + xlim(0,200) + xlab("Frequency (Hz)") + ylab("Power (AU)") + ggtitle("LFP2 PSD")
# see that spike at 60

# for reasons that are not entirely clear, running an FFT on the average LFP generates
# output that differs from the matlab output

Y <- fft(colMeans(trials)) # compute column means of trial matrix
PSD <- abs(Y/L)
PSD <- PSD[1:((L/2)+1)]
PSD[2:(L/2)] <- 2*PSD[2:(L/2)] # fix mirrored halves
f <- Fs*(0:(L/2))/L

data.frame(frequency = f, power = PSD) %>% ggplot(aes(x = frequency, y = power)) +
  geom_line() + xlim(0,200) + xlab("Frequency (Hz)") + ylab("Power (AU)") + ggtitle("LFP2 PSD")

# apply notch filter to remove line noise and harmonics
# sequentially filter ~180, 120, 60 Hz (1 Hz either side)

order <- round(Fs)
Nyquist <- floor(Fs/2)

notch1 <- signal::fir1(order, w = c(179, 181)/Nyquist, type = "stop")
notch2 <- signal::fir1(order, w = c(119, 121)/Nyquist, type = "stop")
notch3 <- signal::fir1(order, w = c(59, 61)/Nyquist, type = "stop")

LFP_notch <- signal::filtfilt(notch1, LFP$V1) %>%
  signal::filtfilt(notch2, .) %>%
  signal::filtfilt(notch3, .)

trials_LFP_notch <- matrix(0, nrow = length(events$V1), ncol = 5000)

for (k in 1:length(events$V1)) {
  trials_LFP_notch[k,] <- LFP_notch[(events$V1[k]-1999):(events$V1[k]+3000)]
}

trials_LFP_notch %>% as.data.frame() %>% pivot_longer(V1:V5000, names_to = "time_point", values_to = "LFP") %>%
  mutate(time_point = parse_number(time_point)-2000) %>%
  ggplot(aes(x = time_point, y = LFP)) +
  stat_summary(fun = mean, geom = "line") + xlab("Time from event (ms)") + ylab("") +
  ggtitle("Event related potential (notch-filtered data)")
# looks substantially the same so we didn't screw up the signal

# try FFT again

Y <- fft(trials_LFP_notch) 
Y <- t(matrix(rep(1,nrow(Y)))) %*% Y
Y <- as.vector(Y)
PSD <- abs(Y/L)
PSD <- PSD[1:((L/2)+1)]
PSD[2:(L/2)] <- 2*PSD[2:(L/2)] # fix mirrored halves
PSD <- PSD/nrow(trials) # not clear this is necessary - but need to scale?
f <- Fs*(0:(L/2))/L

data.frame(frequency = f, power = PSD) %>% ggplot(aes(x = frequency, y = power)) +
  geom_line() + xlim(0,200) + xlab("Frequency (Hz)") + ylab("Power (AU)") + 
  ggtitle("LFP2 PSD (notch filtered)")
# spikes at noise frequencies mostly gone

# band passed signals
# Theta: 4-8 Hz
# Alpha: 8-12 Hz
# Beta: 13-30 Hz

order <- round(Fs)
Nyquist <- floor(Fs/2)

theta_filt <- signal::fir1(order, c(4, 8)/Nyquist, type = "pass")
alpha_filt <- signal::fir1(order, c(8, 12)/Nyquist, type = "pass")
beta_filt <- signal::fir1(order, c(13, 30)/Nyquist, type = "pass")

lfp_theta <- signal::filtfilt(theta_filt, LFP_notch)
lfp_alpha <- signal::filtfilt(alpha_filt, LFP_notch)
lfp_beta <- signal::filtfilt(beta_filt, LFP_notch)

# this will get annoying so let's do this
generate_average_ERP <- function(your_LFP) {
  trials_loop <- matrix(0, nrow = length(events$V1), ncol = 5000)
  for(k in 1:length(events$V1)) {
    trials_loop[k,] <- your_LFP[(events$V1[k]-1999):(events$V1[k]+3000)]
  }
  colMeans(trials_loop)
}

band_passed_ERP <- 
  data.frame(t = -1999:3000,
             original = generate_average_ERP(LFP_notch),
             theta = generate_average_ERP(lfp_theta),
             beta = generate_average_ERP(lfp_beta),
             alpha = generate_average_ERP(lfp_alpha)) %>%
  pivot_longer(!t, values_to = "ERP", names_to = "signal")

band_passed_ERP %>%
  mutate(signal = factor(signal, levels = c("original", "theta", "alpha", "beta"))) %>%
  ggplot(aes(x = t, y = ERP)) + geom_line() + ggtitle("Original and band-passed ERP") +
  xlab("time from event (ms)") + ylab("") +
  facet_wrap(~ signal, scales = "free")

# Hilbert function in seewave package
# install.packages("seewave")

theta_hilbert <- seewave::hilbert(generate_average_ERP(lfp_theta), f = Fs)
alpha_hilbert <- seewave::hilbert(generate_average_ERP(lfp_alpha), f = Fs)
beta_hilbert <- seewave::hilbert(generate_average_ERP(lfp_beta), f = Fs)

data.frame(t = -1999:3000,
           theta = abs(theta_hilbert),
           alpha = abs(alpha_hilbert),
           beta = abs(beta_hilbert)) %>%
  pivot_longer(!t, values_to = "amplitude", names_to = "signal") %>%
  mutate(signal = factor(signal, levels = c("theta", "alpha", "beta"))) %>%
  ggplot(aes(x = t, y = amplitude)) + geom_line() + ggtitle("Amplitude") +
  xlab("time from event (ms)") + ylab("") +
  facet_wrap(~ signal, scales = "free")

ERP_hilb <- 
  data.frame(t = -1999:3000,
           theta = abs(theta_hilbert),
           alpha = abs(alpha_hilbert),
           beta = abs(beta_hilbert)) %>%
  pivot_longer(!t, values_to = "ERP", names_to = "signal") %>%
  mutate(type = "amplitude")

band_passed_ERP %>% 
  mutate(type = "signal") %>%
  bind_rows(ERP_hilb) %>%
  mutate(signal = factor(signal, levels = c("original", "theta", "alpha", "beta"))) %>%
  mutate(type = factor(type, levels = c("signal", "amplitude"))) %>%
  ggplot(aes(x = t, y = ERP)) + geom_line(aes(color = type)) + 
  ggtitle("Original signals + Hilbert amplitude") +
  xlab("time from event (ms)") + ylab("") +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~ signal, scales = "free")

data.frame(t = -1999:3000,
           theta = Arg(theta_hilbert),
           alpha = Arg(alpha_hilbert),
           beta = Arg(beta_hilbert)) %>%
  pivot_longer(!t, values_to = "phase", names_to = "signal") %>%
  mutate(signal = factor(signal, levels = c("theta", "alpha", "beta"))) %>%
  ggplot(aes(x = t, y = phase)) + geom_line() + ggtitle("Phase") +
  xlab("time from event (ms)") + ylab("") +
  facet_wrap(~ signal, scales = "free")


  
  
