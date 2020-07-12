
install.packages("biosignalEMG")
library(biosignalEMG)

emg1 <- emg(x)

 
# @Guerrero:2019 example:

emg2 <- emg(EMG1, samplingrate = 1000, units = "mV", data.name = "Random EMG" )

op <- par(mfrow = c(2,1))
x <- rnorm(1000, 1, 1)
emg1 <- emg(x, samplingrate = 1000, units = "mV", data.name = "TestRun from Guerrero")
plot(emg1, main = "simulated EMG with a DC offset")
abline(h = mean(emg1$values), col = "red", lwd = 2)
# remove dc bias
emg2 <- dcbiasremoval(emg1)
plot(emg2, main = "simulated EMG with overall zero mean")
abline(h = mean(emg2$values), col = "red", lwd = 2)
par(op)



# my attempt with some random data from online

EMG1 <- read.csv("~/Test_runs/data_random/Random_EMG_1.txt")
EMG1 <- EMG1[-c(1, 2, 3), ] # delete the first few rows of string data
EMG1 <- sapply(EMG1, as.numeric)

EMG1 <- emg(EMG1, samplingrate = 1000, units = "mV", data.name = "Random EMG")
EMG1 <- dcbiasremoval(EMG1)
abline(h = mean(EMG1$values), col = "red", lwd = 2)
par(mfrow = c(1,1))
plot(EMG1)

# practice rectification with synthetic EMG data

emgx <- syntheticemg(n.length.out = 5000, on.sd = 1,
                     on.duration.mean = 350, on.duration.sd = 10, off.sd = 0.05,
                     off.duration.mean = 300, off.duration.sd = 20, on.mode.pos = 0.75, 
                     shape.factor = 0.5, samplingrate = 1000, units = "mV",
                     data.name = "Synthetic EMG")
plot(emgx, main = "Synthetic EMG")

par(mfrow = c(2, 1))

emgR1 <- rectification(emgx, rtype = "fullwave")
plot(emgR1, main = "Full-wave rectified EMG")
emgR2 <- rectification(emgx, rtype = "halfwave")
plot(emgR2, main = "Half-wave rectified EMG")


# practive rectification on my random data set

EMGR1 <- rectification(EMG1, rtype = "fullwave")
plot(EMGR1, main = "Full-wave rectified EMG")
EMGR2 <- rectification(EMG1, rtype = "halfwave")
plot(EMGR2, main = "Half-wave rectified EMG")


# Linear envelope on synthetic data:

# Moving Average envelope:

emgMA <- envelope(emgx, method = "MA", wsize = 60)
plot(emgx, main = "Moving-Average Envelope", col = "gray")
plot(emgMA, add = TRUE, lwd = 2)

# Root mean squared envelope:

emgRMS <- envelope(emgx, method = "RMS", wsize = 60)
plot(emgx, main = "RMS Envelope", col = "gray")
plot(emgRMS, add = TRUE, lwd = 2)


# linear enveloped on my random data set

# Moving Average envelope:

EMGma <- envelope(EMGR1, method = "MA", wsize = 100)
plot(EMGR1, main = "Moving-Average Envelope", col = "gray")
plot(EMGma, add = TRUE, lwd = 2)

# Root mean squared envelope:

EMGrms <- envelope(EMGR1, method = "RMS", wsize = 300)
plot(EMGR1, main = "RMS Envelope", col = "gray")
plot(EMGrms, add = TRUE, lwd = 2)

