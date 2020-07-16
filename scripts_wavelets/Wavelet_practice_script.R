####sunspots with wavethresh####

data("sunspots")
install.packages("wavethresh")
library(wavethresh)
y <- c(3,6,4,3,5,1,3,5)
ywd <- wd(y, filter.number = 1, family = "DaubExPhase")
summary(ywd)
## Levels:  3 
## Length of original:  8 
## Filter was:  Haar wavelet 
## Boundary handling:  periodic 
## Transform type:  wavelet 
## Date:  Fri May  5 01:20:43 2017
plot(ywd)


####wavScalogram with Random_EMG_1####

library(wavScalogram)
library(biosignalEMG)

EMG1 <- read.table("data_random/Random_EMG_1.txt")
EMG1 <- EMG1[-c(1, 2, 3), ] # delete the first few rows of string data
EMG1 <- sapply(EMG1, as.numeric)
EMG1 <- EMG1[-c(1000:100000)]
str(EMG1)

EMG1 <- emg(EMG1, samplingrate = 1000, units = "mV", 
            data.name = "Random EMG" )

str(EMG1)
plot(EMG1)

EMG1 <- dcbiasremoval(EMG1)
EMG1 <- rectification(EMG1, rtype = "fullwave")

EMG1_eveloped <- envelope(EMG1, method = "RMS", wsize = 60)

wavelet_data <- EMG1$values

plot(wavelet_data)

cwt <- cwt_wst(signal = wavelet_data, dt = 1, energy_density = TRUE)




####wavScalogram Wachowiak data####


EMG2 <- read.csv("data_wachowiak/S01_bs.csv")

EMG2_gastroc <- EMG2$X1.22E.06

EMG2_gastroc <- diff(EMG2_gastroc)

EMG2_gastroc <- c(EMG2_gastroc*10000)

EMG2_gastroc <- EMG2_gastroc[-c(1:10000), ]

EMG2_gastroc <- data.frame(EMG2_gastroc)

EMG2_gastroc <- emg(EMG2_gastroc, samplingrate = 1000, units = "mV", 
            data.name = "Wachowiak EMG" )

plot(EMG2_gastroc)

EMG2_gastroc <- dcbiasremoval(EMG2_gastroc)
EMG2_gastroc <- rectification(EMG2_gastroc, rtype = "fullwave")

plot(EMG2_gastroc)

wavelet_gastroc <- EMG2_gastroc$values

wavelet_gastroc <- sapply(wavelet_gastroc, as.numeric)
wavelet_gastroc <- c(wavelet_gastroc/1000)
wavelet_gastroc_rounded <- round(wavelet_gastroc, 5)

head(wavelet_gastroc)

cwt <- cwt_wst(signal = wavelet_gastroc, dt = 1, energy_density = TRUE)

