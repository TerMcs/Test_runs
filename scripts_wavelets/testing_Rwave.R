library(Rwave)

x <- 1:512
chirp <- sin(2*pi * (x + 0.002 * (x-256)^2 ) / 16)
wave_test <- cwt(chirp, noctave=4, nvoice=12, plot = T)
wave_test

emgx <- rectification(emgx, rtype = "fullwave")

wave_test3 <- cwt(emgx$values, noctave=6, nvoice=12, w0=1*pi, plot = T)
wave_test3_polar <- cwtpolar(wave_test3)

wave_test3 <- cwt(EMG2_gastroc, noctave=8, nvoice=12, w0=2*pi, plot = T)

plot(EMG2_gastroc)

library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(wave_test)
fig <- fig %>% add_surface()

fig

wave_test <- data.frame(wave_test)

fig

rm(z)

x <- 1:512
chirp <- sin(2*pi * (x + 0.002 * (x-256)^2 ) / 16)
retChirp <- cwt(chirp, noctave=5, nvoice=12, twoD=FALSE, plot=FALSE)
retPolar <- cwtpolar(retChirp)
retImageMod <- cwtimage(retPolar$modulus)
retImageArg <- cwtimage(retPolar$argument)

x <- 1:512
chirp <- sin(2*pi * (x + 0.002 * (x-256)^2 ) / 16)
retChirp <- cwtTh(chirp, noctave=5, nvoice=12, moments=20)

data(HOWAREYOU)
plot.ts(HOWAREYOU)
cgtHOWAREYOU <- cgt(HOWAREYOU,70,0.01,100)

cgtemgx <- cgt(emgx$values, nvoice = 20)
