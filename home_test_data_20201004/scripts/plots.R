
library(biosignalEMG)

emg_4_mvc <- read.csv("home_test_data_20201004/emg_Run_4_mvc.csv") 
emg_IMU_4 <- read.csv("home_test_data_20201004/emg_Run_4.csv") 
nirs_5 <- read.csv("home_test_data_20201004/nirs_Run_5.csv")

emgA_4_rect <- emg_IMU_4[,2]
  
emgA_4_rect <- emg(emgA_4_rect, samplingrate = 1261, units = "mV", data.name = "Random EMG")
emgA_4_rect <- rectification(emgA_4_rect, rtype = "fullwave")
plot(emgA_4_rect, main = "Free standing flexion and return, rectified", type = "l")
emgA_4_rec_rms <- envelope(emgA_4_rect, method = "RMS", wsize = 500)
plot(emgA_4_rec_rms)

emgB_4_rect <- emg_IMU_4[1:210000,16]

emgB_4_rect <- emg(emgB_4_rect, samplingrate = 1261, units = "mV", data.name = "Random EMG")
emgB_4_rect <- rectification(emgB_4_rect, rtype = "fullwave")
plot(emgB_4_rect, main = "Free standing flexion and return, rectified", type = "l")
emgB_4_rec_rms <- envelope(emgB_4_rect, method = "RMS", wsize = 100)
plot(emgB_4_rec_rms, main = "Free standing flexion and return RMS", type = "l")

plot(emg_IMU_4[,1], emg_IMU_4[,2], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 EMG (mV)", type = "l")
plot(emg_IMU_4[,3], emg_IMU_4[,4], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 Accelerometer x-axis",type = "l")
plot(emg_IMU_4[,5], emg_IMU_4[,6], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 Accelerometer y-axis",type = "l")
plot(emg_IMU_4[,7], emg_IMU_4[,8], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 Accelerometer z-axis",type = "l")
plot(emg_IMU_4[,9], emg_IMU_4[,10], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 Gyro x-axis",type = "l")
plot(emg_IMU_4[,11], emg_IMU_4[,12], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 Gyro y-axis",type = "l")
plot(emg_IMU_4[,13], emg_IMU_4[,14], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L2 Gyro z-axis",type = "l")
plot(emg_IMU_4[,15], emg_IMU_4[,16], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 EMG (mV)",type = "l")
plot(emg_IMU_4[,17], emg_IMU_4[,18], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 Accelerometer x-axis",type = "l")
plot(emg_IMU_4[,19], emg_IMU_4[,20], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 Accelerometer y-axis",type = "l")
plot(emg_IMU_4[,21], emg_IMU_4[,22], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 Accelerometer z-axis",type = "l")
plot(emg_IMU_4[,23], emg_IMU_4[,24], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 Gyro x-axis",type = "l")
plot(emg_IMU_4[,25], emg_IMU_4[,26], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 Gyro y-axis",type = "l")
plot(emg_IMU_4[,27], emg_IMU_4[,28], main = "Free standing flexion and return", 
     xlab = "time", ylab = "L4 Gyro z-axis",type = "l")

plot(nirs_5[,2], type = "l")
plot(nirs_5[,3], type = "l")
plot(nirs_5[,4], type = "l")

plot(nirs_4[,2], type = "l")
plot(nirs_4[,3], type = "l")
plot(nirs_4[,4], type = "l")


head(emg_4_rect)

plot(emg_4_rect$Values, main = "Free standing flexion and return RMS", type = "l")
