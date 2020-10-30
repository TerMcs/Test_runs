# load NIRS data (converted to csv via excel from tab delimited file saved via the NIRS software)

nirs2b <- read.csv("C:/Users/ThinkPad/OneDrive - Bournemouth University/Desktop/R-PhD/Test_runs/NIRS_data/Monitoring_3b.csv") 
nirs2b
SO2_data <- nirs2b[,2]
oxyHb_data <- nirs2b[,3]
deoxyHb_data <- nirs2b[,4]
plot(SO2_data, type = "l")
plot(oxyHb_data, type = "l")
plot(deoxyHb_data, type = "l")