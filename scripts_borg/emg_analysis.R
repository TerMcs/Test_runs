#RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
#
# EMG data analysis
# Example with quiet standing test data
#  	
# Frank Borg 2010 -
# frank.borg@chydenius.fi
# University of Jyväskylä,
# Kokkola University consortium Chydenius, Finland
#
#RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

    outputpdf = "C:/EMGR/result.pdf"  # output file for the report if chhosen report = TRUE  
	sinkfile = "C:/EMGR/sink.txt" # output file for the output which is usually diplayed at the command line
	
	report = TRUE  # if report = TRUE then the script will output pdf report, otherwise it will plot graphs in the console mode.
	
	usesink = FALSE # if set true then the output at the command line is directed to the sink file
	
	if(usesink) sink(file = sinkfile)
	
	EMGfuns <- "C:/EMGR/EMGfuns.R" # script file with EMG analysis fucntions
	
	source(EMGfuns) # loads the EMGfuns

	sampFreq1 <- 1000 # sampling rate for EMG, Hz
	sampFreq2 <- 100 # sampling rate for force plate data, Hz

	# data files
	bal1f <- "C:/EMGR/data.bal" # force plate data -- COP
	emg1f <- "C:/EMGR/data.emg" # EMG data

	BAL1 <- read.table(bal1f, header = FALSE) # COP data (mm)
	EMG1 <- read.table(emg1f, header = FALSE) # EMG data (mV)
	
	n1 <- nrow(EMG1)
	n2 <- nrow(BAL1)
	
	time1 <- seq(1,n1,1)/sampFreq1 # time for EMG data
	time2 <- seq(1,n2,1)/sampFreq2 # time for COP data	
	
	names(BAL1) <- c("COP_X", "COP_Y", "Fz") # COP = Center Of Pressure (unit: mm), Fz = total vertical force (unit: N)
	names(EMG1) <- c("TAd", "LGd", "MGd","TAs", "LGs", "MGs") # TAs = Tibialis ant. sinister, LG = Lateral gastroc., MG = Medial gastroc.
	
		op <- par(mar = c(4,4,4,4)) # sets margins for plot area and enough margin on the rigth side for labels and legend
	
	if(report){
		library(lattice) # lattice is a graphical package
		trellis.device(device = pdf, file = outputpdf, fonts = "Helvetica")
	}	


	#1. plot normalized raw, ARV, turns and bw0 (Butterworth) processed EMG
	
	ARV <- EMG_arv(EMG1$MGs)
		
	# legends for the plot
	main <- "Gastroc. med. sin. sEMG"
	xlab <- "Time (sec)" # x-axis legend

	ylab <- "EMG raw, ARV, turns, bw0" # y-axis legend
	mx <- max(abs(EMG1$MGs- mean(EMG1$MGs)), na.rm = TRUE)
	plot(time1, (EMG1$MGs- mean(EMG1$MGs))/mx, ylim = c(-1,5), type = "l", main = main, xlab = xlab, ylab = ylab, col = "black")
	mx <- max(ARV$arv, na.rm = TRUE)
	lines(time1, ARV$arv/mx + 1, col = "red") # adds second graph to the plot
	turns <- EMG_turns(EMG1$MGs)
	mx <- max(turns$turns.ps, na.rm = TRUE)
	lines(time1, turns$turns.ps/mx + 2, col = "blue") # add ssecond graph to the plot
	bw <- EMG_bw0(EMG1$MGs, cutOff = 1.0)
	bwmx <- max(abs(bw$bw0 - mean(bw$bw0)), na.rm = TRUE)
	lines(bw$time/1000, (bw$bw0 - mean(bw$bw0))/bwmx + 3, col = "green") # adds second graph to the plot
	legend("topright", c("Raw EMG", "ARV", "Turns", "BW0"), cex=0.8, col=c("black", "red", "blue", "green"), lty = c(1,1,1,1)) #adds legend
	
	
	if(!report) devAskNewPage(ask = TRUE) # This prompts the user to click the plot area in order to open the next plot
	
	#2. plot EMG spectrum
	
	spec <- EMG_spec(EMG1$MGs, plot = TRUE) 
	
	print(paste("Mean freq = ", round(spec$meanf, 1), " Hz"))  # prints to the sink
	print(paste("Median freq = ", round(spec$medianf,1), " Hz"))
	
	if(!report) devAskNewPage(ask = TRUE)
	
	#3. Plot mean/median freq vs time using short time Fourier transform STFT
	
	stft <- EMG_stft_f(EMG1$MGs, plot = TRUE) 
	
			
	#4. filter bank intensity analysis ... calculation may take some time
	
	if(!report) devAskNewPage(ask = TRUE)
	
	emgfb <- EMG_morvt(EMG1$MGs, J = 12, plot = TRUE)
	
	
	
	#5. Multiresolution analysis
	
	if(!report) devAskNewPage(ask = TRUE)
	
	s <-EMGx_mra(EMG1$MGs, J = 8, plot = TRUE)
	
	print(s$parameters)
	
	rm(s) # removes the variable s from memory
	
	
	#6. plot COP data
	
	if(!report) devAskNewPage(ask = TRUE)
	
	plot(BAL1$COP_X, BAL1$COP_Y, type = "l", main = "Posturogram", ylab = "COP Y (mm)", xlab = "COP X (mm)") 
	grid()

	#7. plot COP Y vs lowpass filtered MG EMG -- detail
	
	if(!report) devAskNewPage(ask = TRUE)
	
	mxy <- max(abs(BAL1$COP_Y - mean(BAL1$COP_Y)), na.rm = TRUE)
	
	plot(bw$time/1000, bw$bw0/bwmx, ylim = c(0,2),  
	main = "COP Y and Gastroc. EMG", xlab = "Time (s)", ylab = "Lowpass filtered rEMG",  type = "l")
	lines(time2, (BAL1$COP_Y - mean(BAL1$COP_Y))/mxy + 1, col = "red")
	axis(side = 4)
	mtext("COP Y", side = 4, line = 2, col = "red")
	print(bw$parameters)
	
	
	#8. Plot COP Y vs lowpassed Gastroc EMG
	
	if(!report) devAskNewPage(ask = TRUE)
	
	n <- length(EMG1$MGs)
	n1 <- length(BAL1$COP_Y)
	main <- "COP Y vs lowpassed Gastroc EMG"
	xlab = "COP Y, normalized"
	ylab = "Lowpassed rectified EMG"
	plot((BAL1$COP_Y - mean(BAL1$COP_Y))/mxy, downSample(bw$bw0, n, n1)/bwmx, main = main, xlab = xlab, ylab = ylab, type = "p", pch = ".")
		
	print(cor.test((BAL1$COP_Y - mean(BAL1$COP_Y)), downSample(bw$bw0, n, n1)))	# correlation between COP Y and lowpassed rectified EMG
	
	#8.b Coherence COP_Y vs downsampled EMG
	
	res <- EMGx_coh((BAL1$COP_Y - mean(BAL1$COP_Y))/mxy, downSample(bw$bw0, n, n1)/bwmx, sampFreq = 100)
		
	res <- EMG_coh((BAL1$COP_Y - mean(BAL1$COP_Y))/mxy, downSample(bw$bw0, n, n1)/bwmx, sampFreq = 100, DT = 4000, plot = TRUE)
	
	par(op) # restores default graphical parameters values
	
	if(usesink) {
		sink(file = NULL) # redirects sink to the standard input-output
		unlink(sinkfile)
	}	
	
	if(report) dev.off()
	

	