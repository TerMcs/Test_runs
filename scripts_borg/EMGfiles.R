


# Demonstrates batch analysis of data.
# The data is in multiple files that are selected interactively
# and put into a file list. Data files supposed to be of asci type with
# EMG data in column 1.
# F Borg 2010 -

# filelist -- contains paths to asci files with EMG data


library(tcltk)  # required for tk_choose.files etc


Filters <- matrix(c("EMG data", ".asc", "All files", "*"),
                  2, 2, byrow = TRUE)

if(interactive()) filelist <- tk_choose.files(filter = Filters)


outputpdf <- "C:/EMGanalysis1.pdf"  # output graphs to this file
outputtxt <- "C:/EMGanalysis1.txt"  # output text/numbers to this file

pdf(outputpdf) # open the pdf driver
fp <- file(outputtxt, "w") # open text-file for wirting
n <- length(filelist) 

for(i in 1:n){
	EMG <- read.table(filelist[i], header = FALSE)
	title <- paste("Data from ", filelist[i])
	cat("Standar deviation = ", sd(EMG$V1), " for data EMG$V1 in file ", filelist[i], "\n", file = fp)
	plot(EMG$V1, main = title, type = "l")
}

close(fp) # close the text file
dev.off() # close the pdf driver
