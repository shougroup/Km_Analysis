###########		Values/Settings shared by all experiments	############################
# need to have the following R packages+dependencies installed: plyr, zoo, ggplot2, drc
#	option to plot/calculate replicates individually or averaged
#  don't use R studio, it throws up plotting errors
# Open bioact3 output and make sure all columns are in "number" not "scientific" format

require(zoo); require(plyr); require(ggplot2); require(drc);
################	Experiment 1 	#########################
setup<-NULL #initialize the setup object
# Location of the Km analysis code, make sure the path is correct for YOU (different on different machines)
#for Mac
#setup$code=c("/Volumes/fh/shougroup/_Biology_Protocols/Yeast/Km_StarvationToleranceAssay/AnalysisCode/KmAnalysis_v4.0.R")
# for PC 
setup$code=c("X:/fast/shou_w/shougroup/_Biology_Protocols/Yeast/Km_StarvationToleranceAssay/AnalysisCode/KmAnalysis_v4.0.R")
# working directory containing bioact3 output, can be set individually, make sure the path is correct for YOU!
#for Mac
#setup$wd="/Volumes/fh/shougroup/_Biology_Protocols/Yeast/Km_StarvationToleranceAssay/AnalysisCode/ExampleData" 
#for PC
setup$wd="X:/fast/shou_w/shougroup/_Biology_Protocols/Yeast/Km_StarvationToleranceAssay/AnalysisCode/ExampleData"

setwd(setup$wd)

# nutrient being used (shows up in graphs & file names)
setup$nutr="Lys" 
# length of prestarvation (shows up in graphs & file names)
setup$prestarve="6hPrestarve" 
# number of timepoints used for linear model window 
setup$n=4 
# fraction of maximum intensity for which to consider using the data, currently only used for "individual" or fixed window methods
setup$p=1.0 

# Options: 
#	"maximum" finds maximum growth rate, 
#	"individual" finds lowest residual window for each strain/conc combo, 
#	"overall" finds window with lowest residuals for all strains at a given concentration (makes sure growth rate is predicted in the same time window. 
#	If you set method to number 5, all windows start at that time-point with length n
setup$method="maximum" 

#set of high contrast color pairs (maximum of 12), Colors will be assigned to strains in the order listed (e.g. WY2218 will be chartreuse2)
setup$colors=c("salmon","red","chartreuse2","chartreuse4","dodgerblue","blue","deeppink1","deeppink3","grey48","black","darkorange","darkorange3") 

setup$cellsperwell=1500 #not actually used in the code but gets stored with all the other settings
setup$microscope="broad" #  shows up in graphs and filenames
setup$date="20150324" #shows up in graphs and filenames
setup$infile="./InputFiles/DONT_DELETE_ExampleData1.txt" # tab delimited bioact3 textfile (code looks for columns labeled "position","slice","min.elapsed","expTcorr_bs_intden") Open in excel once to chance all values from scientific to numbers.

# Or read it in from a tab delimited text file in the working directory with columns position, strain, conc, & rep
# strains will be plotted in the order listed here
setup$lookup<-read.delim("./InputFiles/DONT_DELETE_Example_lookuptable.tab")

# Make sure your lookup table is correct before proceeding!
setup$lookup

# set to 1 if you want ot check the intensity curves for every tile, should be done to find positions with problems.
TileScreen=0

# set desired data range & tiles to exclude
#number of timepoints to use for analysis
setup$slices=1:9 
# Tiles to exclude from analysis (usually based on screen tiles)
setup$Set2NA<-as.factor(c("B03a","B03b","B03c","B03d","E06a","E06b","E06c","E06d","E07a","E07b","E07c","E07d","F02d","F03a","F05a","F05b","F05c","F05d","F07a","F10c","F11a","F11b","F11d","G03b","G03d","G08c","G11a","H03d","H05b","H10a","H10b","H10c","H10d"))
# runs the code
source(setup$code,echo=TRUE,local=TRUE)
# warnings during plotting are normal, as are errors during Km calculation (if the data really doesn't fit the Monod curve the solution won't converge)

#saves the Results
Example1<-Results
################	Experiment 2 	#########################
# Second Example without comments
setup<-NULL
setup$code=c("/Volumes/fh/shougroup/_Biology_Protocols/Yeast/Km_StarvationToleranceAssay/AnalysisCode/KmAnalysis_v4.0.R")
setup$wd="/Volumes/fh/shougroup/_Biology_Protocols/Yeast/Km_StarvationToleranceAssay/AnalysisCode/ExampleData"
setwd(setup$wd)

setup$nutr="Lys"
setup$n=4
setup$p=1
setup$method="maximum"
setup$prestarve="6hPrestarve"
setup$colors=c("salmon","red","chartreuse2","chartreuse4","dodgerblue","deeppink1","deeppink3","grey48","black","darkorange","darkorange3")

setup$cellsperwell=1500
setup$microscope="offbroad"
setup$date="20150324"
setup$infile="./InputFiles/DONT_DELETE_ExampleData2.txt"

# Setup lookup table
setup$lookup<-read.delim("./InputFiles/DONT_DELETE_Example_lookuptable.tab")
setup$lookup

# set desired data range & tiles to exclude
setup$slices=1:9
setup$Set2NA<-as.factor(c("A05b","A12a","A12b","A12c","A12d","B06a","D01b","F03d","F04a","F08b","G12d","E06d","E07a","F07a","F07b","F07c","F07d","G06c"))

TileScreen=1

source(setup$code,echo=TRUE,local=TRUE)
Example2<-Results

####################################################################################################################################################
####################	Code for calculating & plotting the average Growth Rate vs conc with Standard error	########################################
####################################################################################################################################################

# list of at least two Results$results objects
lst<-list(Example1=Example1$results,Example2=Example2$results)

# Calculates Average growth rate + Std. deviation 
meanGR<-calcMeanGR(lst,.(strain,conc))
outfile=paste("MeanGR","_",setup$nutr,"_",setup$prestarve,"_","p=",setup$p,"_","n=",setup$n,"_","method=",setup$method,sep="")
write.table(meanGR,file=paste(outfile,"_fits.tab",sep=""),quote=FALSE,row.names=FALSE,sep="\t")

# Calculates Km using data from all the replicates
meanKm<-calcKm(lst,.(strain))
outfile=paste("MeanKm","_",setup$nutr,"_",setup$prestarve,"_","p=",setup$p,"_","n=",setup$n,"_","method=",setup$method,sep="")
write.table(meanKm$results,file=paste(outfile,"_Km.tab",sep=""),quote=FALSE,row.names=FALSE,sep="\t")

#plotting results with error bars
pdf(file=paste(outfile,"_plots.pdf",sep=""),width=10.5, height=8)
mytheme = 	list(
				geom_line(linetype="dotted", size=0.2),
				geom_pointrange(size=0.8,shape=2,position=position_jitter(w=0.1),aes(ymin=mean.g.rate-sd.g.rate/sqrt(n),ymax=mean.g.rate+sd.g.rate/sqrt(n))),
				xlab(paste("[",setup$nutr,"] in uM",sep="")),
				ylab("Growth Rate (1/hr)"),
				scale_color_manual(values=setup$colors,limits=setup$strains),
				ggtitle(paste("Growth Rate at x uM",setup$nutr,"\nMean +/- SEM is shown\n(p =",setup$p,"n =",setup$n,"method =",setup$method,") Data Points are jittered but lines are not. x-axis is categorical",sep=" ")),
				theme(legend.position="right",legend.text=element_text(size=7,face="bold"),plot.title=element_text(size=10,hjust = 0))
				)
ggplot(data=meanGR, aes(x=factor(conc, levels=levels(as.factor(meanGR$conc))[order(as.numeric(levels(as.factor(meanGR$conc))))]), y=mean.g.rate ,col=strain,group=strain))+mytheme

# plotting results with individual points
mytheme = 	list(
				geom_line(linetype="dotted", size=0.2),
				geom_point(data=ldply(lst),shape=2,position=position_jitter(w=0.1),aes(x=conc,y=g.rate, group=strain)),
				xlab(paste("[",setup$nutr,"] in uM",sep="")),
				ylab("Growth Rate (1/hr)"),
				scale_color_manual(values=setup$colors,limits=setup$strains),
				ggtitle(paste("Growth Rate at x uM",setup$nutr,"\nIndividual data points are shown\n(p =",setup$p,"n =",setup$n,"method =",setup$method,") Data points are jittered but lines are not. x-axis is categorical",sep=" ")),
				theme(legend.position="right",legend.text=element_text(size=7,face="bold"),plot.title=element_text(size=10,hjust = 0))
				)
ggplot(data=meanGR, aes(x=factor(conc, levels=levels(as.factor(meanGR$conc))[order(as.numeric(levels(as.factor(meanGR$conc))))]), y=mean.g.rate ,col=strain,group=strain))+mytheme

# plotting Km curves
mytheme = 	list(
				xlab(paste("[",setup$nutr,"] in uM",sep="")),
				ylab("Growth Rate (1/hr)"),
				scale_x_log10(),
				annotation_logticks(sides="b",size=0.2),
				scale_color_manual(values=setup$colors,limits=setup$strains),
				ggtitle(paste(setup$nutr,"Mean Growth Rate +/- SEM & Predicted Monod Curves & Km+/-fitting error at y=0)","\n(p =",setup$p,"n =",setup$n,"method =",setup$method,")",sep=" ")),
				theme(legend.position="right",legend.text=element_text(size=7,face="bold"),plot.title=element_text(size=10,hjust = 0))
				)

ggplot(data=meanKm$results,aes(x=Km,y=half.max,color=strain)) + mytheme+
geom_point(shape="X",size=3,aes(x=Km,y=0))+ 
geom_errorbarh(data=meanKm$results,aes(xmin=Km-Km.err,xmax=Km+Km.err,y=0,color=strain,height=0.01))+
geom_line(data=meanKm$preds, linetype="twodash",aes(x=conc, y=g.rate ,color=strain,group=strain))+
geom_pointrange(data=meanGR,size=0.6,position=position_jitter(w=0.02),shape=20,aes(x=as.numeric(conc),y=mean.g.rate, ymin=mean.g.rate-sd.g.rate/sqrt(n),ymax=mean.g.rate+sd.g.rate/sqrt(n)))#+
#geom_point(data=meanKm$data,shape=4,aes(x=as.numeric(conc),y=g.rate)) # if you want the individual data points instead
dev.off()