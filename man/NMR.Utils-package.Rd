\name{NMR.Utils-package}
\alias{NMR.Utils-package}
\alias{NMR.Utils}
\docType{package}
\title{
\packageTitle{NMR.Utils}
}
\description{
\packageDescription{NMR.Utils}
}
\details{

\packageIndices{NMR.Utils}

Load the library, then follow one of the examples below.

}
\author{
\packageAuthor{NMR.Utils}

Maintainer: \packageMaintainer{NMR.Utils}
}

\keyword{ package }

\examples{
#Load the Library
library(NMR.Utils)
#Read in the NMR Data
data=read.NMR("path/to/real.txt",imaginary_file="path/to/imaginary.txt")
#Read the ATMC log file to find out when each scan occured (uncomment next two lines if you used autotune)
#offsets=read.ATMC("path")
#data=storeOffsets(data,offsets)
#Read the echem data
#echem=read.echem("/path/to/echem.mpt",'biologic')
#Reduce number of scans to those containing data
data=reduceScans(data, c(1,45))
#Attempt to autophase data
data=apkpseudo2d(data,c(-200,200),c(1000,1300),0,c(-50,50),c(-5,0))
#Forget about the immaginary data once phased
data=makeReal(data)
#Perform baseline correction
data=correct_baseline(data, method="rollingBall",wm=800,ws=200)
#Plot the data
plotinsitu(data,echem,xrange=c(1500,-500), yrange='auto', plot_offset=100000, plot.colour.ranges=list(c(210,900)), plot.colour.yranges=list(c(50000,200000)),V_range=c(0,2))

##FITTING##

fit=new_fit(integration_range=c(-200,200))

electrolyte=new_model(pseudoVoigt,height=1e9,centre=-9.95,hwhm=2.1,shape=1)
SEI=new_model(pseudoVoigt,height=1e7,centre=-9,hwhm=32,shape=1)
peak=new_model(pseudoVoigt,height=1e7,centre=20,hwhm=17,shape=1)

electrolyte=add_constraint_to_model(electrolyte, parameter='height', constraint_type='vary', constraint=5e7)
#electrolyte=add_constraint_to_model(electrolyte, parameter='centre', constraint_type='vary', constraint=0.25)
electrolyte=add_constraint_to_model(electrolyte, parameter='hwhm', constraint_type='vary', constraint=0.1)

SEI=add_constraint_to_model(SEI, parameter='height', constraint_type='vary', constraint=1e7)
SEI=add_constraint_to_model(SEI, parameter='centre', constraint_type='range', constraint=c(-12,-9))
SEI=add_constraint_to_model(SEI, parameter='hwhm', constraint_type='variable_range', constraint=c(10,30,50))

peak=add_constraint_to_model(peak, parameter='height', constraint_type='vary', constraint=1e7)
peak=add_constraint_to_model(peak, parameter='centre', constraint_type='variable_range', constraint=c(5,10,60))
peak=add_constraint_to_model(peak, parameter='hwhm', constraint_type='variable_range', constraint=c(1,0,30))

fit=add_model_to_fit(fit,electrolyte)
fit=add_model_to_fit(fit,SEI)
fit=add_model_to_fit(fit,peak)

fit=run_fit_for_data(fit,data)

plot_fit(fit,data,echem)

}
