#' Plots the chemical or mortality decay curve for a defined new insecticide 
#' 
#' @param mr VALUE pre-existing level of resistance to the new insecticide; 
#' logically involving (1-el)^x for some power x number of previously fixed 
#' mutations for the insecticide
#' @param iX VALUE name of new insecticide
#' @param iX_abb VALUE 3-letter abbreviated name of new insecticide
#' @param cd VALUE rate for chemical decay of new insecticide on bednets 
#' @param md VALUE rate for mortality decay from new insecticide on bednets 
#' @param mh VALUE half-life for mortality decay from new insecticide on bednets
#' @param xu VALUE cost in USD per relative unit of AI for the new insecticide 
#' @param curve specifies the type of decay curve to plot:
#'    "chemical": chemical decay curve (exponential)
#'    "mortality": mortality decay curve (logistic)
#'    
#' @return plots the chemical or mortality decay curve for a defined new 
#' insecticide that has a chemical loading of 1 (in relative units) over 3 years

plot_test_newAI <- function(
  
  # insecticide resistance level 
  mr = c(1), # mulitplier of mortality curve
  
  # insecticide names
  iX = c("New AI 1"), # label for reading in  
  
  # abbreciated named insecticide names
  iX_abb = c("NW1"), # label for plotting  
  
  # chemical decay 
  cd = c(0.02067709), # chemical decay rate per AI
  
  # mortality effect  
  md = c(-0.0312781), # mortality decay rate per AI
  mh = c(2.512306), # mortality half-life per AI
  
  # economic cost  
  xu = c(2.50), # cost per unit AI 
  
  # plot curve 
  curve = "mortality" # curve to plot 
  
){
  
  # simulation setup assuming baseline lifespan=3 and 12 mosquito generations pa
  ot = seq(1,3*12) 
  
  ###
  
  if(curve=="chemical"){ # chemical decay of known insecticides 
    plot(seq(1,3*12+1),rep(-10,3*12+1),font.lab=2,las=1,
         cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
         xlab="Time in Years",ylab="Relative Loading",main="")
    axis(1,at=1+(12*seq(0,12)),seq(0,12),las=1)
    points(seq(1,3*12),1/exp(cd*seq(1,3*12)),type="l",col=viridis(7)[7],lwd=6) 
    legend("topright",paste(iX," (",iX_abb,")",sep=""),pch=19,col=viridis(7)[7])
  }
  
  if(curve=="mortality"){ # mortality decay of known insecticides 
    plot(seq(1,3*12+1),rep(-10,3*12+1),font.lab=2,las=1,
         cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
         xlab="Time in Years",ylab="Proportional Mortality",main="")
    axis(1,at=1+(12*seq(0,12)),seq(0,12),las=1)
    points(seq(1,3*12),mr*exp(md*seq(1,3*12)+mh)/(1+exp(md*seq(1,3*12)+mh)),
           type="l",col=viridis(7)[7],lwd=6)
    legend("topright",paste(iX," (",iX_abb,")",sep=""),pch=19,col=viridis(7)[7])
  }
  
  box()
  
}

# END 