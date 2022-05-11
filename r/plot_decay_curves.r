#' Plots the chemical, mortality and physical decay curves for insecticides 
#' 
#' @param output_list LIST returned output from run_simulation(), which is a 
#' list containing the three-dimensional array (where the three dimensions are 
#' bednet lifespan and chemical loading AI 1 and AI 2) of cumulative vector 
#' population size and all defined and/or useful parameters 
#' @param optimum_list LIST returned output from calculate_optimum() containing 
#' the optimum as a vector in element-index and interpreted forms, followed by 
#' the input for opt_name
#' @param select_opt VALUE specifies the row number of the optimum to plot, 
#' which is only relevant when there are multiple optima found in optimum_list 
#' @param curve VALUE specifies the type of decay curve to plot:
#'    "chemical": chemical decay curve (exponential)
#'    "mortality": mortality decay curve (logistic)
#'    "physical": physical survival decay curve (logistic)
#'    
#' @return plots the chemical, mortality or physical decay curves for all 
#' insecticides at a baseline a chemical loading of 1 (in relative units) over 3
#' years; or for selected insecticides (as chosen within optimum_list) at their 
#' optimal chemical loading and across their optimal bednet lifespan 

plot_decay_curves <- function(output_list,optimum_list=NA,select_opt=1,curve){
  
  # interpret parameters
  output_array = output_list[[1]]
  os = output_list[[2]]
  og = output_list[[3]]
  oy = output_list[[4]]
  ol = output_list[[5]]
  oi_b = output_list[[6]]
  oi_c = output_list[[7]]
  ou = output_list[[8]]
  er = output_list[[9]]
  ek = output_list[[10]]
  el = output_list[[11]]
  xb = output_list[[12]]
  xn = output_list[[13]]
  xs = output_list[[14]]
  pm = output_list[[15]]
  p0 = output_list[[16]]
  p3 = output_list[[17]]
  iA = output_list[[18]]
  iB = output_list[[19]]
  mr_set = output_list[[20]]
  iX_set = output_list[[21]]
  iX_abb = output_list[[22]]
  cd_set = output_list[[23]]
  md_set = output_list[[24]]
  mh_set = output_list[[25]]
  xu_set = output_list[[26]]
  cd = output_list[[27]]
  mr = output_list[[28]]
  md = output_list[[29]]
  mh = output_list[[30]]
  xu = output_list[[31]]
  pd = output_list[[32]]
  ph = output_list[[33]]
  ot = output_list[[34]]
  ob = output_list[[35]]
  oc = output_list[[36]]
  
  # if no optimum given, select baseline 
  if(length(optimum_list)==1){
    opt = c(3,1,1)
  } else {
    opt = optimum_list[[2]][select_opt,]
    opt_name = optimum_list[[3]]
  }
  
  if(curve=="chemical"){ # chemical decay of known insecticides 
    if(length(optimum_list)==1){ # plot all baseline scenarios 
      plot(seq(1,3*og+1),rep(-10,3*og+1),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
           xlab="Time in Years",ylab="Relative Loading",main="")
      axis(1,at=1+(og*seq(0,oy)),seq(0,oy),las=1)
      for(i in 1:7){points(seq(1,3*og),1/exp(cd_set[i]*seq(1,3*og)),
                           type="l",col=viridis(7,alpha=0.5)[i],lwd=6)}    
      legend("topright",paste(iX_set," (",iX_abb,")",sep=""),pch=19,
             col=viridis(7,alpha=0.5))
    } else { # plot an optimal scenario 
      plot(seq(1,opt[1]*og+1),rep(-10,opt[1]*og+1),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,4.49),xaxt='n',xaxs="i",yaxs="i",
           xlab="Time in Years",ylab="Relative Loading",main="")
      axis(1,at=1+(og*seq(0,oy)),seq(0,oy),las=1)
      points(seq(1,opt[1]*og),opt[2]/exp(cd[1]*seq(1,opt[1]*og)),
             type="l",col=viridis(7)[which(iX_set==iA)],lwd=6)
      points(seq(1,opt[1]*og),opt[3]/exp(cd[2]*seq(1,opt[1]*og)),
             type="l",col=viridis(7)[which(iX_set==iB)],lwd=6)
      legend("topright",paste(c(iA,iB)," (",
                              iX_abb[c(which(iX_set==iA),which(iX_set==iB))],
                              ")",sep=""),pch=19,
             col=viridis(7)[c(which(iX_set==iA),which(iX_set==iB))])
    }
  }
  
  if(curve=="mortality"){ # mortality decay of known insecticides 
    if(length(optimum_list)==1){ # plot all baseline scenarios 
      plot(seq(1,3*og+1),rep(-10,3*og+1),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
           xlab="Time in Years",ylab="Proportional Mortality",main="")
      axis(1,at=1+(og*seq(0,oy)),seq(0,oy),las=1)
      for(i in 1:7){points(seq(1,3*og),
                           mr_set[i]*exp(md_set[i]*seq(1,3*og)+mh_set[i])/
                             (1+exp(md_set[i]*seq(1,3*og)+mh_set[i])),type="l",
                           col=viridis(7,alpha=0.5)[i],lwd=6)}
      legend("topright",paste(iX_set," (",iX_abb,")",sep=""),pch=19,
             col=viridis(7,alpha=0.5))
    } else { # plot an optimal scenario 
      plot(seq(1,opt[1]*og+1),rep(-10,opt[1]*og+1),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
           xlab="Time in Years",ylab="Proportional Mortality",main="")
      axis(1,at=1+(og*seq(0,oy)),seq(0,oy),las=1)
      load1 = log(1/opt[2])/cd[1] # relative AI effect
      mort1 = mr[1]*exp(md[1]*(seq(1,opt[1]*og)+load1)+mh[1])/
        (1+exp(md[1]*(seq(1,opt[1]*og)+load1)+mh[1])) # mortality 
      load2 = log(1/opt[3])/cd[2] # relative AI effect
      mort2 = mr[2]*exp(md[2]*(seq(1,opt[1]*og)+load2)+mh[2])/
        (1+exp(md[2]*(seq(1,opt[1]*og)+load2)+mh[2])) # mortality 
      points(seq(1,opt[1]*og),mort1,type="l",
             col=viridis(7)[which(iX_set==iA)],lwd=6)
      points(seq(1,opt[1]*og),mort2,type="l",
             col=viridis(7)[which(iX_set==iB)],lwd=6)
      legend("topright",paste(c(iA,iB)," (",
                              iX_abb[c(which(iX_set==iA),which(iX_set==iB))],
                              ")",sep=""),pch=19,
             col=viridis(7)[c(which(iX_set==iA),which(iX_set==iB))])
    }
  }
  
  if(curve=="physical"){ # physical decay of bednets 
    if(length(optimum_list)==1){
      plot(seq(1,3*og+1),rep(-10,3*og+1),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
           xlab="Time in Years",ylab="Proportional Bed-Net Survival",main="")
      axis(1,at=1+(og*seq(0,oy)),seq(0,oy),las=1)
      points(seq(1,3*og),pm*exp(pd*seq(1,3*og)+ph)/(1+exp(pd*seq(1,3*og)+ph)),
             type="l",col="grey",lwd=6)
    } else {
      plot(seq(1,opt[1]*og+1),rep(-10,opt[1]*og+1),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,1.19),xaxt='n',xaxs="i",yaxs="i",
           xlab="Time in Years",ylab="Proportional Bed-Net Survival",main="")
      axis(1,at=1+(og*seq(0,oy)),seq(0,oy),las=1)
      points(seq(1,opt[1]*og),pm*exp(pd*seq(1,opt[1]*og)+ph)/
               (1+exp(pd*seq(1,opt[1]*og)+ph)),type="l",col="grey",lwd=6)
    }
  }
  
  box()
  
}

# END 