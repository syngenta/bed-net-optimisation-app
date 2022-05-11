#' Plots the impact of a chosen bednet product on the mosquito population  
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
#' @param curve VALUE specifies what to plot:
#'    "Both": plots dynamics for population size and allele frequencies  
#'    "Population Size": plots dynamics for population size only 
#'    "Allele Frequency": plots dynamics for allele frequencies only 
#'    
#' @return a plot showing the dynamics of the impact of a chosen bednet product 
#' on the mosquito population through time 

plot_optimal_dynamics <- function(output_list,optimum_list=NA,select_opt=1,
                                  curve="Both"){
  
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
    opt_name = "Baseline"
  } else {
    opt = optimum_list[[2]][select_opt,]
    opt_name = optimum_list[[3]]
  }
  
  # calculate underlying curves  
  cyct = 1+(ot-1)%%(og*opt[1]) # bednet lifespan cycle time 
  use1 = rep(1,oy*og) # Mixtures
  use2 = use1
  if(os=="Mosaic"){
    use1 = rep(0.5,oy*og)
    use2 = use1
  }
  if(os=="Rotation"){
    use1 = rep(c(rep(1,og*opt[1]),rep(0,og*opt[1])),
               ceiling(0.5*oy/opt[1]))[1:(oy*og)]
    use2 = (!use1)+0
  }
  phys = pm*exp(pd*cyct+ph)/(1+exp(pd*cyct+ph)) # physical survival 
  if(opt[2]==0){
    load1 = 0
    mort1 = rep(0,length(cyct)) # mortality 
  } else {
    load1 = log(1/opt[2])/cd[1]
    mort1 = mr[1]*exp(md[1]*(cyct+load1)+mh[1])/
      (1+exp(md[1]*(cyct+load1)+mh[1])) # mortality 
  }
  if(opt[3]==0){
    load2 = 0 
    mort2 = rep(0,length(cyct)) # mortality 
  } else {
    load2 = ifelse(opt[3]==0,0,log(1/opt[3])/cd[1])
    mort2 = mr[2]*exp(md[2]*(cyct+load2)+mh[2])/
      (1+exp(md[2]*(cyct+load2)+mh[2])) # mortality 
  }
  cover = (opt[1]/3)*xb/(xn+opt[2]*xu[1]+opt[3]*xu[2])
  if(os!="Mixture"){
    cover = (opt[1]/3)*xb/(xn+0.5*opt[2]*xu[1]+0.5*opt[3]*xu[2])
  }
  cover[cover>1] = 1
  
  # fitness 
  if(os!="Mosaic"){
    wAB = 1-cover*phys + 
      cover*phys*(1-(1-el)*use1*mort1)*(1-(1-el)*use2*mort2)
    wAb = 1-cover*phys + cover*phys*(1-(1-el)*use1*mort1)*(1-use2*mort2)
    waB = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-(1-el)*use2*mort2)
    wab = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-use2*mort2)
    wA = 1-(1-el)*mort1
    wa = 1-mort1
    wB = 1-(1-el)*mort2
    wb = 1-mort2
  }
  if(os=="Mosaic"){
    wAB = 1-cover*phys + cover*phys*(1-0.5*(1-el)*(mort1+mort2))
    wAb = 1-cover*phys + cover*phys*(1-0.5*((1-el)*mort1+mort2))
    waB = 1-cover*phys + cover*phys*(1-0.5*(mort1+(1-el)*mort2))
    wab = 1-cover*phys + cover*phys*(1-0.5*(mort1+mort2))
    wA = 1-0.5*(1-el)*mort1
    wa = 1-0.5*mort1
    wB = 1-0.5*(1-el)*mort2
    wb = 1-0.5*mort2
  }
  # setup 
  fA = 1/ek
  fB = 1/ek
  n = ek
  tA = 0
  tB = 0 
  wAa = c()
  wBb = c()
  # loops 
  for(tt in ot){ # generations 
    # mean fitness
    wAaBb = fA[tt]*fB[tt]*wAB[tt]+fA[tt]*(1-fB[tt])*wAb[tt]+
      (1-fA[tt])*fB[tt]*waB[tt]+(1-fA[tt])*(1-fB[tt])*wab[tt]
    wAa = append(wAa,fA[tt]*wA[3*og]+(1-fA[tt])*wa[3*og])
    wBb = append(wBb,fB[tt]*wB[3*og]+(1-fB[tt])*wb[3*og])
    # frequency at locus 1 & 2
    fA = append(fA,(fA[tt]*fB[tt]*wAB[tt]+fA[tt]*(1-fB[tt])*wAb[tt])/wAaBb)
    fB = append(fB,(fA[tt]*fB[tt]*wAB[tt]+(1-fA[tt])*fB[tt]*waB[tt])/wAaBb)
    # population size 
    n = append(n,n[tt]*wAaBb*(1+er*(1-(n[tt]*wAaBb/ek))))
    # limit for new mutation 
    if(fA[tt+1]>0.99){
      tA = tA + n[tt+1]
      if(tA>(1/ou)){
        tA = 0 
        fA[tt+1] = 1/ek 
        mort1 = (1-el)*mort1
        # fitness 
        if(os!="Mosaic"){
          wAB = 1-cover*phys + 
            cover*phys*(1-(1-el)*use1*mort1)*(1-(1-el)*use2*mort2)
          wAb = 1-cover*phys + cover*phys*(1-(1-el)*use1*mort1)*(1-use2*mort2)
          waB = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-(1-el)*use2*mort2)
          wab = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-use2*mort2)
          wA = 1-(1-el)*mort1
          wa = 1-mort1
        }
        if(os=="Mosaic"){
          wAB = 1-cover*phys + cover*phys*(1-0.5*(1-el)*(mort1+mort2))
          wAb = 1-cover*phys + cover*phys*(1-0.5*((1-el)*mort1+mort2))
          waB = 1-cover*phys + cover*phys*(1-0.5*(mort1+(1-el)*mort2))
          wab = 1-cover*phys + cover*phys*(1-0.5*(mort1+mort2))
          wA = 1-0.5*(1-el)*mort1
          wa = 1-0.5*mort1
        }
      }
    }
    if(fB[tt+1]>0.99){
      tB = tB + n[tt+1]
      if(tB>(1/ou)){
        tB = 0 
        fB[tt+1] = 1/ek 
        mort2 = (1-el)*mort2
        # fitness 
        if(os!="Mosaic"){
          wAB = 1-cover*phys + 
            cover*phys*(1-(1-el)*use1*mort1)*(1-(1-el)*use2*mort2)
          wAb = 1-cover*phys + cover*phys*(1-(1-el)*use1*mort1)*(1-use2*mort2)
          waB = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-(1-el)*use2*mort2)
          wab = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-use2*mort2)
          wB = 1-(1-el)*mort2
          wb = 1-mort2
        }
        if(os=="Mosaic"){
          wAB = 1-cover*phys + cover*phys*(1-0.5*(1-el)*(mort1+mort2))
          wAb = 1-cover*phys + cover*phys*(1-0.5*((1-el)*mort1+mort2))
          waB = 1-cover*phys + cover*phys*(1-0.5*(mort1+(1-el)*mort2))
          wab = 1-cover*phys + cover*phys*(1-0.5*(mort1+mort2))
          wB = 1-0.5*(1-el)*mort2
          wb = 1-0.5*mort2
        }
      }
    }
  }
  
  # plot optimal dynamics 
  #if(os=="Mixture"){
  #  mort_after3 = (1-(1-(mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
  #                                   (1+exp(md[1]*(3*og+load1)+mh[1]))))*
  #                             (1-(mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
  #                                   (1+exp(md[2]*(3*og+load2)+mh[2])))))
  #} else {
  #  mort_after3 = c(mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
  #                              (1+exp(md[1]*(3*og+load1)+mh[1])),
  #                            mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
  #                              (1+exp(md[2]*(3*og+load2)+mh[2])))
  #}
  # yaxis label 
  yaxislab = ifelse(curve=="Both","Population Size / Allele Frequency",curve)
  # plot
  if(curve!="Resistance Phenotype"){
    if(curve=="Both"|curve=="Population Size"){
      plot(c(0,og*oy),rep(-10,2),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(0,log10(ek)+3),
           xaxt='n',xaxs="i",yaxs="i",xlab="Time in Years",ylab=yaxislab,main="")
      #paste(opt_name," Optimum: ",paste(round(100*mort_after3,1),collapse="% & "),
      #      "% mortality at 3 years",sep="")
      points(rep(og*opt[1],2),c(0,log10(ek)+3),type="l",col="grey")
    } 
    if(curve=="Allele Frequency"){
      plot(c(0,og*oy),rep(-15,2),font.lab=2,las=1,
           cex.lab=1.5,cex.main=1.5,ylim=c(-1+log10(1/ek),3),
           xaxt='n',xaxs="i",yaxs="i",xlab="Time in Years",ylab=yaxislab,main="")
      #paste(opt_name," Optimum: ",paste(round(100*mort_after3,1),collapse="% & "),
      #      "% mortality at 3 years",sep="")
      points(rep(og*opt[1],2),c(-1+log10(1/ek),3),type="l",col="grey")
    }
    axis(1,at=(og*seq(0,oy)),seq(0,oy),las=1)
  }
  if(curve=="Both"|curve=="Population Size"){
    points(seq(0,oy*og),log10(n),type="l",col=viridis(3)[1],lwd=6)
  }
  if(curve=="Both"|curve=="Allele Frequency"){
    if(opt[2]==0){
      if(curve=="Allele Frequency"){
        points(seq(0,oy*og),log10(fA),type="l",lwd=6,col=viridis(3)[2])
      } else {
        points(seq(0,oy*og),log10(ek*fA),type="l",lwd=6,col=viridis(3)[2]) 
      }
    } else {
      switchA = log10(ek*c(fA,1))-log10(ek*c(0,fA))<(-1)
      switchA[1] = TRUE
      if(sum(switchA)==1){
        if(curve=="Allele Frequency"){
          points(seq(0,oy*og),log10(fA),type="l",lwd=6,col=viridis(3)[2])
        } else {
          points(seq(0,oy*og),log10(ek*fA),type="l",lwd=6,col=viridis(3)[2]) 
        }
      } else {
        for(i in 1:sum(switchA)){
          if(i<sum(switchA)){
            if(curve=="Allele Frequency"){
              points(seq(-1+which(switchA)[i],-2+which(switchA)[i+1]),
                     log10(fA[seq(which(switchA)[i],-1+which(switchA)[i+1])]),
                     type="l",lwd=6,col=viridis(3)[2])
            } else {
              points(seq(-1+which(switchA)[i],-2+which(switchA)[i+1]),
                     log10(ek*fA[seq(which(switchA)[i],-1+which(switchA)[i+1])]),
                     type="l",lwd=6,col=viridis(3)[2])
            }
          } else {
            if(curve=="Allele Frequency"){
              points(seq(-1+which(switchA)[i],-1+oy*og),
                     log10(fA[seq(which(switchA)[i],oy*og)]),
                     type="l",lwd=6,col=viridis(3)[2])
            } else {
              points(seq(-1+which(switchA)[i],-1+oy*og),
                     log10(ek*fA[seq(which(switchA)[i],oy*og)]),
                     type="l",lwd=6,col=viridis(3)[2]) 
            }
          }
        }
      }
    }
    if(opt[3]==0){
      if(curve=="Allele Frequency"){
        points(seq(0,oy*og),log10(fB),type="l",lwd=6,col=viridis(3)[3])
      } else {
        points(seq(0,oy*og),log10(ek*fB),type="l",lwd=6,col=viridis(3)[3]) 
      }
    } else {
      switchB = log10(ek*c(fB,1))-log10(ek*c(0,fB))<(-1)
      switchB[1] = TRUE
      if(sum(switchB)==1){
        if(curve=="Allele Frequency"){
          points(seq(0,oy*og),log10(fB),type="l",lwd=6,col=viridis(3)[3])
        } else {
          points(seq(0,oy*og),log10(ek*fB),type="l",lwd=6,col=viridis(3)[3]) 
        }
      } else {
        for(i in 1:sum(switchB)){
          if(i<sum(switchB)){
            if(curve=="Allele Frequency"){
              points(seq(-1+which(switchB)[i],-2+which(switchB)[i+1]),
                     log10(fB[seq(which(switchB)[i],-1+which(switchB)[i+1])]),
                     type="l",lwd=6,col=viridis(3)[3])
            } else {
              points(seq(-1+which(switchB)[i],-2+which(switchB)[i+1]),
                     log10(ek*fB[seq(which(switchB)[i],-1+which(switchB)[i+1])]),
                     type="l",lwd=6,col=viridis(3)[3]) 
            }
          } else {
            if(curve=="Allele Frequency"){
              points(seq(-1+which(switchB)[i],oy*og),
                     log10(fB[seq(which(switchB)[i],1+oy*og)]),
                     type="l",lwd=6,col=viridis(3)[3])
            } else {
              points(seq(-1+which(switchB)[i],oy*og),
                     log10(ek*fB[seq(which(switchB)[i],1+oy*og)]),
                     type="l",lwd=6,col=viridis(3)[3]) 
            }
          }
        }
      }
    }
  }
  if(curve=="Resistance Phenotype"){
    plot(c(0,og*oy),rep(-10,2),font.lab=2,las=1,
         cex.lab=1.5,cex.main=1.5,ylim=c(0,1),
         xaxt='n',xaxs="i",yaxs="i",xlab="Time in Years",ylab=yaxislab,main="")
    #paste(opt_name," Optimum: ",paste(round(100*mort_after3,1),collapse="% & "),
    #      "% mortality at 3 years",sep="")
    points(rep(og*opt[1],2),c(0,1),type="l",col="grey")
    axis(1,at=(og*seq(0,oy)),seq(0,oy),las=1)
    points(seq(1,oy*og),wAa,type="l",lwd=6,col=viridis(3)[2])
    points(seq(1,oy*og),wBb,type="l",lwd=6,col=viridis(3)[3])
  }
  if(curve=="Both"){
    legend("topright",c("Population Size",
                        paste("Resistance to",iX_abb[c(which(iA==iX_set),
                                                       which(iB==iX_set))])),
           pch=19,col=viridis(3))
  }
  #if(curve=="Population Size"){
  #  legend("topright",c("Population Size"),pch=19,col=viridis(3)[1])
  #}
  if(curve=="Allele Frequency"|curve=="Resistance Phenotype"){
    legend("topright",paste("Resistance to",iX_abb[c(which(iA==iX_set),
                                                     which(iB==iX_set))]),
           pch=19,col=viridis(3)[2:3])
  }
  
  box()
  
}

# END 