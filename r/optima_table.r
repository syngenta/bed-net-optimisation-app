#' Optima table containing all variations of interest 
#' 
#' @param output_list LIST returned output from run_simulation(), which is a 
#' list containing the three-dimensional array (where the three dimensions are 
#' bednet lifespan and chemical loading AI 1 and AI 2) of cumulative vector 
#' population size and all defined and/or useful parameters 
#' 
#' @return a table of optima of all variations of solo and duo insecticide 
#' strategies under Global, WHO, PQL and UC constraints

optima_table <- function(output_list){
  
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
  
  ###
  
  # find optima 
  both_glb = calculate_optimum(output_list,echo=F)
  both_who = calculate_optimum(output_list,opt_name="WHO",echo=F)
  both_pql = calculate_optimum(output_list,opt_name="PQL",echo=F)
  both_uc = calculate_optimum(output_list,opt_name="UC",echo=F)
  solo1_glb = calculate_optimum(output_list,opt_type="Solo AI 1",echo=F)
  solo1_who = calculate_optimum(output_list,opt_name="WHO",opt_type="Solo AI 1",
                                echo=F)
  solo1_pql = calculate_optimum(output_list,opt_name="PQL",opt_type="Solo AI 1",
                                echo=F)
  solo1_uc = calculate_optimum(output_list,opt_name="UC",opt_type="Solo AI 1",
                               echo=F)
  solo2_glb = calculate_optimum(output_list,opt_type="Solo AI 2",echo=F)
  solo2_who = calculate_optimum(output_list,opt_name="WHO",opt_type="Solo AI 2",
                                echo=F)
  solo2_pql = calculate_optimum(output_list,opt_name="PQL",opt_type="Solo AI 2",
                                echo=F)
  solo2_uc = calculate_optimum(output_list,opt_name="UC",opt_type="Solo AI 2",
                               echo=F)
  
  # assemble in a matrix 
  if(os=="Mixture"){
    opt_mat = matrix(0,0,1+6+5)
    colnames(opt_mat) = c("Optimum Type","Dim1","Dim2","Dim3",
                          "Bed-Net Deployment Lifespan",
                          paste("Relative Loading ",iA,sep=""),
                          paste("Relative Loading ",iB,sep=""),
                          "Bed-Net Coverage of Human Population",
                          "Cost per Bed-Net in USD",
                          paste("Average Control over Simulation Timespan (",oy," Years)",sep=""),
                          "Bioefficacy of 3 Year-Old Bed-Net at Simulation Start",
                          paste("Bioefficacy of 3 Year-Old Bed-Net at Simulation End (",oy," Years)",sep=""))
  } else {
    opt_mat = matrix(0,0,1+6+8)
    colnames(opt_mat) = c("Optimum Type","Dim1","Dim2","Dim3",
                          "Bed-Net Deployment Lifespan",
                          paste("Relative Loading ",iA," on Bed-Net 1",sep=""),
                          paste("Relative Loading ",iB," on Bed-Net 2",sep=""),
                          "Bed-Net Coverage of Human Population",
                          "Cost per Bed-Net 1 in USD",
                          "Cost per Bed-Net 2 in USD",
                          paste("Average Control over Simulation Timespan (",oy," Years)",sep=""),
                          "Bioefficacy of 3 Year-Old Bed-Net 1 at Simulation Start",
                          "Bioefficacy of 3 Year-Old Bed-Net 2 at Simulation Start",
                          paste("Bioefficacy of 3 Year-Old Bed-Net 1 at Simulation End (",oy," Years)",sep=""),
                          paste("Bioefficacy of 3 Year-Old Bed-Net 2 at Simulation End (",oy," Years)",sep=""))
  }

  # joint optima 
  opt_mat = rbind(opt_mat,c("Joint BL",which(ob==1),rep(which(oc==1),2),3,1,1,
                            rep(0,ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Joint MC",both_glb[[1]],both_glb[[2]],
                                matrix(0,nrow(both_glb[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Joint CW",both_who[[1]],both_who[[2]],
                                matrix(0,nrow(both_who[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Joint MW",both_pql[[1]],both_pql[[2]],
                                matrix(0,nrow(both_pql[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Joint UC",both_uc[[1]],both_uc[[2]],
                                matrix(0,nrow(both_uc[[1]]),ncol(opt_mat)-7)))
  # solo 1 optima
  opt_mat = rbind(opt_mat,c("Solo 1 BL",which(ob==1),which(oc==1),1,3,1,0,
                            rep(0,ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 1 MC",solo1_glb[[1]],solo1_glb[[2]],
                                matrix(0,nrow(solo1_glb[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 1 CW",solo1_who[[1]],solo1_who[[2]],
                                matrix(0,nrow(solo1_who[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 1 MW",solo1_pql[[1]],solo1_pql[[2]],
                                matrix(0,nrow(solo1_pql[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 1 UC",solo1_uc[[1]],solo1_uc[[2]],
                                matrix(0,nrow(solo1_uc[[1]]),ncol(opt_mat)-7)))
  # solo 2 optima 
  opt_mat = rbind(opt_mat,c("Solo 2 BL",which(ob==1),1,which(oc==1),3,0,1,
                            rep(0,ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 2 MC",solo2_glb[[1]],solo2_glb[[2]],
                                matrix(0,nrow(solo2_glb[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 2 CW",solo2_who[[1]],solo2_who[[2]],
                                matrix(0,nrow(solo2_who[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 2 MW",solo2_pql[[1]],solo2_pql[[2]],
                                matrix(0,nrow(solo2_pql[[1]]),ncol(opt_mat)-7)))
  opt_mat = rbind(opt_mat,cbind("Solo 2 UC",solo2_uc[[1]],solo2_uc[[2]],
                                matrix(0,nrow(solo2_uc[[1]]),ncol(opt_mat)-7)))
  
  # calculate remaining columns 
  for(i in 1:nrow(opt_mat)){
    opt_cont = 100*round(abs(ek*og*oy-
                         output_array[as.numeric(opt_mat[i,2]),
                                      as.numeric(opt_mat[i,3]),
                                      as.numeric(opt_mat[i,4])])/(ek*og*oy),3)
    load1 = log(1/as.numeric(opt_mat[i,6]))/cd[1] # relative AI effect
    load2 = log(1/as.numeric(opt_mat[i,7]))/cd[2] # relative AI effect
    if(os=="Mixture"){
      opt_cove = round(100*(as.numeric(opt_mat[i,5])/3)*xb/
                         (xn+as.numeric(opt_mat[i,6])*xu[1]+
                             as.numeric(opt_mat[i,7])*xu[2]),1)
      opt_cost = round(xn+as.numeric(opt_mat[i,6])*xu[1]+
                          as.numeric(opt_mat[i,7])*xu[2],2)
      mort_after3 = round(100*(1-(1-(mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
                                       (1+exp(md[1]*(3*og+load1)+mh[1]))))*
                                 (1-(mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
                                       (1+exp(md[2]*(3*og+load2)+mh[2]))))),1)
    } else {
      opt_cove = round(100*(as.numeric(opt_mat[i,5])/3)*xb/
                         (xn+0.5*as.numeric(opt_mat[i,6])*xu[1]+
                             0.5*as.numeric(opt_mat[i,7])*xu[2]),1)
      opt_cost = round(c(xn+as.numeric(opt_mat[i,6])*xu[1],
                         xn+as.numeric(opt_mat[i,7])*xu[2]),2)
      mort_after3 = round(100*c(mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
                                  (1+exp(md[1]*(3*og+load1)+mh[1])),
                                mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
                                  (1+exp(md[2]*(3*og+load2)+mh[2]))),1)
    }
    opt_cove[opt_cove>100] = 100
    
    ## calculate bioefficacy at end of simulation 
    opt = as.numeric(opt_mat[i,5:7])
    mr_obs = mr 
    if(sum(is.na(opt))==3){
      if(os=="Mixture"){
        mort_after12 = 0 
      } else {
        mort_after12 = c(0,0) 
      }
    } else {
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
            mr_obs[1] = (1-el)*mr_obs[1]
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
            mr_obs[2] = (1-el)*mr_obs[2]
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
      mr12 = c((1-el*fA[length(fA)])*mr_obs[1],(1-el*fB[length(fB)])*mr_obs[2])
      # calculate bioefficacy 
      load1 = log(1/as.numeric(opt_mat[i,6]))/cd[1] # relative AI effect
      load2 = log(1/as.numeric(opt_mat[i,7]))/cd[2] # relative AI effect
      if(os=="Mixture"){
        mort_after12 = round(100*(1-(1-(mr12[1]*exp(md[1]*(3*og+load1)+mh[1])/
                                          (1+exp(md[1]*(3*og+load1)+mh[1]))))*
                                    (1-(mr12[2]*exp(md[2]*(3*og+load2)+mh[2])/
                                          (1+exp(md[2]*(3*og+load2)+mh[2]))))),1)
      } else {
        mort_after12 = round(100*c(mr12[1]*exp(md[1]*(3*og+load1)+mh[1])/
                                     (1+exp(md[1]*(3*og+load1)+mh[1])),
                                   mr12[2]*exp(md[2]*(3*og+load2)+mh[2])/
                                     (1+exp(md[2]*(3*og+load2)+mh[2]))),1)
      }
    }

    # put into opt_mat 
    if(is.na(opt_cont)){
      if(os=="Mixture"){
        opt_mat[i,8:12] = NA
      } else {
        opt_mat[i,8:15] = NA
      }
    } else {
      if(os=="Mixture"){
        opt_mat[i,8:12] = c(opt_cove,opt_cost,opt_cont,mort_after3,mort_after12)
      } else {
        opt_mat[i,8:15] = c(opt_cove,opt_cost,opt_cont,mort_after3,mort_after12)
      }
      
    }
  }

  # remove uninformative columns 
  opt_mat = opt_mat[,-c(2:4)]

  # hyphen not NA
  opt_mat[is.na(opt_mat)] = "-"
  
  # convert to dataframe 
  opt_tab = as.data.frame(opt_mat)
  
  # return opt_tab 
  return(opt_tab)
  
}

# END