#' Finds the optimum from a simulation run 
#' 
#' @param output_list LIST returned output from run_simulation(), which is a 
#' list containing the three-dimensional array (where the three dimensions are 
#' bednet lifespan and chemical loading AI 1 and AI 2) of cumulative vector 
#' population size and all defined and/or useful parameters 
#' @param opt_name VALUE specifies some constraints to apply to output_list 
#' before searching for the optimum: 
#'   "Global": the smallest cumulative vector population size in output_list 
#'   "WHO": the incentivised bednet resulting from WHO commoditisation; the 
#'   cheapest bednet that provides >80% mortality after 3 years of deployment 
#'   "PQL": the smallest cumulative vector population size in output_list that 
#'   would be accepted onto the WHO prequalification list for providing >80% 
#'   mortality after 3 years of deployment 
#'   "UC": the smallest cumulative vector population size in output_list that 
#'   also is cheap enough to be deployed with universal coverage 
#' @param opt_type VALUE specifies some constraints to apply to output_list 
#' before searching for the optimum: 
#'   "Both AIs": finds the optimum across chemical loadings of both AIs 
#'   "Solo AI 1": finds the optimum across chemical loadings of AI 1 only 
#'   "Solo AI 2": finds the optimum across chemical loadings of AI 2 only 
#' @param echo BOOLEAN specifies whether the calculated optimum should print-out
#' 
#' @return a list containing the optimum as a vector in element-index and 
#' interpreted forms, followed by the input for opt_name

calculate_optimum <- function(output_list,opt_name="Global",
                              opt_type="Both AIs",echo=T){
  
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
  
  if(opt_type=="Solo AI 1"){
    output_array[,,-1] = NA
  }
  if(opt_type=="Solo AI 2"){
    output_array[,-1,] = NA
  }
  
  if(opt_name=="Global"){
    # find minimum population size 
    output = which(apply(output_array, c(1,2,3), function(x){
      isTRUE(all.equal(x,min(output_array,na.rm=T)))}),arr.ind=TRUE)
  }
  if(opt_name!="Global"){
    # modify output_array to only show bednets that fit WHO requirements 
    mod_output_array = output_array[which(ob==3),,] # 3 year lifespan 
    load1 = log(1/oc)/cd[1] # relative AI effect
    mort1 = mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
      (1+exp(md[1]*(3*og+load1)+mh[1])) # mortality 
    load2 = log(1/oc)/cd[2] # relative AI effect
    mort2 = mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
      (1+exp(md[2]*(3*og+load2)+mh[2])) # mortality
    if(os=="Mixture"){ # combined mortality of AI 1 and 2 >80%  
      mort12 = (1-outer(1-mort1,1-mort2,"*"))>0.8
      cost12 = xn+outer(oc*xu[1],oc*xu[2],"+")
    } else { # independent mortality of AI 1 and 2 >80%
      mort12 = outer(mort1>0.8,mort2>0.8,"*")
      cost12 = xn+outer(0.5*oc*xu[1],0.5*oc*xu[2],"+")
    }
    mort12[mort12==0] = NA
    # WHO cheapest commodity vs PQL vector control vs universal coverage 
    if(opt_name=="WHO"){ # cheapest that fits requirements 
      mod_output_array = mort12*(!is.na(mod_output_array))
      mod_output_array[mod_output_array==0] = NA
      # find minimum bednet cost  
      if(sum(is.na(mod_output_array))==length(mod_output_array)){
        output = matrix(NA,1,3)
      } else {
        output = cbind(which(ob==3),
                       which(apply(mod_output_array*cost12, c(1,2), function(x){
                         isTRUE(all.equal(x,min(mod_output_array*cost12,
                                                na.rm=T)))}),arr.ind=TRUE))  
      }
    }
    if(opt_name=="PQL"){ # mortality >80% after 3 years 
      # modify output_array to only show bednets that fit WHO requirements 
      mod_output_array = mort12*as.numeric(mod_output_array)
      mod_output_array[mod_output_array==0] = NA
      # find minimum population size 
      if(sum(is.na(mod_output_array))==length(mod_output_array)){
        output = matrix(NA,1,3)
      } else {
        output = cbind(which(ob==3),
                       which(apply(mod_output_array, c(1,2), function(x){
                         isTRUE(all.equal(x,min(mod_output_array,na.rm=T)))}),
                             arr.ind=TRUE))
      }
    }
    if(opt_name=="UC"){
      mod_output_array = (cost12<=xb)*mod_output_array
      mod_output_array[mod_output_array==0] = NA
      # find minimum bednet cost  
      if(sum(is.na(mod_output_array))==length(mod_output_array)){
        output = matrix(NA,1,3)
      } else {
        output = cbind(which(ob==3),
                       which(apply(mod_output_array, c(1,2), function(x){
                         isTRUE(all.equal(x,min(mod_output_array,na.rm=T)))}),
                             arr.ind=TRUE))
      }
    }
  }
  
  # interpret optimum/optima 
  if(sum(is.na(output))!=length(output)){
    for(ii in 1:nrow(output)){
      opt_kill = 100*abs(ek*og*oy-output_array[output[ii,1],
                                               output[ii,2],
                                               output[ii,3]])/(ek*og*oy)
      load1 = log(1/oc[output[ii,2]])/cd[1] # relative AI effect
      load2 = log(1/oc[output[ii,3]])/cd[2] # relative AI effect
      if(os=="Mixture"){
        opt_cove = round(100*(ob[output[ii,1]]/3)*xb/
                           (xn+oc[output[ii,2]]*xu[1]+oc[output[ii,3]]*xu[2]),1)
        opt_cost = round(xn+oc[output[ii,2]]*xu[1]+
                           oc[output[ii,3]]*xu[2],2)
        mort_after3 = round(100*(1-(1-(mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
                                         (1+exp(md[1]*(3*og+load1)+mh[1]))))*
                                   (1-(mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
                                         (1+exp(md[2]*(3*og+load2)+mh[2]))))),1)
      } else {
        opt_cove = round(100*(ob[output[ii,1]]/3)*xb/
                           (xn+0.5*oc[output[ii,2]]*xu[1]+
                              0.5*oc[output[ii,3]]*xu[2]),1)
        opt_cost = round(c(xn+oc[output[ii,2]]*xu[1],
                           xn+oc[output[ii,3]]*xu[2]),2)
        mort_after3 = round(100*c(mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
                                    (1+exp(md[1]*(3*og+load1)+mh[1])),
                                  mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
                                    (1+exp(md[2]*(3*og+load2)+mh[2]))),1)
      }
      opt_cove[opt_cove>100] = 100
      # print-out optimum meaning 
      if(echo==T){
        cat(paste("\n",opt_name," Optimum: \n ",ob[output[ii,1]],
                  " years;\n ",oc[output[ii,2]],
                  "x baseline concentration for ",
                  paste(iX_set[1:6],
                        " (",iX_abb[1:6],")",sep="")[which(iA==iX_set)],
                  ";\n ",oc[output[ii,3]],"x baseline concentration for ",
                  paste(iX_set[1:6],
                        " (",iX_abb[1:6],")",sep="")[which(iB==iX_set)],
                  ";\n ",opt_cove,"% coverage;\n ",
                  "$",paste(opt_cost,collapse=" & $")," per bednet; \n ",
                  round(opt_kill,1),"% avg. control; \n ",
                  paste(mort_after3,collapse="% & "),
                  "% mortality at 3 years \n \n",sep=""))
      }
    }
    # return optimum list  
    return(list(output,
                cbind(ob[output[,1]],oc[output[,2]],oc[output[,3]]),
                opt_name))
  } else {
    # return optimum list  
    return(list(output,
                output,
                opt_name))
  }
}

# END 