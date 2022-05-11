#' Plots the landscape around a chosen bednet product, showing the extent that 
#' the chosen bednet is optimal in comparison to other similar bednet products 
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
#' @param fixed_dim VALUE specifies a dimension of the array in output_list to 
#' hold constant, whilst plotting variation across the other two dimensions:
#'    1: bednet lifespan
#'    2: chemical loading of AI 1
#'    3: chemical loading of AI 2
#' @param z_var VALUE specifies the variable for the z-colour dimension:
#'    "vc": cumulative vector population size (from output_list)
#'    "cv": proportional level of coverage (between 0 and 1)
#'    "bc": bednet cost in USD (starting from xn)
#'    
#' @return a plot showing the landscape of the optimality of a chosen bednet 
#' product in comparison to other similar bednet products, keeping one dimension 
#' fixed and exploring the landscape across two dimensions for a selected z 
#' variable

plot_optimality_landscape <- function(output_list,optimum_list=NA,
                                      select_opt=1,fixed_dim=1,z_var="vc"){

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
    opt0 = c(which(ob==3),which(oc==1),which(oc==1))
    opt = c(3,1,1)
    opt_name = "Baseline"
    optimum_list = list(matrix(opt,1,3),matrix(opt,1,3))
  } else {
    opt0 = optimum_list[[1]][select_opt,]
    opt = optimum_list[[2]][select_opt,]
    opt_name = optimum_list[[3]]
  }
  
  # keep one dimension fixed and find input matrix across other dimensions 
  if(fixed_dim==1){
    xaxis = "loading"
    xlab_text = paste("Relative Loading",iX_abb[which(iA==iX_set)])
    ylab_text = paste("Relative Loading",iX_abb[which(iB==iX_set)])
    # calcualte where add NAs for optimal values that are invalid 
    if(opt_name!="Global"){
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
    }
    if(z_var=="vc"){
      input_matrix = output_array[opt0[1],,]
    }
    if(z_var=="cv"){
      if(os=="Mixture"){
        input_matrix = ((opt[1]/3)*xb)/(xn+outer(oc*xu[1],oc*xu[2],"+"))  
      } else {
        input_matrix = ((opt[1]/3)*xb)/(xn+outer(0.5*oc*xu[1],0.5*oc*xu[2],"+"))  
      }
    }
    if(z_var=="bc"){
      if(os=="Mixture"){
        input_matrix = xn+outer(oc*xu[1],oc*xu[2],"+")  
      } else {
        input_matrix = xn+outer(0.5*oc*xu[1],0.5*oc*xu[2],"+") 
      }
    }
    # actually add NAs for optimal values that are invalid 
    if(opt_name=="WHO"|opt_name=="PQL"){
      input_matrix = mort12*input_matrix # combined mortality >80%
      input_matrix[input_matrix==0] = NA
    }
    if(opt_name=="UC"){
      input_matrix = (cost12<=xb)*input_matrix
      input_matrix[input_matrix==0] = NA
    }
  }
  if(fixed_dim==2){
    xaxis = "lifespan"
    xlab_text = "Bed-Net Lifespan in Years"
    ylab_text = paste("Relative Loading",iX_abb[which(iB==iX_set)])
    # calculate where add NAs for optimal values that are invalid 
    if(opt_name!="Global"){
      load1 = rep(log(1/opt[2])/cd[1],length(ob)) # relative AI effect
      mort1 = mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
        (1+exp(md[1]*(3*og+load1)+mh[1])) # mortality 
      load2 = log(1/oc)/cd[2] # relative AI effect
      mort2 = mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
        (1+exp(md[2]*(3*og+load2)+mh[2])) # mortality 
      if(os=="Mixture"){ # combined mortality of AI 1 and 2 >80%  
        mort12 = (1-outer(1-mort1,1-mort2,"*"))>0.8
        cost12 = xn+outer(rep(opt[2]*xu[1],length(ob)),oc*xu[2],"+") # price 
      } else { # independent mortality of AI 1 and 2 >80%
        mort12 = outer(mort1>0.8,mort2>0.8,"*")
        cost12 = xn+outer(0.5*rep(opt[2]*xu[1],length(ob)),0.5*oc*xu[2],"+")
      }
    }
    if(z_var=="vc"){
      input_matrix = output_array[,opt0[2],]
    }
    if(z_var=="cv"){
      if(os=="Mixture"){
        input_matrix = outer(ob/3*xb,xn+opt[2]*xu[1]+oc*xu[2],"/")
      } else {
        input_matrix = outer(ob/3*xb,xn+0.5*opt[2]*xu[1]+.5*oc*xu[2],"/")
      }
    }
    if(z_var=="bc"){
      if(os=="Mixture"){
        input_matrix = xn+outer(rep(opt[2]*xu[1],length(ob)),oc*xu[2],"+")
      } else {
        input_matrix = xn+outer(0.5*rep(opt[2]*xu[1],length(ob)),
                                0.5*oc*xu[2],"+")
      }
    }
    # actually add NAs for optimal values that are invalid 
    if(opt_name=="WHO"|opt_name=="PQL"){
      input_matrix = mort12*input_matrix # combined mortality >80%
      input_matrix[input_matrix==0] = NA
    }
    if(opt_name=="UC"){
      input_matrix = (cost12<=xb)*input_matrix
      input_matrix[input_matrix==0] = NA
    }
  }
  if(fixed_dim==3){
    xaxis = "lifespan"
    xlab_text = "Bed-Net Lifespan in Years"
    ylab_text = paste("Relative Loading",iX_abb[which(iA==iX_set)])
    # calculate where add NAs for optimal values that are invalid 
    if(opt_name!="Global"){
      load1 = log(1/oc)/cd[1] # relative AI effect
      mort1 = mr[1]*exp(md[1]*(3*og+load1)+mh[1])/
        (1+exp(md[1]*(3*og+load1)+mh[1])) # mortality 
      load2 = rep(log(1/opt[3])/cd[2],length(ob)) # relative AI effect
      mort2 = mr[2]*exp(md[2]*(3*og+load2)+mh[2])/
        (1+exp(md[2]*(3*og+load2)+mh[2])) # mortality 
      if(os=="Mixture"){ # combined mortality of AI 1 and 2 >80%  
        mort21 = (1-outer(1-mort2,1-mort1,"*"))>0.8
        cost21 = xn+outer(rep(opt[3]*xu[2],length(ob)),oc*xu[1],"+") # price 
      } else { # independent mortality of AI 1 and 2 >80%
        mort21 = outer(mort2>0.8,mort1>0.8,"*")
        cost21 = xn+outer(0.5*rep(opt[3]*xu[2],length(ob)),0.5*oc*xu[1],"+")
      }
    }
    if(z_var=="vc"){
      input_matrix = output_array[,,opt0[3]]
    }
    if(z_var=="cv"){
      if(os=="Mixture"){
        input_matrix = outer(ob/3*xb,xn+oc*xu[1]+opt[3]*xu[2],"/") 
      } else {
        input_matrix = outer(ob/3*xb,xn+0.5*oc*xu[1]+0.5*opt[3]*xu[2],"/")
      }
    }
    if(z_var=="bc"){
      if(os=="Mixture"){
        input_matrix = xn+outer(rep(opt[3]*xu[2],length(ob)),oc*xu[1],"+")
      } else {
        input_matrix = xn+outer(0.5*rep(opt[3]*xu[2],length(ob)),
                                0.5*oc*xu[1],"+")
      }
    }  
    # actually add NAs for optimal values that are invalid 
    if(opt_name=="WHO"|opt_name=="PQL"){
      input_matrix = mort21*input_matrix # combined mortality >80%
      input_matrix[input_matrix==0] = NA
    }
    if(opt_name=="UC"){
      input_matrix = (cost21<=xb)*input_matrix
      input_matrix[input_matrix==0] = NA
    }
  }
  
  #main_title = paste(opt_name,ifelse(opt_name=="Baseline",": "," Optimum: "),
  #                   opt[fixed_dim]," ",ifelse(fixed_dim==1,"Year Lifespan",
  #                          paste("x Loading ",ifelse(fixed_dim==2,
  #                                iX_abb[which(iA==iX_set)],
  #                                iX_abb[which(iB==iX_set)]),sep="")),sep="")
  main_title=""
  
  if(z_var=="cv"){
    input_matrix[input_matrix>1] = 1
    opt_col = "black"
    opt_zlim = c(0,1)
  } else {
    opt_col = "white"
    if(z_var=="vc"){opt_zlim = c(min(input_matrix,na.rm=T),og*oy*ek)}
    if(z_var=="bc"){opt_zlim = c(xn,max(input_matrix,na.rm=T))}
  }
  if(fixed_dim!=1){input_matrix=rbind(rep(NA,length(oc)),input_matrix)}
  
  if(length(input_matrix)!=sum(is.na(input_matrix))){
    
    image(input_matrix,col=viridis(100),xaxt="n",yaxt="n",las=1,xlab=xlab_text,
          ylab=ylab_text,main=main_title,font.lab=2,cex.lab=1.5,cex.main=1.5,
          zlim=opt_zlim)
    contour(input_matrix,zlim=opt_zlim,nlevels=9,col=opt_col,drawlabels=F,add=T)
    if(xaxis=="lifespan"){
      axis(1,at=seq(0,1,length.out=7),
           labels=max(oy)*seq(0,1,length.out=7),las=1)
      if(fixed_dim==2){
        for(ii in 1:nrow(optimum_list[[2]])){
          points(optimum_list[[2]][ii,1]/max(ob),
                 optimum_list[[2]][ii,3]/max(oc),pch=19,col=opt_col)
        }
      }
      if(fixed_dim==3){
        for(ii in 1:nrow(optimum_list[[2]])){
          points(optimum_list[[2]][ii,1]/max(ob),
                 optimum_list[[2]][ii,2]/max(oc),pch=19,col=opt_col)
        }
      }
    }
    if(xaxis=="loading"){
      axis(1,at=seq(0,1,length.out=9),
           labels=max(oc)*seq(0,1,length.out=9),las=1)
      for(ii in 1:nrow(optimum_list[[2]])){
        points(optimum_list[[2]][ii,2]/max(oc),
               optimum_list[[2]][ii,3]/max(oc),pch=19,col=opt_col)
      }
    }
    axis(2,at=seq(0,1,length.out=9),labels=max(oc)*seq(0,1,length.out=9),las=1)
    box()
    
  } else {
    
    plot(-10,-10,type="l",lwd=6,font.lab=2,las=1,cex.lab=1.5,cex.main=1.5,
         xlim=c(0,1),ylim=c(0,1),xaxt='n',yaxt='n',xaxs="i",yaxs="i",
         xlab=xlab_text,ylab=ylab_text,main=main_title)
    if(xaxis=="lifespan"){axis(1,at=seq(0,1,length.out=7),
                               labels=max(oy)*seq(0,1,length.out=7),las=1)}
    if(xaxis=="loading"){axis(1,at=seq(0,1,length.out=9),
                              labels=max(oc)*seq(0,1,length.out=9),las=1)}
    axis(2,at=seq(0,1,length.out=9),labels=max(oc)*seq(0,1,length.out=9),las=1)
    
  }
  
}

# END 