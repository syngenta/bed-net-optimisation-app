#' Runs a simulation to generate an output array of vector control values 
#' 
#' @param os VALUE resistance-management strategy: Mixture, Rotation or Mosaic; 
#' rotations and mosaics assume equal coverage for each bednet  
#' @param og VALUE mosquito generations per year of simulation 
#' @param oy VALUE number of years of simulation; and the maximum deployment 
#' lifespan of a bednet (where the minimum is oi_b)
#' @param ol VALUE the maximum chemical loading of an AI onto a bednet in 
#' relative units
#' @param oi_b VALUE interval in years for the vector of deployment lifespans to 
#' run the optimisation across 
#' @param oi_c VALUE interval in relative units for the vector of chemical 
#' loading to run the optimisation across (for two AIs)
#' @param ou VALUE resistance mutation rate (which determines the time it takes
#' for second, third etc resistance mutations to arise)
#' @param er VALUE mosquito population growth rate (for a logistic model)
#' @param ek VALUE mosquito population carrying capacity (for a logistic model)
#' @param el VALUE level of resistance confered by a resistance mutation, as a 
#' proportional reduction in mortality
#' @param xb VALUE public health budget per household for purchasing bednets 
#' @param xn VALUE base cost in USD for an untreated bednet 
#' @param xs VECTOR proportional subsidy of AI costs for the two insecticides 
#' @param pm VALUE proportional usage of provisioned bednets
#' @param p0 VALUE proportional bednet physical survival on arrival (year 0)
#' @param p3 VALUE proportional bednet physical survival after 3 years 
#' @param iA VALUE name of the first insecticide: Pyrethroid, PBO, 
#' Chlorefenapyr, Pyriproxyfen, VECTA, New AI 1, New AI 2; New AIs can be 
#' renamed elsewhere
#' @param iB VALUE name of the second insecticide: Pyrethroid, PBO, 
#' Chlorefenapyr, Pyriproxyfen, VECTA, New AI 1, New AI 2; New AIs can be 
#' renamed elsewhere
#' @param mr_set VECTOR pre-existing level of resistance to insecticides; 
#' logically involving (1-el)^x for some power x number of previously fixed 
#' mutations for each insecticide
#' @param iX_set VECTOR names of insecticides 
#' @param iX_abb VECTOR 3-letter abbreviated names of insecticides 
#' @param cd_set VECTOR rate for chemical decay of insecticides on bednets 
#' @param md_set VECTOR rate for mortality decay from insecticides on bednets 
#' @param mh_set VECTOR half-life for mortality decay from insecticides on 
#' bednets
#' @param xu_set VECTOR cost in USD per relative unit of AI per insecticide 
#' 
#' @return a list containing the three-dimensional array (where the three 
#' dimensions are bednet lifespan and chemical loading AI 1 and AI 2) output_vc 
#' of the cumulative vector population size and all defined and/or useful 
#' parameters to pass to other functions 

run_simulation <- function(
  
  # optimization setup 
  os = "Mixture", # strategy : "Mixture", "Rotation", "Mosaic"
  og = 12, # generations per year 
  oy = 12, # max years bednet lifespan 
  ol = 4, # max relative chemical loading 
  oi_b = 0.25, # interval of lifespan
  oi_c = 0.2, # interval of loading
  ou = 10^-6, # expected resistance mutation rate 
  
  # ecology  
  er = 1, # population growth rate 
  ek = 10^9, # population carrying capacity 
  el = 0.5, # proportional resistance from mutation NB: 1 = complete resistance 
  
  # economics
  xb = 2, # budget per household per year in dollars 
  xn = 1.8, # cost in dollars per bednet 
  xs = c(0,0), # proportional subsidy of insecticide cost 
  
  # physical
  pm = 0.88, # constant physical survival multiplier
  p0 = 0.995711963, # proportional physical survival after 0 years
  p3 = 0.464204692, # proportional physical survival after 3 years 
  
  # insecticides: named or provide a manual entry in the order: c1, m0, m3, xu
  iA = "Pyrethroid",
  iB = "New AI 1",
  # customize a "new" insecticide to have any set of parameters by editing below 
  
  # insecticide resistance level 
  mr_set = c((1-el)^2,
             1,
             1,
             1,
             1,
             1), # mulitplier of mortality curve
  
  # insecticide names
  iX_set = c("Pyrethroid",
             "PBO",
             "Chlorfenapyr",
             "Pyriproxyfen",
             "New AI 1",
             "New AI 2"), # label for reading in  
  
  # abbreviated named insecticide names
  iX_abb = c("PYR",
             "PBO",
             "CFP",
             "PPF",
             "NW1",
             "NW2"), # label for plotting  
  
  # chemical decay 
  cd_set = c(0.01857947,
             0.01777809,
             0.02035838,
             0.03076537,
             0.02067709,
             0.02067709), # chemical decay rate per AI
  
  # mortality effect  
  md_set = c(-0.02454014,
             -0.062127873,
             -0.013320582,
             -0.20854655,
             -0.0312781,
             -0.0312781), # mortality decay rate per AI
  mh_set = c(2.231073,
             0.364367554,
             -0.092747368,
             1.2101277,
             2.512306,
             2.512306), # mortality half-life per AI
  
  # economic cost  
  xu_set = c(0.20,
             0.50,
             1.30,
             1.90,
             2.50,
             2.50), # cost per unit AI 
  
  token=NULL
){
  
  checkpoints <- c(.2, .4, .6, .8)
  
  ###
  
  # assign insecticide variables 
  # insecticide A
  cd = cd_set[which(iA==iX_set)] # chemical decay rate 
  mr = mr_set[which(iA==iX_set)] # mortality resistance level 
  md = md_set[which(iA==iX_set)] # mortality decay rate
  mh = mh_set[which(iA==iX_set)] # mortality halflife 
  xu = (1-xs[1])*xu_set[which(iA==iX_set)] # cost per unit AI 
  # insecticide B 
  cd = append(cd,cd_set[which(iB==iX_set)]) # chemical decay rate 
  mr = append(mr,mr_set[which(iB==iX_set)]) # mortality resistance level 
  md = append(md,md_set[which(iB==iX_set)]) # mortality decay rate
  mh = append(mh,mh_set[which(iB==iX_set)]) # mortality halflife 
  xu = append(xu,(1-xs[2])*xu_set[which(iB==iX_set)]) # cost per unit AI 
  # assign physical survival variables 
  pd = (log(p3/(1-p3))-log(p0/(1-p0)))/(3*og) # physical decay rate
  ph = log(p0/(1-p0)) # mortality halflife 
  
  # simulation setup NB: baseline lifespan=3, loading=1
  ot = seq(1,oy*og) # 10 years in generations 
  ob = seq(oi_b*og,oy*og,by=oi_b*og)/og # optimize over range of lifespans 
  oc = seq(0,ol,by=oi_c) # optimize over range of loadings 
  # empty tables for data 
  output_vc = array(0,dim=c(length(ob),length(oc),length(oc))) # vector control
  
  ###
  
  for(i in 1:length(ob)){ # bednet lifespan 
    
    # bednet lifespan derivative 
    cyct = 1+(ot-1)%%(og*ob[i]) # bednet lifespan cycle time 
    
    # bednet usage derivative 
    use1 = rep(1,oy*og) # Mixtures
    use2 = use1
    if(os=="Mosaic"){
      use1 = rep(0.5,oy*og)
      use2 = use1
    }
    if(os=="Rotation"){
      use1 = rep(c(rep(1,og*ob[i]),rep(0,og*ob[i])),
                 ceiling(0.5*oy/ob[i]))[1:(oy*og)]
      use2 = (!use1)+0
    }
    
    # physical derivative 
    phys = pm*exp(pd*cyct+ph)/(1+exp(pd*cyct+ph)) # physical survival 
    
    for(j in 1:length(oc)){ # concentration of first AI 
      
      # chemical and mortality derivatives  
      
      if(oc[j]==0){
        mort1 = rep(0,length(cyct)) # mortality 
      } else {
        load1 = log(1/oc[j])/cd[1] # temporal displacement from AI 1 loading 
        mort1 = mr[1]*exp(md[1]*(cyct+load1)+mh[1])/
          (1+exp(md[1]*(cyct+load1)+mh[1])) # mortality AI 1 
      }
      
      for(k in 1:length(oc)){ # concentration of second AI 
        
        if(oc[k]==0){
          mort2 = rep(0,length(cyct))
        } else {
          load2 = log(1/oc[k])/cd[2] # temporal displacement from AI 2 loading 
          mort2 = mr[2]*exp(md[2]*(cyct+load2)+mh[2])/
            (1+exp(md[2]*(cyct+load2)+mh[2])) # mortality AI 2
        }
        
        # coverage given bednet and AI cost
        cover = (ob[i]/3)*xb/(xn+oc[j]*xu[1]+oc[k]*xu[2])
        if(os!="Mixture"){
          cover = (ob[i]/3)*xb/(xn+0.5*oc[j]*xu[1]+0.5*oc[k]*xu[2])
        }
        cover[cover>1] = 1
        
        # fitness 
        if(os!="Mosaic"){
          wAB = 1-cover*phys + 
            cover*phys*(1-(1-el)*use1*mort1)*(1-(1-el)*use2*mort2)
          wAb = 1-cover*phys + cover*phys*(1-(1-el)*use1*mort1)*(1-use2*mort2)
          waB = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-(1-el)*use2*mort2)
          wab = 1-cover*phys + cover*phys*(1-use1*mort1)*(1-use2*mort2)
        }
        if(os=="Mosaic"){
          wAB = 1-cover*phys + cover*phys*(1-0.5*(1-el)*(mort1+mort2))
          wAb = 1-cover*phys + cover*phys*(1-0.5*((1-el)*mort1+mort2))
          waB = 1-cover*phys + cover*phys*(1-0.5*(mort1+(1-el)*mort2))
          wab = 1-cover*phys + cover*phys*(1-0.5*(mort1+mort2))
        }
        
        # setup 
        fA = 1/ek
        fB = 1/ek
        n = ek
        tA = 0
        tB = 0 
        m1 = mort1
        m2 = mort2
        
        # loops 
        for(tt in ot){ # generations 
          # mean fitness
          wAaBb = fA[tt]*fB[tt]*wAB[tt]+fA[tt]*(1-fB[tt])*wAb[tt]+
            (1-fA[tt])*fB[tt]*waB[tt]+(1-fA[tt])*(1-fB[tt])*wab[tt]
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
              fA[tt+1] = 1/n[tt+1] 
              m1 = (1-el)*m1
              # fitness 
              if(os!="Mosaic"){
                wAB = 1-cover*phys + cover*phys*(1-(1-el)*use1*m1)*(1-(1-el)*use2*m2)
                wAb = 1-cover*phys + cover*phys*(1-(1-el)*use1*m1)*(1-use2*m2)
                waB = 1-cover*phys + cover*phys*(1-use1*m1)*(1-(1-el)*use2*m2)
                wab = 1-cover*phys + cover*phys*(1-use1*m1)*(1-use2*m2)
              }
              if(os=="Mosaic"){
                wAB = 1-cover*phys + cover*phys*(1-0.5*(1-el)*(m1+m2))
                wAb = 1-cover*phys + cover*phys*(1-0.5*((1-el)*m1+m2))
                waB = 1-cover*phys + cover*phys*(1-0.5*(m1+(1-el)*m2))
                wab = 1-cover*phys + cover*phys*(1-0.5*(m1+m2))
              }
            }
          }
          if(fB[tt+1]>0.99){
            tB = tB + n[tt+1]
            if(tB>(1/ou)){
              tB = 0 
              fB[tt+1] = 1/n[tt+1] 
              m2 = (1-el)*m2
              # fitness 
              if(os!="Mosaic"){
                wAB = 1-cover*phys + cover*phys*(1-(1-el)*use1*m1)*(1-(1-el)*use2*m2)
                wAb = 1-cover*phys + cover*phys*(1-(1-el)*use1*m1)*(1-use2*m2)
                waB = 1-cover*phys + cover*phys*(1-use1*m1)*(1-(1-el)*use2*m2)
                wab = 1-cover*phys + cover*phys*(1-use1*m1)*(1-use2*m2)
              }
              if(os=="Mosaic"){
                wAB = 1-cover*phys + cover*phys*(1-0.5*(1-el)*(m1+m2))
                wAb = 1-cover*phys + cover*phys*(1-0.5*((1-el)*m1+m2))
                waB = 1-cover*phys + cover*phys*(1-0.5*(m1+(1-el)*m2))
                wab = 1-cover*phys + cover*phys*(1-0.5*(m1+m2))
              }
            }
          }
        }
        
        # outputs 
        output_vc[i,j,k] = sum(n[2:max(ot)])
        
      }
    }
    if (!is.null(token) && length(checkpoints)) {
      progress <- i/length(ob)
      if (progress > checkpoints[1]){
        saveRDS(checkpoints[1] + .1, file.path('tmp', token))
        checkpoints <- checkpoints[-1]
      }
    }
    if (is.null(token)) {
      print(paste("Simulation is ",round(100*i/length(ob),1),"% complete",
                  sep=""))
    }
  }
  
  if (!is.null(token)) file.remove(file.path('tmp', token))
  
  # return output array of vector control values 
  return(list(
    output_vc=output_vc,
    os=os,
    og=og,
    oy=oy,
    ol=ol,
    oi_b=oi_b,
    oi_c=oi_c,
    ou=ou,
    er=er,
    ek=ek,
    el=el,
    xb=xb,
    xn=xn,
    xs=xs,
    pm=pm,
    p0=p0,
    p3=p3,
    iA=iA,
    iB=iB,
    mr_set=mr_set,
    iX_set=iX_set,
    iX_abb=iX_abb,
    cd_set=cd_set,
    md_set=md_set,
    mh_set=mh_set,
    xu_set=xu_set,
    cd=cd,
    mr=mr,
    md=md,
    mh=mh,
    xu=xu,
    pd=pd,
    ph=ph,
    ot=ot,
    ob=ob,
    oc=oc
  ))
  
}

# END 