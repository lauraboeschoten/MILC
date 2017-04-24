prep_sim <- function(scen.mat,nsim,nboot,ncomb,brew.file,seed){
  
  #Loop over scenarios  
  for (c in 1:nrow(scen.mat)){
    
    cat(c)
    
    #Set seed
    set.seed(seed)
    
    #start timing
    st.local <- Sys.time()
    
    #Name of data file
    
    setwd("D:\\Surfdrive\\Project 1\\datapackage\\0_createdata")
    
    dat.fil.name <- paste0("dat_",scen.mat[c,1],"_",
                           substring(scen.mat[c,2], 3),"_",
                           substring(scen.mat[c,3], 3),"_",
                           substring(scen.mat[c,4], 3),".txt")
    
    
    samples <- read.delim(dat.fil.name, header=T, sep="\t")
    
    # for brew
    cp.min=log((1-scen.mat[c,2])/scen.mat[c,2])
    cp.plus=log(scen.mat[c,2]/(1-scen.mat[c,2]))
    imp.comp=log((2*scen.mat[c,3])/(1-2*scen.mat[c,3]))
    co.coef=log(scen.mat[c,4]/(1-scen.mat[c,4]))
    
    setwd("D:\\Surfdrive\\Project 1\\datapackage\\3_restricted_conditional_model")
    #run simulation
    sim.res <- simulation(samples=samples,
                          nsim=nsim, 
                          ssize=scen.mat[c,1], 
                          nboot=nboot,  
                          ncomb=ncomb, 
                          brew.file=brew.file,
                          cp.min=cp.min,
                          cp.plus=cp.plus,
                          imp.comp=imp.comp,
                          co.coef=co.coef)
    
    #Assignment name
    sav.name <- paste0("sim_res_",scen.mat[c,1],"_",
                       substring(scen.mat[c,2], 3),"_",
                       substring(scen.mat[c,3], 3),"_",
                       substring(scen.mat[c,4], 3))
    
    
    #Assign sim.res to name
    assign(sav.name,sim.res)
    
    #print runtime to console
    print(Sys.time() - st.local)
    
    #Save results
    sav.name.file <- paste0(sav.name,".Rdata")
    
    save(list=c(sav.name), file = sav.name.file)
    
  }}

