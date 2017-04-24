prep_sim <- function(scen.mat,nsim,nboot,ncomb,brew.file,seed){
  
  #Loop over scenarios  
  for (c in 1:nrow(scen.mat)){
    
    cat(c)
    
    #Set seed
    set.seed(seed)
    
    #start timing
    st.local <- Sys.time()
    
    #Name of data file
    
    setwd("F:\\SurfDrive_Laura\\Project 1\\simulation\\sim_jan_nboot\\nboot_40\\createdata")
    
    dat.fil.name <- paste0("dat_",scen.mat[c,1],"_",
                           substring(scen.mat[c,2], 3),"_",
                           substring(scen.mat[c,3], 3),".txt")
    
    
    samples <- read.delim(dat.fil.name, header=T, sep="\t")
    
    # for brew
    cp.min=log((1-scen.mat[c,2])/scen.mat[c,2])
    cp.plus=log(scen.mat[c,2]/(1-scen.mat[c,2]))
    imp.comp=log((2*scen.mat[c,3])/(1-2*scen.mat[c,3]))
    
    setwd("F:\\SurfDrive_Laura\\Project 1\\simulation\\sim_jan_nboot\\nboot_40\\model3")  
    #run simulation
    sim.res <- simulation(samples=samples,
                          nsim=nsim, 
                          ssize=scen.mat[c,1], 
                          nboot=nboot,  
                          ncomb=ncomb, 
                          brew.file=brew.file,
                          cp.min=cp.min,
                          cp.plus=cp.plus,
                          imp.comp=imp.comp)
    #Assignment name
    sav.name <- paste0("sim_res_",scen.mat[c,1],"_",
                       substring(scen.mat[c,2], 3),"_",
                       substring(scen.mat[c,3], 3))
    
    
    #Assign sim.res to name
    assign(sav.name,sim.res)
    
    #print runtime to console
    print(Sys.time() - st.local)
    
    #Save results
    sav.name.file <- paste0(sav.name,".Rdata")
    
    save(list=c(sav.name), file = sav.name.file)
    
  }}

