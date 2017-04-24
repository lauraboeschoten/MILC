inp.make <- function(scen.mat,nsim){
  
  #Loop over scenarios
  for(i in 1:nrow(scen.mat)){
    
    cat(i)
    
    #Name SS input file for LG  
    cre.name <- paste0("create_ss",scen.mat$ss[i],".txt")
    
    #Content of text file
    cre.con <- c("gender1 gender2 gender3 sport pregnant freq",
                 paste("0 0 0 0 0",scen.mat$ss[i]))
    
    #Save text file to disc
    cre.file <- file(cre.name)
    writeLines(cre.con, cre.file)
    close(cre.file)  
    
    ## Make LG syntax
    
    #Name LG syntax file
    lg.name  <- paste0("LG_",scen.mat[i,1],"_",
                       substring(scen.mat[i,2], 3),"_",
                       substring(scen.mat[i,3], 3),"_",
                       substring(scen.mat[i,4], 3),".lgs")
    
    dat.name <- paste0("dat_",scen.mat[i,1],"_",
                       substring(scen.mat[i,2], 3),"_",
                       substring(scen.mat[i,3], 3),"_",
                       substring(scen.mat[i,4], 3),".txt")
    
    in.file  <- paste0("D:\\Surfdrive\\Project 1\\datapackage\\createdata\\",cre.name)
    out.file <- paste0("D:\\Surfdrive\\Project 1\\datapackage\\createdata\\",dat.name)
    
    cp.min   =log((1-scen.mat[i,2])/scen.mat[i,2])
    cp.plus  =log(scen.mat[i,2]/(1-scen.mat[i,2]))
    imp.comp =log((2*scen.mat[i,3])/(1-2*scen.mat[i,3]))
    co.coef  =log(scen.mat[i,4]/(1-scen.mat[i,4]))
    
    envir       <- new.env()
    run.template_T("brew_data.brew",envir,lg.name,in.file,out.file,nsim,cp.min,
                   cp.plus,imp.comp,co.coef)
    
  }}