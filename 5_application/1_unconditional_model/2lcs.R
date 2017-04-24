library(brew)

setwd("D:\\Surfdrive\\Project 1\\datapackage\\5_application\\1_unconditional_model")
source("runtemplate.R")

samples            <- read.table("samples.txt", comment.char="", header=TRUE)

outfiles           <- paste0("model.",1:5)
bootstrap.colnames <- tail(colnames(samples),5)

# bootstrap 1
brew.model <- "brew_model1.brew"
envir <- new.env()
run.template(template.path=brew.model, envir=envir, temp.filename.base=outfiles[1], 
               bootstrap.colnames=bootstrap.colnames[1])

# bootstrap 2
brew.model <- "brew_model2.brew"
envir <- new.env()
run.template(template.path=brew.model, envir=envir, temp.filename.base=outfiles[2], 
             bootstrap.colnames=bootstrap.colnames[2])

# bootstrap 3
brew.model <- "brew_model3.brew"
envir <- new.env()
run.template(template.path=brew.model, envir=envir, temp.filename.base=outfiles[3], 
             bootstrap.colnames=bootstrap.colnames[3])

# bootstrap 4
brew.model <- "brew_model4.brew"
envir <- new.env()
run.template(template.path=brew.model, envir=envir, temp.filename.base=outfiles[4], 
             bootstrap.colnames=bootstrap.colnames[4])

# bootstrap 5
brew.model <- "brew_model5.brew"
envir <- new.env()
run.template(template.path=brew.model, envir=envir, temp.filename.base=outfiles[5], 
             bootstrap.colnames=bootstrap.colnames[5])


