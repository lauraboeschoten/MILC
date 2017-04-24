run.template_T <- function(template.path,envir,temp.filename.base,in.file,out.file,
                           nsim,cp.min,cp.plus,imp.comp,co.coef) {
  
  temp.filename <- temp.filename.base                         
  template      <- file(template.path, 'r')
  brew(template, output=temp.filename) 
  close(template)
  shell(sprintf('"C:\\Program Files\\LatentGOLD5.0\\lg50.exe" %s', temp.filename))
}