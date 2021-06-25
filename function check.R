


ROCR_check <- function (){
  l = packageVersion("ROCR")
  if (l[1] != "1.0-7"){
    require(devtools)
    install_version("ROCR", version = "1.0-7", repos = "http://cran.us.r-project.org", upgrade = 'never')
  }
}
   

  
    



