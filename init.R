#init.R

my_packages=c("shiny","shinydashboard","shinythemes","shinyWidgets","highcharter","bslib",
              "thematic","dplyr")

install_if_missing=function(p){
  if(p %in% rownames(install.packages())==FALSE){
    install.packages(p) 
     }
  }

invisible(sapply(my_packages,install_if_missing))