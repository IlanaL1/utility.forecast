# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# documenting packages
#' utility functions
#' 
#' utility functions are those used by execute_forecast They therefore must be 
#' loaded by the R server within the procedure that does the forecasting. In 
#' contrast, new sql procedures, are run as data pre-processing or checking and 
#' can be called prior to calling the execute forecast function. They return 
#' diagnostic information that can be used - by the main application to 
#' communicate issues to the user, or - set flags as an input parameter to the 
#' forecast functions
"_PACKAGE"
#> [1] "_PACKAGE"