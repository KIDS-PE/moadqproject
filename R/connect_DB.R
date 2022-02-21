#' connect_DB function
#'
#' This function allows you to connect R and CDM DB
#' @param
#' @keywords connect
#' @return con object
#' @import DBI
#' @export
#' @examples
#' connect_DB()

connect_DB<-function(){
  con_info<-readRDS('data/result/con_info.RDS')
  drv<-DBI::dbDriver(con_info$dbtype)
  con<-DBI::dbConnect(drv, dbname=con_info$dbname, host=con_info$host, port=con_info$port, user=con_info$user, password=con_info$password)  # input password if needed
  return(con)
}

