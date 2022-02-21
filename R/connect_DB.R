#' connect_DB function
#'
#' This function allows you to connect R and CDM DB
#' @param
#' @keywords connect
#' @return con object
#' @import DatabaseConnector
#' @export
#' @examples
#' connect_DB()

connect_DB<-function(){
  con_info<-readRDS('data/result/con_info.RDS')
  if(is.null(con_info$password)==TRUE){con_info$password<-''}
  connectionDetails <- createConnectionDetails(dbms=con_info$dbtype,
                                               server=paste0(con_info$host, '/', con_info$dbname),
                                               port=con_info$port,
                                               user=con_info$user,
                                               password=con_info$password)

  con <- connect(connectionDetails)

  return(con)

}





