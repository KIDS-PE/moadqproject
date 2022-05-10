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

  con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))

  if(is.null(con_info$password)==TRUE){con_info$password<-''}
  if(is.null(con_info$host)==TRUE){local_server<-con_info$dbname}
  if(is.null(con_info$dbname)==TRUE){local_server<-con_info$host}
  if(is.null(con_info$host)==FALSE & is.null(con_info$dbname)==FALSE){local_server<-paste0(con_info$host, '/', con_info$dbname)}

  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = con_info$jdbcDrivers)
  connectionDetails <- createConnectionDetails(dbms=con_info$dbtype,
                                               server=local_server,
                                               port=con_info$port,
                                               user=con_info$user,
                                               password=con_info$password)

  con <- connect(connectionDetails)

  return(con)

}





