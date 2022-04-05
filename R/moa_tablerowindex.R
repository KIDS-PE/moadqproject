#' moa_tablerowindex function
#'
#' This function allows you to write table row index
#' @param
#' @keywords tablerowindex
#' @return rds file
#' @import DBI
#' @import dplyr
#' @import SqlRender
#' @import foreach
#' @import doParallel
#' @import parallel
#' @import doSNOW
#' @export
#' @examples
#' moa_tablerowindex(x)

moa_tablerowindex<-function(){

###################################
##        Table row index        ##
###################################

  con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))

  mydbtype=tolower(con_info$dbtype)
  myschemaname_lv1=con_info$schemaname_lv1
  myschemaname_lv2=con_info$schemaname_lv2
  myschemanave_vocab=con_info$schemaname_vocab

  sql<-translate('select count(*) from @A."@B"', targetDialect = mydbtype)
  consistency_result<-readRDS(file.path(system.file(package="moadqproject"), 'results/consistency.rds'))
  table_count<-consistency_result%>%filter(rule=='table name consistency')%>%select(c('level', 'table', 'result'))%>%unique()

  iterations<-nrow(table_count); pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)

  n_core<-detectCores(); cl=makeCluster(n_core-1); registerDoSNOW(cl)
  clusterEvalQ(cl, {library(moadqproject); con <- connect_DB(); NULL})

  table_count<-
  foreach(i=c(1:nrow(table_count)), .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI'),
          .noexport="con", .options.snow = opts)%dopar%{
    if(table_count$result[i]==TRUE){
      if(table_count$level[i]==1){schema=myschemaname_lv1}; if(table_count$level[i]==2){schema=myschemaname_lv2}
      tmp1<-dbGetQuery(con, render(sql, A=schema, B=table_count$table[i]))
      count=tmp1$count
    }
    else{count=0}

    cbind(table_count[i,], count)

  }
  close(pb)

  clusterEvalQ(cl, {library(DatabaseConnector); disconnect(con); NULL})

  saveRDS(table_count, file.path(system.file(package="moadqproject"), 'results/table_count.rds'))
  stopCluster(cl)
}
