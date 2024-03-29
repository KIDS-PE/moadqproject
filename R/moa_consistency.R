#' moa_consistency function
#'
#' This function allows you to perform consistency check
#' @param
#' @keywords consistency
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import SqlRender
#' @import tcltk
#' @import foreach
#' @import doParallel
#' @import parallel
#' @import doSNOW
#' @export
#' @examples
#' moa_consistency(x)

moa_consistency<-function(){

  con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))

  mydbtype=tolower(con_info$dbtype)
  myschemaname_lv1=con_info$schemaname_lv1
  myschemaname_lv2=con_info$schemaname_lv2
  myvocabschemaname=con_info$schemaname_vocab

  n_core<-detectCores(); cl=makeCluster(n_core-1); registerDoSNOW(cl)
  clusterEvalQ(cl, { library(moadqproject); con <- connect_DB(); NULL })

  # table name consistency
  i1=(consistency_rule%>%filter(rule=='table name consistency'))$rule_id

  iterations<-length(i1); pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)

  round1<-foreach(i=i1, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI'),
                  .noexport="con", .options.snow = opts)%dopar%{
    sql<-translate("select table_name from information_schema.tables where table_schema='@A'", targetDialect = mydbtype)
    tmp1<-which(consistency_rule$rule_id==i); tmp2<-consistency_rule[tmp1,]
    if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
    tmp3<-dbGetQuery(con, render(sql, A=schema))
    result<-(tmp2$ref)%in%tmp3$table_name==TRUE
    cbind(tmp2, result)
    }

  close(pb)

  # field name consistency
  i2=(consistency_rule%>%filter(rule=='field name consistency'))$rule_id

  iterations<-length(i2); pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)

  round2<-foreach(i=i2, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI', 'tcltk'), .noexport="con", .options.snow = opts)%dopar%{
    sql<-translate("select column_name from information_schema.columns where table_schema='@A' and table_name='@B'", targetDialect=mydbtype)
    tmp1<-which(consistency_rule$rule_id==i); tmp2<-consistency_rule[tmp1,]
    if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
    tmp3<-dbGetQuery(con, render(sql, A=schema, B=tmp2$table))
    result<-(tmp2$ref%in%tmp3$column_name==TRUE)
    cbind(tmp2, result)
  }
  close(pb)

  # field type consistency

  i3=(consistency_rule%>%filter(rule=='field type consistency'))$rule_id

  iterations<-length(i3); pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)

  round3<-foreach(i=i3, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI', 'tcltk', 'moadqproject'), .noexport="con", .options.snow = opts)%dopar%{
    sql<-translate("select data_type from information_schema.columns where table_schema='@A' and table_name='@B' and column_name='@C'", targetDialect = mydbtype)
    tmp1<-which(consistency_rule$rule_id==i); tmp2<-consistency_rule[tmp1,]
    if((round2%>%filter(rule=='field name consistency' & level==tmp2$level & table==tmp2$table & field==tmp2$field))$result==TRUE){
      if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
      tmp3<-dbGetQuery(con, render(sql, A=schema, B=tmp2$table, C=tmp2$field)); tmp3$type<-''
      for(j in c(1:nrow(search_type))){tmp3$type[grep(paste0(search_type$type_detail[j]), tmp3$data_type)]<-search_type$type[j]}
      result<-(tmp2$ref==tmp3$type)
    }
    else{result<-NA}
    cbind(tmp2, result)
  }

  close(pb)

  clusterEvalQ(cl, {library(DatabaseConnector); disconnect(con); NULL})

  consistency_result<-rbind(round1, round2, round3)
  saveRDS(consistency_result, file.path(system.file(package="moadqproject"), 'results/consistency.rds'))

  progress<-read.table(file.path(system.file(package='moadqproject'), 'results/progress.txt'), header=TRUE)
  progress$status[which(progress$rule=="Consistency")]<-TRUE
  write.table(progress, file.path(system.file(package='moadqproject'), 'results/progress.txt'), row.names = FALSE)

  consistency_score<-aggregate(result~level+table, consistency_result, FUN=mean)
  names(consistency_score)<-c('level', 'table', 'consistency')
  saveRDS(consistency_score, file.path(system.file(package='moadqproject'), 'results/consistency_score.rds'))

  stopCluster(cl)

}
