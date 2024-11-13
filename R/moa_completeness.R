#' moa_completeness function
#'
#' This function allows you to perform completeness check
#' @param
#' @keywords completeness
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import SqlRender
#' @import tcltk
#' @import foreach
#' @import doParallel
#' @import parallel
#' @import doSNOW
#' @import itertools
#' @export
#' @examples
#' moa_completeness(x)

moa_completeness<-function(){

  con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))

  mydbtype=tolower(con_info$dbtype)
  myschemaname_lv1=con_info$schemaname_lv1
  myschemaname_lv2=con_info$schemaname_lv2
  myvocabschemaname=con_info$schemaname_vocab

#  sql<-translate('select "@A" as "A" from @B."@C"', targetDialect = mydbtype)

  n_core<-detectCores(); cl=makeCluster(n_core-1); registerDoSNOW(cl)
  clusterEvalQ(cl, {library(moadqproject); con <- connect_DB(); NULL})

  iterations<-nrow(completeness_rule); pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)

#completeness_result <-
#  foreach(i=completeness_rule$rule_id, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI', 'moadqproject'), .noexport="con", .options.snow = opts)%dopar%{
#    tmp1<-which(completeness_rule$rule_id==i); tmp2<-completeness_rule[tmp1,]
#    if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
#    if(is_consistent(tmp2)[[1]]==TRUE){
#      tmp3<-dbGetQuery(con, render(sql, A=tmp2$field, B=schema, C=tmp2$table))
#      names(tmp3)<-toupper(names(tmp3))
#      tmp3$A<-gsub(' ', '', as.character(tmp3$A))
#      tmp4<-is.na(tmp3$A)|tmp3$A==''
#      if(length(tmp4)==0){result<-NA}else{result<-round((sum(tmp4)/length(tmp4))*100, 0)}
#    } else{result<-NA}
#
#    cbind(tmp2, result)
#    gc()
#
#  }


sql1<- translate('select count(*) as "A" FROM @B."@C"', targetDialect = mydbtype)  
sql2<- translate('select count(*) as "A" FROM @B."@C" WHERE "@A" IS NULL', targetDialect = mydbtype)

  completeness_result <-
  foreach(i=completeness_rule$rule_id, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI', 'moadqproject'), .noexport="con", .options.snow = opts)%dopar%{
    tmp1<-which(completeness_rule$rule_id==i); tmp2<-completeness_rule[tmp1,]
    if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
    if(is_consistent(tmp2)[[1]]==TRUE){
      tmp3<-dbGetQuery(con, render(sql1, B=schema, C=tmp2$table)); names(tmp3)<-toupper(names(tmp3))
      tmp4<-dbGetQuery(con, render(sql2, A=tmp2$field, B=schema, C=tmp2$table)); names(tmp4)<-toupper(names(tmp4))
      result<-round(tmp4$A/tmp3$A*100, 0)
    } else{result<-NA}

    cbind(tmp2, result)
  }

 close(pb)
  
clusterEvalQ(cl, {library(DatabaseConnector); disconnect(con); NULL})

saveRDS(completeness_result, file.path(system.file(package="moadqproject"), 'results/completeness.rds'))

progress<-read.table(file.path(system.file(package='moadqproject'), 'results/progress.txt'), header=TRUE)
progress$status[which(progress$rule=="Completeness")]<-TRUE
write.table(progress, file.path(system.file(package='moadqproject'), 'results/progress.txt'), row.names = FALSE)

completeness_result$pass<-completeness_result$result==0

completeness_score<-aggregate(pass~level+table, completeness_result, FUN=mean)
names(completeness_score)<-c('level', 'table', 'completeness')
saveRDS(completeness_score, file.path(system.file(package="moadqproject"), 'results/completeness_score.rds'))

stopCluster(cl)
}
