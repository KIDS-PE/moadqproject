#' moa_validity function
#'
#' This function allows you to perform validity check
#' @param
#' @keywords validity
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
#' moa_validity(x)


moa_validity<-function(){

  con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))

  #con_info<-readRDS('data/result/con_info.RDS')
  mydbtype=tolower(con_info$dbtype)
  myschemaname_lv1=con_info$schemaname_lv1
  myschemaname_lv2=con_info$schemaname_lv2
  myvocabschemaname=con_info$schemaname_vocab

  sql1<-translate('select "@A" as "A" from @B."@C"', targetDialect = mydbtype)
  sql2<-translate('select distinct concept_id from @B.concept where @D', targetDialect = mydbtype)

  n_core<-detectCores(); cl=makeCluster(n_core-1); registerDoSNOW(cl)
  clusterEvalQ(cl, { library(moadqproject); con <- connect_DB(); NULL })

  iterations<-nrow(validity_rule); pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)

  validity_result<-
    foreach(i=validity_rule$rule_id, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI', 'moadqproject'), .noexport="con", .options.snow = opts)%dopar%{
      tmp1<-which(validity_rule$rule_id==i); tmp2<-validity_rule[tmp1,]
      if(is_consistent(tmp2)[[1]]==TRUE){
        if(tmp2$rule=='vocab validity'){
          if(tmp2$level==1){
            tmp3<-dbGetQuery(con, render(sql1, A=tmp2$field, B=myschemaname_lv1, C=tmp2$table))
            names(tmp3)<-toupper(names(tmp3))
            tmp4<-unlist(strsplit(tmp2$ref, split = '\\|'))
            tmp5<-tmp3$A%in%tmp4==FALSE
            result<-round((sum(tmp5)/length(tmp5))*100, 0)
          }
          if(tmp2$level==2){
            tmp3<-dbGetQuery(con, render(sql1, A=tmp2$field, B=myschemaname_lv2, C=tmp2$table))
            names(tmp3)<-toupper(names(tmp3))
            tmp4<-dbGetQuery(con, render(sql2, B=myvocabschemaname, D=tmp2$ref))
            names(tmp4)<-toupper(names(tmp4))
            tmp5<-tmp3$A%in%tmp4$concept_id==FALSE
            result<-round((sum(tmp5)/length(tmp5))*100, 0)
          }
        }} else{result<-NA}

      cbind(tmp2, result)
    }
  close(pb)

  clusterEvalQ(cl, {library(DatabaseConnector); disconnect(con); NULL})

  saveRDS(validity_result, file.path(system.file(package="moadqproject"), 'results/validity.rds'))

  progress<-read.table(file.path(system.file(package='moadqproject'), 'results/progress.txt'), header=TRUE)
  progress$status[which(progress$rule=="Validity")]<-TRUE
  write.table(progress, file.path(system.file(package='moadqproject'), 'results/progress.txt'), row.names = FALSE)

  validity_result$pass<-validity_result$result==0
  validity_score<-aggregate(pass~level+table, validity_result, FUN=mean)
  names(validity_score)<-c('level', 'table', 'validity')
  saveRDS(validity_score, file.path(system.file(package='moadqproject'), 'results/validity_score.rds'))

  stopCluster(cl)

}
