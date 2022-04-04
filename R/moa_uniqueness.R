#' moa_uniqueness function
#'
#' This function allows you to perform uniqueness check
#' @param
#' @keywords uniqueness
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import SqlRender
#' @import foreach
#' @import doParallel
#' @import parallel
#' @export
#' @examples
#' moa_uniqueness(x)

moa_uniqueness<-function(){

con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))

mydbtype=tolower(con_info$dbtype)
myschemaname_lv1=con_info$schemaname_lv1
myschemaname_lv2=con_info$schemaname_lv2
myvocabschemaname=con_info$schemaname_vocab

n_core<-detectCores()
cl=makeCluster(n_core-1)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(moadqproject)
  con <- connect_DB()
  NULL
})

uniqueness_result<-

  foreach(i= uniqueness_rule$rule_id, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI'), .noexport="con")%dopar%{
    tmp1<-which(uniqueness_rule$rule_id==i); tmp2<-uniqueness_rule[tmp1,]
    if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
    if(is_consistent(tmp2)[[1]]==TRUE){
      if(tmp2$rule=='global uniqueness'){
        sql<-translate('select "@A" as "A" from @B."@C"', targetDialect = mydbtype)
        tmp3<-dbGetQuery(con, render(sql, A=tmp2$field, B=schema, C=tmp2$table))
        result<-duplicated(tmp3$A)|duplicated(tmp3$A, fromLast = TRUE)}
      tmp4<-round(sum(tmp4)/length(tmp4)*100, 0)
      if(tmp2$rule=='local uniqueness'){
        sql<-translate('select "@A" as "A", "@B" as "B" from @C."@D"', targetDialect = mydbtype)
        tmp3<-dbGetQuery(con, render(sql, A=tmp2$field, B=tmp2$ref, C=schema, D=tmp2$table))
        tmp4<-tmp3%>%group_by(B)%>%mutate(distinct_values=n_distinct(A))
        tmp4<-tmp4$distinct_values>1
        result<-round(sum(tmp4)/length(tmp4)*100, 0)}
    } else{result<-NA}

  cbind(tmp2, result)
}

saveRDS(uniqueness_result, file.path(system.file(package="moadqproject"), 'results/uniqueness.rds'))

progress<-read.table(file.path(system.file(package='moadqproject'), 'results/progress.txt'), header=TRUE)
progress$status[which(progress$rule=="Uniqueness")]<-TRUE
write.table(progress, file.path(system.file(package='moadqproject'), 'results/progress.txt'), row.names = FALSE)

uniqueness_result$pass<-uniqueness_result$result==0
uniqueness_score<-aggregate(pass~level+table, uniqueness_result, FUN=mean)
names(uniqueness_score)<-c('level', 'table', 'uniqueness')

saveRDS(uniqueness_score, file.path(system.file(package='moadqproject'), 'results/uniqueness_score.rds'))

stopCluster(cl)

}
