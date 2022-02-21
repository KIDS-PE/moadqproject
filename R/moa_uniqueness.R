#' moa_uniqueness function
#'
#' This function allows you to perform uniqueness check
#' @param
#' @keywords uniqueness
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @import SqlRender
#' @import tcltk
#' @export
#' @examples
#' moa_uniqueness(x)

moa_uniqueness<-function(con){

con_info<-readRDS('data/result/con_info.RDS')
consistency_result<-readRDS('data/result/consistency.RDS')

mydbtype=tolower(con_info$dbtype)
myschemaname_lv1=con_info$schemaname_lv1
myschemaname_lv2=con_info$schemaname_lv2
myvocabschemaname=con_info$schemaname_vocab

uniqueness_rule<-read.csv('data/rule/uniqueness.csv', header=TRUE)
uniqueness_result<-uniqueness_rule
uniqueness_result$result<-NA
n_iter<-nrow(uniqueness_rule)
pb <- tkProgressBar(title = "Checking uniqueness", label = "Percentage completed",
                    min = 0, max = n_iter,initial = 0, width = 500)

for(i in uniqueness_rule$rule_id){
  tmp1<-which(uniqueness_rule$rule_id==i); tmp2<-uniqueness_rule[tmp1,]
  if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
  if(is_consistent(tmp2)[[1]]==TRUE){
    if(tmp2$rule=='global uniqueness'){
      sql<-translate('select "@A" as "A" from @B."@C"', targetDialect = mydbtype)
      tmp3<-dbGetQuery(con, render(sql, A=tmp2$field, B=schema, C=tmp2$table))
      tmp4<-duplicated(tmp3$A)|duplicated(tmp3$A, fromLast = TRUE)}
    tmp4<-round(sum(tmp4)/length(tmp4)*100, 0)
    if(tmp2$rule=='local uniqueness'){
      sql<-translate('select "@A" as "A", "@B" as "B" from @C."@D"', targetDialect = mydbtype)
      tmp3<-dbGetQuery(con, render(sql, A=tmp2$field, B=tmp2$ref, C=schema, D=tmp2$table))
      tmp4<-tmp3%>%group_by(B)%>%mutate(distinct_values=n_distinct(A))
      tmp4<-tmp4$distinct_values>1
      tmp4<-round(sum(tmp4)/length(tmp4)*100, 0)}
  } else{tmp4<-NA}

  uniqueness_result$result[tmp1]<-tmp4
  pctg <- paste(round(tmp1/n_iter *100, 0), "% completed |", tmp2$table, "-", tmp2$field)
  setTkProgressBar(pb, tmp1, label = pctg)

}

close(pb)
remove(tmp1); remove(tmp2); remove(tmp3); remove(tmp4)
saveRDS(uniqueness_result, file=paste0("data/result/uniqueness.rds"))

progress<-read.table('data/result/progress.txt', header=TRUE)
progress$status[which(progress$rule=="Uniqueness")]<-TRUE
write.table(progress, 'data/result/progress.txt', row.names = FALSE)

uniqueness_result$pass<-uniqueness_result$result==0

uniqueness_score<-aggregate(pass~level+table, uniqueness_result, FUN=mean)
names(uniqueness_score)<-c('level', 'table', 'uniqueness')

saveRDS(uniqueness_score, 'data/result/uniqueness_score.rds')



}
