#' moa_completeness function
#'
#' This function allows you to perform completeness check
#' @param
#' @keywords completeness
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @import SqlRender
#' @import tcltk
#' @export
#' @examples
#' moa_completeness(x)

moa_completeness<-function(con){

  con_info<-readRDS('data/result/con_info.RDS')
  mydbtype=tolower(con_info$dbtype)
  myschemaname_lv1=con_info$schemaname_lv1
  myschemaname_lv2=con_info$schemaname_lv2
  myvocabschemaname=con_info$schemaname_vocab


completeness_rule<-read.csv("data/rule/completeness.csv", header=TRUE)
sql<-translate('select "@A" as "A" from @B."@C"', targetDialect = mydbtype)
completeness_result<-completeness_rule; completeness_result$result<-NA

n_iter<-nrow(completeness_rule)
pb <- tkProgressBar(title = "Checking completeness", label = "Percentage completed",
                    min = 0, max = n_iter,initial = 0, width = 500)

for(i in completeness_rule$rule_id){
  tmp1<-which(completeness_rule$rule_id==i); tmp2<-completeness_rule[tmp1,]
  if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
  if(is_consistent(tmp2)[[1]]==TRUE){
    tmp3<-dbGetQuery(con, render(sql, A=tmp2$field, B=schema, C=tmp2$table))
    tmp3$A<-gsub(' ', '', as.character(tmp3$A))
    tmp4<-is.na(tmp3$A)|tmp3$A==''
    if(length(tmp4)==0){completeness_result$result[tmp1]<-NA}else{completeness_result$result[tmp1]<-round((sum(tmp4)/length(tmp4))*100, 0)}
  } else{completeness_result$result[tmp1]<-NA}

  pctg <- paste(round(tmp1/n_iter *100, 0), "% completed |", tmp2$table, "-", tmp2$field)
  setTkProgressBar(pb, tmp1, label = pctg)

}

close(pb)
remove(tmp1); remove(tmp2); remove(tmp3); remove(tmp4)

saveRDS(completeness_result, file=paste0("data/result/completeness.rds"))

progress<-read.table('data/result/progress.txt', header=TRUE)
progress$status[which(progress$rule=="Completeness")]<-TRUE
write.table(progress, 'data/result/progress.txt', row.names = FALSE)


completeness_result$pass<-completeness_result$result==0

completeness_score<-aggregate(pass~level+table, completeness_result, FUN=mean)
names(completeness_score)<-c('level', 'table', 'completeness')

saveRDS(completeness_score, 'data/result/completeness_score.rds')



}
