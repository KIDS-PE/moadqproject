#' moa_consistency function
#'
#' This function allows you to perform consistency check
#' @param
#' @keywords consistency
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @import SqlRender
#' @import tcltk
#' @export
#' @examples
#' moa_consistency(x)

moa_consistency<-function(con){

#con_info<-readRDS('data/result/con_info.RDS')
mydbtype=tolower(con_info$dbtype)
myschemaname_lv1=con_info$schemaname_lv1
myschemaname_lv2=con_info$schemaname_lv2
myvocabschemaname=con_info$schemaname_vocab

#consistency_rule<-read.csv('data/rule/consistency.csv', header=TRUE)

consistency_result<-consistency_rule
consistency_result$result<-FALSE

# table name consistency
sql<-translate("select table_name from information_schema.tables where table_schema='@A'", targetDialect = mydbtype)

for(i in (consistency_result%>%filter(rule=='table name consistency'))$rule_id){
  tmp1<-which(consistency_result$rule_id==i); tmp2<-consistency_result[tmp1,]
  if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
  tmp3<-dbGetQuery(con, render(sql, A=schema))
  consistency_result$result[tmp1]<-(tmp2$ref)%in%tmp3$table_name==TRUE
}

remove(tmp1); remove(tmp2); remove(tmp3)

# field name consistency
sql<-translate("select column_name from information_schema.columns where table_schema='@A' and table_name='@B'", targetDialect=mydbtype)

for(i in (consistency_result%>%filter(rule=='field name consistency'))$rule_id){
  tmp1<-which(consistency_result$rule_id==i); tmp2<-consistency_result[tmp1,]
  if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
  tmp3<-dbGetQuery(con, render(sql, A=schema, B=tmp2$table))
  consistency_result$result[tmp1]<-(tmp2$ref%in%tmp3$column_name==TRUE)
}

remove(tmp1); remove(tmp2); remove(tmp3)

# field type consistency
search_type<-read.csv('data/rule/datatype.csv', header=TRUE)
sql<-translate("select data_type from information_schema.columns where table_schema='@A' and table_name='@B' and column_name='@C'", targetDialect = mydbtype)

for(i in (consistency_result%>%filter(rule=='field type consistency'))$rule_id){
  tmp1<-which(consistency_result$rule_id==i); tmp2<-consistency_result[tmp1,]
  if((consistency_result%>%filter(rule=='field name consistency' & level==tmp2$level & table==tmp2$table & field==tmp2$field))$result==TRUE){
    if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}
    tmp3<-dbGetQuery(con, render(sql, A=schema, B=tmp2$table, C=tmp2$field)); tmp3$type<-''
    for(j in c(1:nrow(search_type))){tmp3$type[grep(paste0(search_type$type_detail[j]), tmp3$data_type)]<-search_type$type[j]}
    consistency_result$result[tmp1]<-(tmp2$ref==tmp3$type)
  }
  else{consistency_result$result[tmp1]<-NA}
}


remove(tmp1); remove(tmp2); remove(tmp3)
#saveRDS(consistency_result, 'data/result/consistency.rds')
usethis::use_data(consistency_result, overwrite = TRUE)

#progress<-read.table('data/result/progress.txt', header=TRUE)
progress$status[which(progress$rule=="Consistency")]<-TRUE
#write.table(progress, 'data/result/progress.txt', row.names = FALSE)
usethis::use_data(progress, overwrite = TRUE)

consistency_score<-aggregate(result~level+table, consistency_result, FUN=mean)
names(consistency_score)<-c('level', 'table', 'consistency')

#saveRDS(consistency_score, 'data/result/consistency_score.rds')
usethis::use_data(consistency_score, overwrite = TRUE)

}
