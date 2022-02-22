#' moa_validity function
#'
#' This function allows you to perform validity check
#' @param
#' @keywords validity
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @import SqlRender
#' @import tcltk
#' @export
#' @examples
#' moa_validity(x)


moa_validity<-function(con){

  #con_info<-readRDS('data/result/con_info.RDS')
  mydbtype=tolower(con_info$dbtype)
  myschemaname_lv1=con_info$schemaname_lv1
  myschemaname_lv2=con_info$schemaname_lv2
  myvocabschemaname=con_info$schemaname_vocab


#validity_rule<-read.csv('data/rule/validity.csv', header=TRUE)
  validity_result<-validity_rule; validity_result$result<-NA

sql1<-translate('select "@A" as "A" from @B."@C"', targetDialect = mydbtype)
sql2<-translate('select distinct concept_id from @B.concept where @D', targetDialect = mydbtype)

n_iter<-nrow(validity_rule)
pb <- tkProgressBar(title = "Checking validity", label = "Percentage completed",
                    min = 0, max = n_iter,initial = 0, width = 500)

for(i in validity_rule$rule_id){
  tmp1<-which(validity_rule$rule_id==i); tmp2<-validity_rule[tmp1,]
  if(is_consistent(tmp2)[[1]]==TRUE){
    if(tmp2$rule=='vocab validity'){
      if(tmp2$level==1){
        tmp3<-dbGetQuery(con, render(sql1, A=tmp2$field, B=myschemaname_lv1, C=tmp2$table))
        tmp4<-unlist(strsplit(tmp2$ref, split = '\\|'))
        tmp5<-tmp3$A%in%tmp4==FALSE
        validity_result$result[tmp1]<-round((sum(tmp5)/length(tmp5))*100, 0)
      }
      if(tmp2$level==2){
        tmp3<-dbGetQuery(con, render(sql1, A=tmp2$field, B=myschemaname_lv2, C=tmp2$table))
        tmp4<-dbGetQuery(con, render(sql2, B=myvocabschemaname, D=tmp2$ref))
        tmp5<-tmp3$A%in%tmp4$concept_id==FALSE
        validity_result$result[tmp1]<-round((sum(tmp5)/length(tmp5))*100, 0)
      }
    }} else{validity_result$result[tmp1]<-NA}

  pctg <- paste(round(tmp1/n_iter *100, 0), "% completed |", tmp2$table, "-", tmp2$field)
  setTkProgressBar(pb, tmp1, label = pctg)

}

#saveRDS(validity_result, file = paste0('data/result/validity.rds'))
usethis::use_data(validity_result, overwrite=TRUE)


close(pb)
remove(tmp1); remove(tmp2); remove(tmp3); remove(tmp4); remove(tmp5)

#progress<-read.table('data/result/progress.txt', header=TRUE)
progress$status[which(progress$rule=="Validity")]<-TRUE
#write.table(progress, 'data/result/progress.txt', row.names = FALSE)
usethis::use_data(progress, overwrite=TRUE)

validity_result$pass<-validity_result$result==0

validity_score<-aggregate(pass~level+table, validity_result, FUN=mean)
names(validity_score)<-c('level', 'table', 'validity')

#saveRDS(validity_score, 'data/result/validity_score.rds')
usethis::use_data(validity_score, overwrite=TRUE)

}
