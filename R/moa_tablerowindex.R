#' moa_tablerowindex function
#'
#' This function allows you to write table row index
#' @param
#' @keywords tablerowindex
#' @return rds file
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @import SqlRender
#' @import tcltk
#' @export
#' @examples
#' moa_tablerowindex(x)

moa_tablerowindex<-function(con){

###################################
##        Table row index        ##
###################################

#con_info<-readRDS('data/result/con_info.RDS')
#consistency_result<-readRDS('data/result/consistency.RDS')

mydbtype=tolower(con_info$dbtype)
myschemaname_lv1=con_info$schemaname_lv1
myschemaname_lv2=con_info$schemaname_lv2
myschemanave_vocab=con_info$schemaname_vocab

sql<-translate('select count(*) from @A."@B"', targetDialect = mydbtype)
table_count<-consistency_result%>%filter(rule=='table name consistency')%>%select(c('level', 'table', 'result'))
table_count$count<-0

n_iter<-nrow(table_count)
pb <- tkProgressBar(title = "Counting rows", label = "Percentage completed",
                    min = 0, max = n_iter,initial = 0, width = 400)

for(i in c(1:nrow(table_count))){
  if(table_count$result[i]==TRUE){
    if(table_count$level[i]==1){schema=myschemaname_lv1}; if(table_count$level[i]==2){schema=myschemaname_lv2}
    tmp1<-dbGetQuery(con, render(sql, A=schema, B=table_count$table[i]))
    table_count$count[i]=tmp1$count
  }
  else{table_count$count[i]=0}
  pctg <- paste(round(i/n_iter *100, 0), "% completed |", table_count$table[i])
  setTkProgressBar(pb, i, label = pctg)
}

close(pb)
remove(tmp1)

#saveRDS(table_count, 'data/result/table_count.rds')
usethis::use_data(table_count, overwrite = TRUE)

}
