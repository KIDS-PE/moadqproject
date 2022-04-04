#' moa_accuracy function
#'
#' This function allows you to perform accuracy check
#' @param
#' @keywords accuracy
#' @return csv file
#' @import DBI
#' @import dplyr
#' @import SqlRender
#' @import foreach
#' @import doParallel
#' @import parallel
#' @export
#' @examples
#' moa_accuracy(x)

moa_accuracy<-function(){

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

prerequisite<-function(tmp2){
  prerequisite_result<-c(tmp2$level, tmp2$table, tmp2$field, tmp2$level, tmp2$table, tmp2$ref_key,
                         tmp2$level, tmp2$ref_table, tmp2$ref_key, tmp2$level, tmp2$ref_table, tmp2$ref_field)
  prerequisite_result<-as.data.frame(cbind(matrix(prerequisite_result, ncol=3, byrow = TRUE), matrix(0, nrow=4, ncol=3)))
  colnames(prerequisite_result)<-c('level', 'table', 'field', 'consistency', 'completeness', 'uniqueness')

  for(j in c(1:4)){
    tmp<-prerequisite_result[j,c(1:3)]
    if(grepl('\\"', tmp$field)==TRUE){ tmp$field=gsub('"', '', regmatches(tmp$field, gregexpr('"([^"]*)"', tmp$field))[[1]])}
    prerequisite_result[j,c(4:6)]<-c(is_consistent(tmp)[[2]], is_complete(tmp)[[2]], is_unique(tmp)[[2]])
  }

  return(prerequisite_result)
}

accuracy_result<-

foreach(i=accuracy_rule$rule_id, .combine=rbind, .packages=c('dplyr', 'SqlRender', 'DBI'), .noexport="con")%dopar%{
  tmp1<-which(accuracy_rule$rule_id==i); tmp2<-accuracy_rule[tmp1,]
  if(tmp2$level==1){schema=myschemaname_lv1}; if(tmp2$level==2){schema=myschemaname_lv2}

  if(tmp2$rule=='date accuracy'){
    tmp3<-prerequisite(tmp2)
    error_message<-{c(tmp3$consistency, tmp3$completeness[c(2,3)], tmp3$uniqueness[3])}
    error_message<-unique(error_message[complete.cases(error_message)])

    if(length(error_message)>0){result<-paste(error_message, collapse = ';')} else{
      sql1<-'select "@A" as "field", "@B" as "ref_key" from @C."@D"'
      sql2<-'select "@B" as "ref_key", @E as "ref_field" from @F."@G"'
      tmp4<-dbGetQuery(con, translate(render(sql1, A=tmp2$field, B=tmp2$ref_key, C=schema, D=tmp2$table), targetDialect = mydbtype))
      tmp5<-dbGetQuery(con, translate(render(sql2, B=tmp2$ref_key, E=tmp2$ref_field, F=schema, G=tmp2$ref_table), targetDialect = mydbtype))

      tmp6<-merge(tmp4, tmp5, by='ref_key', all.x=TRUE)
      if(tmp2$rangeorder=='H'){tmp7<-sum(tmp6$field>tmp6$ref_field, na.rm=TRUE)}
      if(tmp2$rangeorder=='L'){tmp7<-sum(tmp6$field<tmp6$ref_field, na.rm=TRUE)}

      if(is.na(tmp7)==TRUE){result<-NA} else{result<-round((tmp7/nrow(tmp6))*100, 0)}
    }
  }


  if(tmp2$rule=='age accuracy'){
    tmp3<-prerequisite(tmp2)
    error_message<-{c(tmp3$consistency, tmp3$completeness[c(2,3)], tmp3$uniqueness[3])}
    error_message<-unique(error_message[complete.cases(error_message)])
    if(tmp2$level==1){ref_date='year(T1."ADate")'}; if(tmp2$level==2){ref_date='year(T1."condition_start_date")'}

    if(length(error_message)>0){result<-paste(error_message, collapse = ';')} else{
      sql1<-'select T1."@A" as "field", T1."@B" as "ref_key", @C as "ref_date", @D as "ref_field"
      from @E."@F" T1 left join @E."@G" T2 on T1."@B"=T2."@B" where "@A"=@H'
      tmp4<-dbGetQuery(con, translate(render(sql1, A=tmp2$field, B=tmp2$ref_key, C=ref_date, D=tmp2$ref_field,
                                             E=schema, F=tmp2$table, G=tmp2$ref_table, H=tmp2$field_value), targetDialect = mydbtype))
      tmp4$age<-as.numeric(tmp4$ref_date-tmp4$ref_field)
      tmp2$threashold<-as.numeric(as.character(tmp2$threashold))
      if(tmp2$rangeorder=='H'){tmp5<-length(which(tmp4$age>tmp2$threashold)==TRUE)}
      if(tmp2$rangeorder=='L'){tmp5<-length(which(tmp4$age<tmp2$threashold)==TRUE)}

      count_rows=(table_count%>%filter(table==tmp2$table))$count
      if(is.na(tmp5)==TRUE){result<-NA} else{result<-round((tmp5/count_rows)*100, 4)}
    }
  }


  if(tmp2$rule=='gender accuracy'){
    tmp3<-prerequisite(tmp2)
    error_message<-{c(tmp3$consistency, tmp3$completeness[c(2,3)], tmp3$uniqueness[3])}
    error_message<-unique(error_message[complete.cases(error_message)])
    if(tmp2$level==1){ref_male="M"; ref_female='F'}; if(tmp2$level==2){ref_male=8507; ref_female=8532}

    if(length(error_message)>0){result<-paste(error_message, collapse = ';')} else{
      sql1<-'select T1."@A" as "field", T1."@B" as "ref_key", @C as "ref_field"
      from @D."@E" T1 left join @D."@F" T2 on T1."@B"=T2."@B" where "@A"=@G'
      tmp4<-dbGetQuery(con, translate(render(sql1, A=tmp2$field, B=tmp2$ref_key, C=tmp2$ref_field,
                                             D=schema, E=tmp2$table, F=tmp2$ref_table, G=tmp2$field_value), targetDialect = mydbtype))
      if(tmp2$threashold=='M'){tmp5<-sum(tmp4$ref_field==ref_female, na.rm=TRUE)}
      if(tmp2$threashold=='F'){tmp5<-sum(tmp4$ref_field==ref_male, na.rm=TRUE)}

      count_rows=(table_count%>%filter(table==tmp2$table))$count
      if(is.na(tmp5)==TRUE){result<-NA} else{result<-round((tmp5/count_rows)*100, 4)}
    }
  }


  if(tmp2$rule=='meas value range accuracy'){
    tmp3<-prerequisite(tmp2)
    error_message<-c(tmp3$consistency); error_message<-unique(error_message[complete.cases(error_message)])

    if(length(error_message)>0){result<-paste(error_message, collapse = ';')} else{
      sql1<-'select "@A" as "field", "@B" as "unit", "@C" as "ref_field" from @D."@E" where "@A"=@F and "@B"=@G'
      tmp4<-dbGetQuery(con, translate(render(sql1, A=tmp2$field, B=tmp2$ref_key, C=tmp2$ref_field,
                                             D=schema, E=tmp2$table, F=tmp2$field_value, G=tmp2$unit), targetDialect = mydbtype))
      tmp2$threashold<-as.numeric(as.character(tmp2$threashold))
      if(tmp2$rangeorder=='H'){tmp5<-sum(tmp4$ref_field>tmp2$threashold, na.rm=TRUE)}
      if(tmp2$rangeorder=='L'){tmp5<-sum(tmp4$ref_field<tmp2$threashold, na.rm=TRUE)}

      count_rows=(table_count%>%filter(table==tmp2$table))$count
      if(is.na(tmp5)==TRUE){result<-NA} else{result<-round((tmp5/count_rows)*100, 4)}
    }
  }


  if(tmp2$rule=='vital value range accuracy'){
    tmp3<-is_consistent(tmp2)[[1]]

    if(tmp3!=TRUE){result<-paste(error_message, collapse = ';')} else{
      sql1<-'select "@A" as "field" from @B."@C"'
      tmp4<-dbGetQuery(con, translate(render(sql1, A=tmp2$field, B=schema, C=tmp2$table), targetDialect = mydbtype))
      tmp2$threashold<-as.numeric(as.character(tmp2$threashold))
      tmp4$field<-as.numeric(gsub("[^\\d]+", "", tmp4$field, perl=TRUE))
      if(tmp2$rangeorder=='H'){tmp5<-sum(tmp4$field>tmp2$threashold, na.rm=TRUE)}
      if(tmp2$rangeorder=='L'){tmp5<-sum(tmp4$field<tmp2$threashold, na.rm=TRUE)}

      count_rows=(table_count%>%filter(table==tmp2$table))$count
      if(is.na(tmp5)==TRUE){result<-NA} else{result<-round((tmp5/count_rows)*100, 4)}
    }

  }

 cbind(tmp2, result)
}


saveRDS(accuracy_result, file.path(system.file(package="moadqproject"), 'results/accuracy.rds'))

progress<-read.table(file.path(system.file(package='moadqproject'), 'results/progress.txt'), header=TRUE)
progress$status[which(progress$rule=="Accuracy")]<-TRUE
write.table(progress, file.path(system.file(package='moadqproject'), 'results/progress.txt'), row.names = FALSE)

accuracy_result$pass<-accuracy_result$result==0
accuracy_score<-aggregate(pass~level+table, accuracy_result, FUN=mean)
names(accuracy_score)<-c('level', 'table', 'accuracy')

saveRDS(accuracy_score, file.path(system.file(package='moadqproject'), 'results/accuracy_score.rds'))

stopCluster(cl)

}
