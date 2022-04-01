#' update_overview function
#'
#' This function allows you to update overview score table
#' @param
#' @keywords overview
#' @return dashboard_overview, no_rules
#' @export
#' @examples
#' update_overview()

update_overview<-function(){

  overview<-merge(consistency_score, completeness_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, uniqueness_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, validity_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, accuracy_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)

  overview$level[which(overview$level==1)]<-'SCDM'
  overview$level[which(overview$level==2)]<-'OMOP'
  overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')]<-
    round(overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')]*100, 0)
  overview$total<-rowMeans(overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')], na.rm = TRUE)

  usethis::use_data(overview, overwrite = TRUE)


  no_rules<-rbind(
    aggregate(rule_id~level+table, consistency_result, length),
    aggregate(rule_id~level+table, completeness_result, length),
    aggregate(rule_id~level+table, uniqueness_result, length),
    aggregate(rule_id~level+table, validity_result, length),
    aggregate(rule_id~level+table, accuracy_result, length))

  no_rules<-aggregate(rule_id~level+table, no_rules, sum)

  no_rules$level[which(no_rules$level==1)]<-'SCDM'
  no_rules$level[which(no_rules$level==2)]<-'OMOP'

  usethis::use_data(no_rules, overwrite=TRUE)

  }
