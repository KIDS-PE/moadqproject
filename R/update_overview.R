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
  consistency_score<-readRDS("data/result/consistency_score.rds")
  completeness_score<-readRDS("data/result/completeness_score.rds")
  uniqueness_score<-readRDS("data/result/uniqueness_score.rds")
  validity_score<-readRDS("data/result/validity_score.rds")
  accuracy_score<-readRDS("data/result/accuracy_score.rds")

  overview<-merge(consistency_score, completeness_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, uniqueness_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, validity_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, accuracy_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)

  overview$level[which(overview$level==1)]<-'SCDM'
  overview$level[which(overview$level==2)]<-'OMOP'
  overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')]<-
    round(overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')]*100, 0)
  overview$total<-rowMeans(overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')], na.rm = TRUE)
  saveRDS(overview, 'data/result/dashboard_overview.rds')

  no_rules<-rbind(
    aggregate(rule_id~level+table, readRDS("data/result/consistency.rds"), length),
    aggregate(rule_id~level+table, readRDS("data/result/completeness.rds"), length),
    aggregate(rule_id~level+table, readRDS("data/result/uniqueness.rds"), length),
    aggregate(rule_id~level+table, readRDS("data/result/validity.rds"), length),
    aggregate(rule_id~level+table, readRDS("data/result/accuracy.rds"), length))

  no_rules<-aggregate(rule_id~level+table, no_rules, sum)

  no_rules$level[which(no_rules$level==1)]<-'SCDM'
  no_rules$level[which(no_rules$level==2)]<-'OMOP'

  saveRDS(no_rules, 'data/result/no_rules.rds')

  }
