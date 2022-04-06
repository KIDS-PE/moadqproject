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

  consistency_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/consistency_score.rds'))
  completeness_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/completeness_score.rds'))
  uniqueness_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/uniqueness_score.rds'))
  validity_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/validity_score.rds'))
  accuracy_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/accuracy_score.rds'))

  overview<-merge(consistency_score, completeness_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, uniqueness_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, validity_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)
  overview<-merge(overview, accuracy_score, by=c('level', 'table'), all.x=TRUE, all.y=TRUE)

  overview$level[which(overview$level==1)]<-'SCDM'
  overview$level[which(overview$level==2)]<-'OMOP'
  overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')]<-
    round(overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')]*100, 0)
  overview$total<-rowMeans(overview[,c('consistency', 'completeness', 'uniqueness', 'validity', 'accuracy')], na.rm = TRUE)

  saveRDS(overview, file.path(system.file(package="moadqproject"), 'results/overview.rds'))

  no_rules<-rbind(
    aggregate(rule_id~level+table, consistency_rule, length),
    aggregate(rule_id~level+table, completeness_rule, length),
    aggregate(rule_id~level+table, uniqueness_rule, length),
    aggregate(rule_id~level+table, validity_rule, length),
    aggregate(rule_id~level+table, accuracy_rule, length))

  no_rules<-aggregate(rule_id~level+table, no_rules, sum)

  no_rules$level[which(no_rules$level==1)]<-'SCDM'
  no_rules$level[which(no_rules$level==2)]<-'OMOP'

  saveRDS(no_rules, file.path(system.file(package="moadqproject"), 'results/no_rules.rds'))

  }
