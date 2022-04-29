#' moa_initialize function
#'
#' This function allows you to initialize dqm process
#' @param
#' @keywords initialize
#' @return rds file
#' @export
#' @examples
#' moa_initialize()


moa_initialize<-function(){
  consistency<-readRDS(file.path(system.file(package="moadqproject"), 'results/consistency.rds'))
  consistency$result<-NA
  saveRDS(consistency, file.path(system.file(package="moadqproject"), 'results/consistency.rds'))

  consistency_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/consistency_score.rds'))
  consistency_score$consistency<-0
  saveRDS(consistency_score, file.path(system.file(package="moadqproject"), 'results/consistency_score.rds'))

  completeness<-readRDS(file.path(system.file(package="moadqproject"), 'results/completeness.rds'))
  completeness$result<-NA
  saveRDS(completeness, file.path(system.file(package="moadqproject"), 'results/completeness.rds'))

  completeness_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/completeness_score.rds'))
  completeness_score$completeness<-0
  saveRDS(completeness_score, file.path(system.file(package="moadqproject"), 'results/completeness_score.rds'))

  uniqueness<-readRDS(file.path(system.file(package="moadqproject"), 'results/uniqueness.rds'))
  uniqueness$result<-NA
  saveRDS(uniqueness, file.path(system.file(package="moadqproject"), 'results/uniqueness.rds'))

  uniqueness_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/uniqueness_score.rds'))
  uniqueness_score$uniqueness<-0
  saveRDS(uniqueness_score, file.path(system.file(package="moadqproject"), 'results/uniqueness_score.rds'))

  validity<-readRDS(file.path(system.file(package="moadqproject"), 'results/validity.rds'))
  validity$result<-NA
  saveRDS(validity, file.path(system.file(package="moadqproject"), 'results/validity.rds'))

  validity_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/validity_score.rds'))
  validity_score$validity<-0
  saveRDS(validity_score, file.path(system.file(package="moadqproject"), 'results/validity_score.rds'))

  accuracy<-readRDS(file.path(system.file(package="moadqproject"), 'results/accuracy.rds'))
  accuracy$result<-NA
  saveRDS(accuracy, file.path(system.file(package="moadqproject"), 'results/accuracy.rds'))

  accuracy_score<-readRDS(file.path(system.file(package="moadqproject"), 'results/accuracy_score.rds'))
  accuracy_score$accuracy<-0
  saveRDS(accuracy_score, file.path(system.file(package="moadqproject"), 'results/accuracy_score.rds'))

  no_rules<-readRDS(file.path(system.file(package="moadqproject"), 'results/no_rules.rds'))
  no_rules$rule_id<-0
  saveRDS(no_rules, file.path(system.file(package="moadqproject"), 'results/no_rules.rds'))

  overview<-readRDS(file.path(system.file(package="moadqproject"), 'results/overview.rds'))
  overview[,-c(1:2)]<-0
  saveRDS(overview, file.path(system.file(package="moadqproject"), 'results/overview.rds'))

  progress<-read.table(file.path(system.file(package="moadqproject"), 'results/progress.txt'), header = TRUE)
  progress$status<-FALSE
  write.table(progress, file.path(system.file(package="moadqproject"), 'results/progress.txt'))

  table_count<-readRDS(file.path(system.file(package="moadqproject"), 'results/table_count.rds'))
  table_count$count<-0
  saveRDS(table_count, file.path(system.file(package="moadqproject"), 'results/table_count.rds'))

}
