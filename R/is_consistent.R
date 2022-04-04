#' is_consistent function
#'
#' This function allows you to bring the result of consistency check
#' @param
#' @keywords consistency
#' @return list of result and error messages
#' @import dplyr
#' @export
#' @examples
#' is_consistent(x)

is_consistent<-function(data){
  consistency_result<-readRDS('results/consistency.rds')
  result<-(consistency_result%>%filter(rule=='field name consistency' & level==data$level & table==data$table & field==data$field))$'result'
  if(result==TRUE){error_message=NA} else{error_message=paste('field', data$field, 'in table', data$table, 'is not consistent')}
  return(list(result, error_message))}
