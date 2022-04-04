#' is_complete function
#'
#' This function allows you to bring the result of completeness check
#' @param
#' @keywords completeness
#' @return list of result and error messages
#' @import dplyr
#' @export
#' @examples
#' is_complete(x)

is_complete<-function(data){

  completeness_result<-readRDS('results/completeness.rds')

  result<-(completeness_result%>%filter(level==data$level & table==data$table & field==data$field))$'result'
  if(length(result)==0) {error_message=paste('field', data$field, 'in table', data$table, 'has no completeness rule')
  } else if(is.na(result)==TRUE){error_message=paste('field', data$field, 'in table', data$table, 'is not consistent')
  } else if(result==0){error_message=NA}else{error_message=paste('field', data$field, 'in table', data$table, 'has', result, '% incomplete data')}
  return(list(result, error_message))}
