#' is_unique function
#'
#' This function allows you to bring the result of uniqueness check
#' @param
#' @keywords uniqueness
#' @return list of result and error messages
#' @import dplyr
#' @export
#' @examples
#' is_unique(x)


is_unique<-function(data){
  uniqueness_result<-readRDS('results/uniqueness.rds')

  result<-(uniqueness_result%>%filter(level==data$level & table==data$table & field==data$field))$result
  if(length(result)==0) {error_message=paste('field', data$field, 'in table', data$table, 'has no uniqueness rule')
  } else if(is.na(result)==TRUE){error_message=paste('field', data$field, 'in table', data$table, 'is not consistent')
  } else if(result==0){error_message=NA} else{error_message=paste('field', data$field, 'in table', data$table, 'has', result, '% duplicated data')}
  return(list(result, error_message))}
