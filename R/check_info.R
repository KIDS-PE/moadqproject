#' check_info function
#'
#' This function allows you to save information on dqm check process
#' @param
#' @keywords check
#' @return check_info.RDS
#' @export
#' @examples
#' check_info()

check_info<-function(){
  check_info<-list("date"=Sys.time(), "version"="1.0")
  saveRDS(check_info, "data/result/check_info.RDS")
}

