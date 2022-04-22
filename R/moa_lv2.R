#' moa_lv2 function
#'
#' This function allows you to create a summary table and a graph for lv 2 dqm analysis
#' @param
#' @keywords lv2 analysis
#' @return rds file
#' @import DBI
#' @import DatabaseConnector
#' @import dplyr
#' @import SqlRender
#' @import ggplot2
#' @import scales
#' @export
#' @examples
#' moa_lv2(schema, table, contents, group, concept_id)

moa_lv2<-function(schema=NULL, input_table=NULL, input_contents=NULL, input_group=NULL, concept_id=NULL){

  con_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/con_info.rds'))
  mydbtype=tolower(con_info$dbtype)
  if(schema=='SCDM'){myschemaname=con_info$schemaname_lv1} else{myschemaname=con_info$schemaname_lv2}

  ind<-(plot_list%>%filter(table==input_table & table_name==input_contents & group==input_group))$plot_id
  sql<-translate((sql_list%>%filter(sql_id==plot_list$sql_id[ind] & level==schema))$sql, targetDialect = mydbtype)
  con<-connect_DB()

  tmp_table<-tryCatch({dbGetQuery(con, render(sql, A=myschemaname, B=input_table, C=plot_list$C[ind], E=plot_list$E[ind]))},
                      error=function(e){showNotification(paste0(e[1]), type='err')})

  if(plot_list$plot_id[ind]==1){
    table_lv2<-aggregate(Count~1+Gender, tmp_table, sum)%>%mutate(Prop=round(prop.table(Count)*100, 1))
    plot_lv2<-ggplot(table_lv2) + geom_bar(aes(x=factor(Gender), y=Count, fill=factor(Gender)), stat="identity") +
              scale_fill_discrete(name = "Gender")+scale_x_discrete(name='Gender')
  }

  if(plot_list$plot_id[ind]==2){
    table_lv2<-aggregate(Count~1+Birth_year, tmp_table, sum)%>%mutate(Prop=round(prop.table(Count)*100, 1))
    plot_lv2<-ggplot(table_lv2) + geom_line(aes(x=Birth_year, y=Count), stat="identity")
  }

  if(plot_list$plot_id[ind]==3){
    tmp_table<-tmp_table%>%mutate('Visit_date'=as.Date(paste0(Visit_year, '-', Visit_month, '-', '1')))
    table_lv2<-aggregate(Count~1+Visit_date, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) +
      geom_line(aes(x=Visit_date, y=Count),stat="identity")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
      theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==4){
    tmp_table<-tmp_table%>%mutate('Visit_date'=as.Date(paste0(Visit_year, '-', Visit_month, '-', '1')))
    table_lv2<-aggregate(Count~1+Visit_date+Gender, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) +
      geom_line(aes(x=Visit_date, y=Count, group=factor(Gender), color=factor(Gender)),stat="identity")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
      theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==5){
    tmp_table<-tmp_table%>%mutate('Visit_date'=as.Date(paste0(Visit_year, '-', Visit_month, '-', '1')))
    table_lv2<-aggregate(Count~1+Visit_date+Visit_type, tmp_table, sum)
    plot_lv2<-
      ggplot(table_lv2) +
      geom_line(aes(x=Visit_date, y=Count, group=factor(Visit_type), color=factor(Visit_type)),stat="identity")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
      theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==6){
    table_lv2<-as.data.frame(as.matrix(aggregate(Count~1+Visit_year+Visit_type, tmp_table, summary)))
    names(table_lv2)<-c('Visit_year', 'Visit_type', 'Min', "25%", "50%", "Mean", "75%", "Max")
    table_lv2[,-c(1:2)]<-round(table_lv2[,-c(1,2)])

    plot_lv2<-tmp_table%>%ggplot(aes(x=Count))+
      geom_histogram(position = 'identity', binwidth = 5) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+facet_grid(Visit_type~Visit_year)
  }

  if(plot_list$plot_id[ind]==7){
    table_lv2<-aggregate(Id~1+Visit_type+Visit_length, tmp_table, length); names(table2)[3]<-'Count'
    table_lv2<-table_lv2%>%mutate('Visit_length_bin'=cut(Visit_length, breaks = c(0,7,30, 90, 180, Inf),
                                                   label=c('[0, 7)', '[7, 30)', '[30, 90)', '[90, 180)', '[180+)'), right=FALSE))
    plot_lv2<-table_lv2%>%ggplot(aes(x=Visit_length_bin, y=Count, group=1))+ geom_bar(stat = 'identity')+
              scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~Visit_type)
  }

  if(plot_list$plot_id[ind]==8){
    tmp_table<-tmp_table%>%mutate('Enr_date'=as.Date(paste0(Enr_year, '-', Enr_month, '-', '01')),'Age'=Enr_year-Birth_year+1)
    tmp_table<-tmp_table%>%mutate('Age_group'=cut(Age, breaks = c(seq(0, 90, by=10), Inf),
                                                  label=c('0~9', '10~19', '20~29', '30~39', '40~49',
                                                          '50~59', '60~69', '70~79', '80~89', '90+'), right=FALSE))
    table_lv2<-aggregate(Count~Enr_date, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) +geom_line(aes(x=Enr_date, y=Count),stat="identity")+
              scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==9){
    tmp_table<-tmp_table%>%mutate('Enr_date'=as.Date(paste0(Enr_year, '-', Enr_month, '-', '01')),'Age'=Enr_year-Birth_year+1)
    tmp_table<-tmp_table%>%mutate('Age_group'=cut(Age, breaks = c(seq(0, 90, by=10), Inf),
                label=c('0~9', '10~19', '20~29', '30~39', '40~49','50~59', '60~69', '70~79', '80~89', '90+'), right=FALSE))
    table_lv2<-aggregate(Count~Enr_date+Gender, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) +
      geom_line(aes(x=Enr_date, y=Count, group=factor(Gender), color=factor(Gender)),stat="identity")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==10){
    tmp_table<-tmp_table%>%mutate('Enr_date'=as.Date(paste0(Enr_year, '-', Enr_month, '-', '01')),'Age'=Enr_year-Birth_year+1)
    tmp_table<-tmp_table%>%mutate('Age_group'=cut(Age, breaks = c(seq(0, 90, by=10), Inf),
                                                  label=c('0~9', '10~19', '20~29', '30~39', '40~49','50~59', '60~69', '70~79', '80~89', '90+'), right=FALSE))
    table_lv2<-aggregate(Count~Enr_date+Age_group, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) + geom_bar(aes(x=Enr_date, y=Count, fill=factor(Age_group)), position='stack', stat="identity")+
              scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+theme(axis.text.x = element_text(angle=45, hjust=1))+
              scale_fill_discrete(name = "Age group")
  }



  if(plot_list$plot_id[ind]==11){
    tmp_table<-tmp_table%>%mutate('Enr_date'=as.Date(paste0(Enr_year, '-', Enr_month, '-', '01')),'Age'=Enr_year-Birth_year+1)
    tmp_table<-tmp_table%>%mutate('Age_group'=cut(Age, breaks = c(seq(0, 90, by=10), Inf),
                 label=c('0~9', '10~19', '20~29', '30~39', '40~49','50~59', '60~69', '70~79', '80~89', '90+'), right=FALSE))
    table_lv2<-aggregate(Count~Enr_date, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) + geom_line(aes(x=Enr_date, y=Count),stat="identity")+
           scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
           theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==12){
    tmp_table<-tmp_table%>%mutate('Enr_date'=as.Date(paste0(Enr_year, '-', Enr_month, '-', '01')),'Age'=Enr_year-Birth_year+1)
    tmp_table<-tmp_table%>%mutate('Age_group'=cut(Age, breaks = c(seq(0, 90, by=10), Inf),
                                                  label=c('0~9', '10~19', '20~29', '30~39', '40~49','50~59', '60~69', '70~79', '80~89', '90+'), right=FALSE))
    table_lv2<-aggregate(Count~Enr_date+Gender, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) +
      geom_line(aes(x=Enr_date, y=Count, group=factor(Gender), color=factor(Gender)),stat="identity")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
      theme(axis.text.x = element_text(angle=45, hjust=1))
  }

  if(plot_list$plot_id[ind]==13){
    tmp_table<-tmp_table%>%mutate('Enr_date'=as.Date(paste0(Enr_year, '-', Enr_month, '-', '01')),'Age'=Enr_year-Birth_year+1)
    tmp_table<-tmp_table%>%mutate('Age_group'=cut(Age, breaks = c(seq(0, 90, by=10), Inf),
                                                  label=c('0~9', '10~19', '20~29', '30~39', '40~49','50~59', '60~69', '70~79', '80~89', '90+'), right=FALSE))
    table_lv2<-aggregate(Count~Enr_date+Age_group, tmp_table, sum)
    plot_lv2<-ggplot(table_lv2) +
      geom_bar(aes(x=Enr_date, y=Count, fill=factor(Age_group)),
               position='stack', stat="identity")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
      theme(axis.text.x = element_text(angle=45, hjust=1))+
      scale_fill_discrete(name = "Age group")
  }




  if(plot_list$plot_id[ind]==14){
    tmp_table<-tmp_table%>%mutate('Observation_length_group'=cut(Observation_length,
                  breaks = c(0, 3, 7, 30, 180, seq(365, max(Observation_length)+1, by=365)),right=FALSE,  dig.lab = 5))
    table_lv2<-as.data.frame(tmp_table%>% group_by(Observation_length_group) %>%summarise(Count = n()))
    plot_lv2<-table_lv2%>%ggplot(aes(x=Observation_length_group, y=Count))+ geom_bar(stat = 'identity') +
                                  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  }

  if(plot_list$plot_id[ind]==15){
    tmp_table<-tmp_table%>%mutate('Observation_length_group'=cut(Observation_length,
                                                                 breaks = c(0, 3, 7, 30, 180, seq(365, max(Observation_length)+1, by=365)),right=FALSE,  dig.lab = 5))
    table_lv2<-as.data.frame(tmp_table%>% group_by(Gender, Observation_length_group) %>%summarise(Count = n()))
    plot_lv2<-table_lv2%>%ggplot(aes(x=Observation_length_group, y=Count, fill=factor(Gender)))+
                                 geom_bar(position = 'stack', stat='identity') +
                                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                                 theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_discrete(name = "Gender")
  }

  if(plot_list$plot_id[ind]==16){
    tmp_table<-tmp_table%>%mutate('Observation_length_group'=cut(Observation_length,
                                                                 breaks = c(0, 3, 7, 30, 180, seq(365, max(Observation_length)+1, by=365)),right=FALSE,  dig.lab = 5))
    table_lv2<-as.data.frame(tmp_table%>% group_by(Birth_year, Observation_length_group) %>%summarise(Count = n()))
    plot_lv2<-table_lv2%>%ggplot(aes(x=Birth_year, y=Count, fill=factor(Observation_length_group)))+
                                 geom_bar(position = 'stack', stat='identity') +
                                 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                                 scale_fill_discrete(name = "Observation_length")
  }

  if(plot_list$plot_id[ind]%in%c(17:20)){
    table_lv2<-aggregate(Count~1, tmp_table, summary)
    table_lv2[,-c(1)]<-round(table_lv2[,-c(1)])
    plot_lv2<-tmp_table%>%ggplot(aes(x=Count))+geom_histogram(position = 'identity', binwidth = 5) +
                          scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
                          scale_x_continuous(name='Number of concepts, per person')
  }

  if(plot_list$plot_id[ind]>20){
    table_lv2<-NULL
    plot_lv2<-NULL
  }

  disconnect(con)

  return(list('table_lv2'=table_lv2, 'plot_lv2'=plot_lv2))
}
