
shinyServer(function(input, output, session) {

  #### dashboard page
  {

    check_info<-readRDS(file.path(system.file(package="moadqproject"), 'results/check_info.rds'))

    output$version_info<-renderUI({
      HTML(paste("Last check :", check_info$date,"<br> MOA DQM version", check_info$version))
    })

    observeEvent(input$run, {
        con<-tryCatch(connect_DB(), error=function(e){showNotification(paste0(e[1]), type='err')})
        connection_status<-tryCatch(dbIsValid(con), error=function(e){showNotification(paste0('Connection information is invalid'), type='err')})
        if(connection_status==TRUE){
          moa_consistency()
          moa_tablerowindex()
          moa_completeness()
          moa_uniqueness()
          moa_validity()
          moa_accuracy()
          update_overview()

          progress<-read.table(file.path(system.file(package="moadqproject"), 'results/progress.txt'))

          progress$col<-'gray'
          progress$col[which(progress$status==TRUE)]<-'teal'

          output$box1<-renderUI({box(id="box1", title=NULL, width=10, align="center", "Level 1", headerBorder = FALSE, background = 'navy')})
          output$box2<-renderUI({box(id="box2", title=NULL, width=2, align="center", "Level 2", headerBorder = FALSE, background = 'navy')})

          output$box11<-renderUI({box(id="box11", title=NULL, width=2, align="center", "Consistency", headerBorder = FALSE, background = progress$col[which(progress$step=='1-1')])})
          output$box12<-renderUI({box(id="box12", title=NULL, width=2, align="center", "Uniqueness", headerBorder = FALSE, background = progress$col[which(progress$step=='1-2')])})
          output$box13<-renderUI({box(id="box13", title=NULL, width=2, align="center", "Completeness", headerBorder = FALSE, background = progress$col[which(progress$step=='1-3')])})
          output$box14<-renderUI({box(id="box14", title=NULL, width=2, align="center", "Validity", headerBorder = FALSE, background = progress$col[which(progress$step=='1-4')])})
          output$box15<-renderUI({box(id="box15", title=NULL, width=2, align="center", "Accuracy", headerBorder = FALSE, background = progress$col[which(progress$step=='1-5')])})
          output$box21<-renderUI({box(id="box21", title=NULL, width=2, align="center", "Distribution", headerBorder = FALSE, background = progress$col[which(progress$step=='2-1')])})


          output$version_info<-renderUI({
            HTML(paste("Last check :", check_info$date,"<br> MOA DQM version", check_info$version))
          })

        } else{showNotification("No valid connection information. Please check connection and try again! (see Configuration Page)", type="warning")}

    })

    progress<-read.table(file.path(system.file(package="moadqproject"), 'results/progress.txt'), header=TRUE)
    progress$col<-'gray'
    progress$col[which(progress$status==TRUE)]<-'teal'

    output$box1<-renderUI({box(id="box1", title=NULL, width=10, align="center", "Level 1", headerBorder = FALSE, background = 'navy')})
    output$box2<-renderUI({box(id="box2", title=NULL, width=2, align="center", "Level 2", headerBorder = FALSE, background = 'navy')})

    output$box11<-renderUI({box(id="box11", title=NULL, width=2, align="center", "Consistency", headerBorder = FALSE, background = progress$col[which(progress$step=='1-1')])})
    output$box12<-renderUI({box(id="box12", title=NULL, width=2, align="center", "Uniqueness", headerBorder = FALSE, background = progress$col[which(progress$step=='1-2')])})
    output$box13<-renderUI({box(id="box13", title=NULL, width=2, align="center", "Completeness", headerBorder = FALSE, background = progress$col[which(progress$step=='1-3')])})
    output$box14<-renderUI({box(id="box14", title=NULL, width=2, align="center", "Validity", headerBorder = FALSE, background = progress$col[which(progress$step=='1-4')])})
    output$box15<-renderUI({box(id="box15", title=NULL, width=2, align="center", "Accuracy", headerBorder = FALSE, background = progress$col[which(progress$step=='1-5')])})
    output$box21<-renderUI({box(id="box21", title=NULL, width=2, align="center", "Distribution", headerBorder = FALSE, background = progress$col[which(progress$step=='2-1')])})

    if(file.exists(file.path(system.file(package="moadqproject"), 'results/overview.rds'))==TRUE){
    overview<-readRDS(file.path(system.file(package="moadqproject"), 'results/overview.rds'))
    } else{ overview<-data.frame(list('consistency'=0, 'uniqueness'=0, 'completeness'=0, 'validity'=0, 'accuracy'=0, 'total'=0))}

      output$overview<-renderFormattable({
        formattable(overview, list(
          consistency=formatter("span", style=x~ifelse(x<80, formattable::style(color="red", font.weight="bold"), NA)),
          uniqueness=formatter("span", style=x~ifelse(x<80, formattable::style(color="red", font.weight="bold"), NA)),
          completeness=formatter("span", style=x~ifelse(x<80, formattable::style(color="red", font.weight="bold"), NA)),
          validity=formatter("span", style=x~ifelse(x<80, formattable::style(color="red", font.weight="bold"), NA)),
          accuracy=formatter("span", style=x~ifelse(x<80, formattable::style(color="red", font.weight="bold"), NA)),
          total=color_tile("lightcoral", "lightgreen"))
        )})
  }

  #### level 1 page
  {
  no_rules<-readRDS(file.path(system.file(package="moadqproject"), 'results/no_rules.rds'))
  table_count<-readRDS(file.path(system.file(package="moadqproject"), 'results/table_count.rds'))

  observeEvent(input$tabset1, {

    observeEvent(list(input$scdm_table, input$omop_table, input$lv1_rule), {

      if(input$tabset1=="SCDM"){
        no_rules_show<-(no_rules%>%filter(level==input$tabset1 & table==input$scdm_table))$rule_id
        table_count_show<-(table_count%>%filter(level==1 & table==input$scdm_table))$count
        score_show<-overview%>%filter(level==input$tabset1 & table==input$scdm_table)
        lv1_table<-readRDS(file.path(system.file(package="moadqproject"), 'results/', paste0(tolower(input$lv1_rule), '.rds')))
        lv1_table_show<-lv1_table%>%filter(level==1 & table==input$scdm_table)


      } else {
        no_rules_show<-(no_rules%>%filter(level==input$tabset1 & table==input$omop_table))$rule_id
        table_count_show<-(table_count%>%filter(level==2 & table==input$omop_table))$count
        score_show<-overview%>%filter(level==input$tabset1 & table==input$omop_table)
        lv1_table<-readRDS(file.path(system.file(package="moadqproject"), 'results/', paste0(tolower(input$lv1_rule), '.rds')))
        lv1_table_show<-lv1_table%>%filter(level==2 & table==input$omop_table)

      }

      output$lv1_box1<-renderInfoBox({infoBox("# OF ROWS CHECKED", table_count_show, icon=icon('database'), color="purple")})
      output$lv1_box2<-renderInfoBox({infoBox("# OF RULES", no_rules_show, icon=icon('search'), color="orange")})
      output$lv1_box3<-renderInfoBox({infoBox("TOTAL SCORE", paste0(score_show$total, '%'), icon=icon('thumbs-up'), color="light-blue")})

      output$g1<-renderGauge({gauge(score_show$consistency, min=0, max=100, symbol="%", label="", sectors=gaugeSectors(success = c(80, 100), warning = c(30, 79), danger = c(0, 29)))})
      output$g2<-renderGauge({gauge(score_show$uniqueness, min=0, max=100, symbol="%", label="", sectors=gaugeSectors(success = c(80, 100), warning = c(30, 79), danger = c(0, 29)))})
      output$g3<-renderGauge({gauge(score_show$completeness, min=0, max=100, symbol="%", label="", sectors=gaugeSectors(success = c(80, 100), warning = c(30, 79), danger = c(0, 29)))})
      output$g4<-renderGauge({gauge(score_show$validity, min=0, max=100, symbol="%", label="", sectors=gaugeSectors(success = c(80, 100), warning = c(30, 79), danger = c(0, 29)))})
      output$g5<-renderGauge({gauge(score_show$accuracy, min=0, max=100, symbol="%", label="", sectors=gaugeSectors(success = c(80, 100), warning = c(30, 79), danger = c(0, 29)))})

      output$lv1_table<-DT::renderDataTable(lv1_table_show)

      output$lv1_download<-downloadHandler(
        filename=function(){paste('data-', Sys.Date(), '.csv', sep='')},
        content=function(file){lv1_table%>%write.csv(file, row.names = FALSE)}
      )

    })

  })

  }

  #### level 2 page

  output$lv2_table<-NULL
  output$lv2_plot<-NULL

  observeEvent(input$tabset2, {observeEvent(list(input$scdm_table_lv2, input$omop_table_lv2), {
    if(input$tabset2=='SCDM'){
      input_table<-input$scdm_table_lv2
      choose_table_list<-(plot_list%>%filter(table==input$scdm_table_lv2)%>%select(table_name)%>%unique())$table_name
      output$choose_table<-renderUI({
        selectInput("select_contents", "Contents",choose_table_list, multiple = TRUE, selectize = FALSE, size=13, width='100%')})
    } else{
      input_table<-input$omop_table_lv2
      choose_table_list<-(plot_list%>%filter(table==input$omop_table_lv2)%>%select(table_name)%>%unique())$table_name
      output$choose_table<-renderUI({
        selectInput("select_contents", "Contents",choose_table_list, multiple = TRUE, selectize = FALSE, size=13, width='100%')})
    }

  })})

  observeEvent(input$select_contents, {
    if(input$tabset2=='SCDM'){
      choose_group_list<-(plot_list%>%filter(table_name==input$select_contents)%>%select(group)%>%unique())$group
      output$choose_group<-renderUI({
        selectInput("select_group", "Group",choose_group_list, multiple = TRUE, selectize = FALSE, size=13)})
    } else{
      choose_group_list<-(plot_list%>%filter(table_name==input$select_contents)%>%select(group)%>%unique())$group
      output$choose_group<-renderUI({
        selectInput("select_group", "Group",choose_group_list, multiple = TRUE, selectize = FALSE, size=13)})
    }

  })

  observeEvent(input$run_lv2, {
    if(input$tabset2=='SCDM'){input_table<-input$scdm_table_lv2} else{input_table<-input$omop_table_lv2}
    lv2_res<-moa_lv2(input_schema=input$tabset2, input_table=input_table, input_contents=input$select_contents,
                     input_group=input$select_group, concept_id=input$concept_id)
    output$lv2_table<-DT::renderDataTable(lv2_res$table_lv2)
    output$lv2_plot<-renderPlotly(lv2_res$plot_lv2)
  })

  ### configuration page
  {
      observeEvent(input$check_connection, {

      if(is.null(input$password)==TRUE){password.tmp=''} else{password.tmp=input$password}
      tryCatch({
       connectionDetails <- createConnectionDetails(dbms=input$dbtype,
                                                    server=paste0(input$host, '/', input$dbname),
                                                    port=input$port,
                                                    user=input$username,
                                                    password=password.tmp,
                                                    pathToDriver = input$jdbcDrivers)
       con <- connect(connectionDetails)
       connection_status<-dbIsValid(con)}, error=function(e){ showNotification(paste0(e[1]), type='err')})
      if(exists('connection_status')==TRUE){
      if(connection_status==TRUE){showNotification("Connection is valid", type="message")
        } else {showNotification("Connection is invalid", type='err')}} else {showNotification("Connection is invalid", type='err')}
      tryCatch(disconnect(con), error=function(e){ e })
      })

    observeEvent(input$save, {
      con_info<-list('site'=input$site, 'dbname'=input$dbname, 'dbtype'=input$dbtype,
                     'schemaname_lv1'=input$schemaname_lv1, 'schemaname_lv2'=input$schemaname_lv2, 'schemaname_vocab'=input$schemaname_vocab,
                     'host'=input$host, 'port'=input$port, 'user'=input$username, 'password'=input$password, 'jdbcDrivers'=input$jdbcDrivers)
      saveRDS(con_info, file.path(system.file(package="moadqproject"), 'results/con_info.rds'))
      showNotification("Saved information", type="message")
    })

    observeEvent(input$del, {
      con_info<-list('site'='', 'dbname'='', 'dbtype'='',
                     'schemaname_lv1'='', 'schemaname_lv2'='', 'schemaname_vocab'='',
                     'host'='', 'port'='', 'user'='', 'password'='')
      saveRDS(con_info, file.path(system.file(package="moadqproject"), 'results/con_info.rds'))
      showNotification("Removed information", type="message")
    })
  }
})
