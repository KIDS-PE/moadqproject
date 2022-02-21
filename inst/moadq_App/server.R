
shinyServer(function(input, output, session) {

  #### dashboard page
  {
    check_info<-readRDS("data/result/check_info.RDS")

    output$version_info<-renderUI({
      HTML(paste("Last check :", check_info$date,"<br> MOA DQM version", check_info$version))
    })

    observeEvent(input$run, {
      if(file.exists("data/result/con_info.RDS")==TRUE){
        con<-connect_DB()
        connection_status<-dbIsValid(con)
        if(connection_status==TRUE){
          moa_consistency(con)
          moa_tablerowindex(con)
          moa_completeness(con)
          moa_uniqueness(con)
          moa_validity(con)
          moa_accuracy(con)
          update_overview()
          progress<-read.table('data/result/progress.txt', header=TRUE)

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
      } else{showNotification("No valid connection information. Please check connection and try again! (see Configuration Page)", type="warning")}

    })

    progress<-read.table('data/result/progress.txt', header=TRUE)

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

    if(file.exists('data/result/dashboard_overview.rds')==TRUE){
      dashboard_overview<-readRDS('data/result/dashboard_overview.rds')

      output$dashboard_overview<-renderFormattable({
        formattable(dashboard_overview, list(
          consistency=formatter("span", style=x~ifelse(x<80, style(color="red", font.weight="bold"), NA)),
          uniqueness=formatter("span", style=x~ifelse(x<80, style(color="red", font.weight="bold"), NA)),
          completeness=formatter("span", style=x~ifelse(x<80, style(color="red", font.weight="bold"), NA)),
          validity=formatter("span", style=x~ifelse(x<80, style(color="red", font.weight="bold"), NA)),
          accuracy=formatter("span", style=x~ifelse(x<80, style(color="red", font.weight="bold"), NA)),
          total=color_tile("lightcoral", "lightgreen"))
        )})

    } else{
      output$dashboard_overview<-renderFormattable({
        formattable(as.data.frame(list("note"="No result ; Please click run DQM button")), list(
          note=formatter("span", style=x~ifelse(is.character(x), style(color="red", font.weight="bold"), NA)))
        )})
    }
  }

  #### level 1 page
  observeEvent(input$tabset1, {

    observeEvent(list(input$scdm_table, input$omop_table, input$lv1_rule), {

      if(input$tabset1=="SCDM"){
        if(file.exists('data/result/no_rules.rds')==TRUE){
          no_rules<-readRDS('data/result/no_rules.rds')
          no_rules_show<-(no_rules%>%filter(level==input$tabset1 & table==input$scdm_table))$rule_id
        } else {no_rules_show<-0}

        if(file.exists('data/result/table_count.rds')==TRUE){
          table_count<-readRDS('data/result/table_count.rds')
          table_count_show<-(table_count%>%filter(level==1 & table==input$scdm_table))$count
        } else {table_count_show<-0}

        score_show<-dashboard_overview%>%filter(level==input$tabset1 & table==input$scdm_table)

        lv1_table_name<-paste0('data/result/', tolower(input$lv1_rule), '.rds')
        if(file.exists(lv1_table_name)==TRUE){
          lv1_table_show<-readRDS(lv1_table_name)%>%filter(level==1 & table==input$scdm_table)
        } else {lv1_table_show<-NULL}


      } else {
        if(file.exists('data/result/no_rules.rds')==TRUE){
          no_rules<-readRDS('data/result/no_rules.rds')
          no_rules_show<-(no_rules%>%filter(level==input$tabset1 & table==input$omop_table))$rule_id
        } else {no_rules_show<-0}

        if(file.exists('data/result/table_count.rds')==TRUE){
          table_count<-readRDS('data/result/table_count.rds')
          table_count_show<-(table_count%>%filter(level==2 & table==input$omop_table))$count
        } else {table_count_show<-0}

        score_show<-dashboard_overview%>%filter(level==input$tabset1 & table==input$omop_table)

        lv1_table_name<-paste0('data/result/', tolower(input$lv1_rule), '.rds')
        if(file.exists(lv1_table_name)==TRUE){
          lv1_table_show<-readRDS(lv1_table_name)%>%filter(level==2 & table==input$omop_table)
        } else {lv1_table_show<-NULL}

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

  #### level 2 page
  plot_show<-readRDS('data/result/plot/plot1.rds')
  output$plot_show<-renderPlot(plot_show)

  ### configuration page
  {
      observeEvent(input$check_connection, {

      if(is.null(input$password)==TRUE){password.tmp=''} else{password.tmp=input$password}

      connectionDetails <- createConnectionDetails(dbms=input$dbtype,
                                                   server=paste0(input$host, '/', input$dbname),
                                                   port=input$port,
                                                   user=input$username,
                                                   password=password.tmp)
      con <- tryCatch(connect(connectionDetails), error=function(e){showNotification(paste0(e), type='err')})
      connection_status<-tryCatch(dbIsValid(con), error=function(e){showNotification("Connection is invalid", type='err')})
      if(connection_status==TRUE){showNotification("Connection is valid", type="message")}
      tryCatch(disconnect(con), error=function(e){ e })
      })

    observeEvent(input$save, {
      con_info<-list('site'=input$site, 'dbname'=input$dbname, 'dbtype'=input$dbtype,
                     'schemaname_lv1'=input$schemaname_lv1, 'schemaname_lv2'=input$schemaname_lv2, 'schemaname_vocab'=input$schemaname_vocab,
                     'host'=input$host, 'port'=input$port, 'user'=input$username, 'password'=input$password)
      saveRDS(con_info, 'data/result/con_info.RDS')
      showNotification("Configuration saved", type="message")
    })
  }
})
