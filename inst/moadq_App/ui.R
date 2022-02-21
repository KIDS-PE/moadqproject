ui.configuration<-{tabItem(tabName = 'Configuration',
                           fluidPage(
                             tags$head(tags$style(HTML('#check_connection{background-color:lightblue}'))),
                             tags$head(tags$style(HTML('#save{background-color:salmon}'))),
                             h1('Database connection'),
                             fluidRow(column(width=4, textInput(width='100%', "site", "site"))),
                             fluidRow(
                               column(width=2,textInput("dbname", "DB name")),
                               column(width=8,
                                      pickerInput(inputId='dbtype', label="DBMS", choices =
                                                    list('oracle', 'postgresql', 'redshift', 'sql server', 'pdw',
                                                         'netezza', 'bigquery', 'sqlite', 'sqlite extended', 'spark')), inline=TRUE)),
                             p(strong("Schema")),
                             fluidRow(
                               column(width=2, textInput("schemaname_lv1", "SCDM")),
                               column(width=5, textInput(width='100%', "schemaname_lv2", "OMOP Standardized clinical data")),
                               column(width=5, textInput(width='100%', "schemaname_vocab", "OMOP Standardized vocabularies"))),
                             fluidRow(
                               column(width=2, textInput("host", "Host")),
                               column(width=2, textInput("port", "Port"))),
                             fluidRow(
                               column(width=2, textInput("username", "Username")),
                               column(width=2,  passwordInput("pw", "Password   "))),
                             fluidRow(column(width=8),
                                      div(style="display:inline-block", actionButton("check_connection", "Test connection")),
                                      div(style="display:inline-block", actionButton("save", "SAVE")))
                           ))}
ui.dashboard<-{tabItem(tabName = 'Dashboard', class = "active", shinyUI(fluidPage(
  fluidRow(column(width=3, shinydashboard::valueBox("MOA CDM", "Data Quality Check", icon=icon("thumbs-up", lib="glyphicon"), color="light-blue", width=12)),
           column(width=4, offset=5, align="right",
                  fluidRow(htmlOutput("version_info")),
                  fluidRow(bsButton('run', "RUN DQM", style="danger", type="action", width=NULL, size="large")))),
  fluidRow(
    box(title="Progress", solidHeader = FALSE, width=12, collapsible = TRUE,
        fluidRow(
          uiOutput("box1"), uiOutput("box2"),
          tags$head(tags$style('#box1 .box-header {display: none}')),
          tags$head(tags$style('#box2 .box-header {display: none}')), style = "height:30px;font-size: 120%;font-weight: bold;"),
        fluidRow(
          uiOutput("box11"), uiOutput("box12"), uiOutput("box13"), uiOutput("box14"), uiOutput("box15"), uiOutput("box21"),
          tags$head(tags$style('#box11 .box-header {display: none}')),
          tags$head(tags$style('#box12 .box-header {display: none}')),
          tags$head(tags$style('#box13 .box-header {display: none}')),
          tags$head(tags$style('#box14 .box-header {display: none}')),
          tags$head(tags$style('#box15 .box-header {display: none}')),
          tags$head(tags$style('#box21 .box-header {display: none}')), style = "height:50px;font-size: 90%;")
    )),
  fluidRow(box(title="Overview", solidHeader=FALSE, width=12, collapsable=TRUE, formattableOutput("dashboard_overview")))
)))
}

ui.sidebar<-{sidebarMenu(
  menuItem("Dashboard", tabName="Dashboard"),
  menuItem("Level1", tabName = "Level1"),
  menuItem("Level2", tabName = "Level2"),
  menuItem("Configuration", tabName = "Configuration"),
  fluidRow(style="height:400px"),
  fluidRow(
    column(width=12,
           box(id="shortcut", title=span(icon("mouse-pointer"),"Shortcuts", style="font-size: 14px;font-weight: bold"),
               width=12, solidHeader = TRUE,
               fluidRow(actionButton(inputId="shortcut1", style='jelly', label="MOA-Net", width='80%',style= "color: #000000; background-color: #C7EFF3; border-color: #C7EFF3",
                                     onclick = sprintf("window.open('%s')", 'https://moa.drugsafe.or.kr'))),
               fluidRow(actionButton(inputId="shortcut2", label="OHDSI", width='80%',style= "color: #000000; background-color: #E9AF78; border-color: #E9AF78",
                                     onclick = sprintf("window.open('%s')", 'https://www.ohdsi.org/'))),
               fluidRow(actionButton(inputId="shortcut3", label="Athena", width='80%',style= "color: #000000; background-color: #9AC278; border-color: #9AC278",
                                     onclick = sprintf("window.open('%s')", 'https://athena.ohdsi.org/'))),
               fluidRow(actionButton(inputId="shortcut4", label="Sentinel Initative", width='80%',style= "color: #000000; background-color: #608ABA; border-color: #608ABA",
                                     onclick = sprintf("window.open('%s')", 'https://www.sentinelinitiative.org/')))),
           tags$head(tags$style('#shortcut .box-header {background: #D3D3D3; text-align: left}'))), align='center')
)}

ui.lv1<-{tabItem(tabName="Level1", shinyUI(fluidPage(
  fluidRow(
    column(width=3,
           tabBox(title=NULL, width=NULL, id="tabset1",
                  tabPanel("SCDM", div(style = 'overflow-y:scroll;height:212px;',
                                       prettyRadioButtons(inputId = "scdm_table", label = "Choose:", icon = icon("check"), bigger = TRUE, status = "info", animation = "jelly",
                                                          choices = c("Demographic", "Enrollment", "Dispensing", "Encounter", "Diagnosis", "Procedure", "Laboratory_result", "Vital_signs")))),
                  tabPanel("OMOP", div(style = 'overflow-y:scroll;height:212px;',
                                       prettyRadioButtons(inputId = "omop_table", label = "Choose:", icon = icon("check"), bigger = TRUE, status = "info", animation = "jelly",
                                                          choices = c("person", "observation_period", "drug_exposure", "visit_occurrence", "condition_occurrence", "procedure_occurrence", "measurement"))))
           )),
    column(width=9,
           fluidRow(infoBoxOutput("lv1_box1", width=3), infoBoxOutput("lv1_box2", width=3), infoBoxOutput("lv1_box3", width=3)),
           fluidRow(
             box(title="Consistency", gaugeOutput("g1"), style = "height:130px;", width=2),
             box(title="Uniqueness", gaugeOutput("g2"), style = "height:130px;", width=2),
             box(title="Completeness", gaugeOutput("g3"), style = "height:130px;", width=2),
             box(title="Validity", gaugeOutput("g4"), style = "height:130px;", width=2),
             box(title="Accuracy", gaugeOutput("g5"), style = "height:130px;", width=2)
           ))),
  fluidRow(box(width=12, title="Result",
               fluidRow(column(3, pickerInput(inputId = "lv1_rule", label=NULL,
                                              choices = c('Consistency', 'Uniqueness', 'Completeness', 'Validity', 'Accuracy'),
                                              options=list('actions-box'=TRUE), multiple=FALSE)),
                        column(9, downloadBttn(outputId = "lv1_download", style="fill", color="primary"), align='right')),
               fluidRow(column(width=12, DT::dataTableOutput("lv1_table")))
  ))
)))}

ui.lv2<-{tabItem(tabName="Level2", shinyUI(fluidPage(
  fluidRow(
    column(width=3,
           tabBox(title=NULL, width=NULL, id="tabset2",
                  tabPanel("SCDM", div(style = 'overflow-y:scroll;height:212px;',
                                       prettyRadioButtons(inputId = "scdm_table", label = "Choose:", icon = icon("check"), bigger = TRUE, status = "info", animation = "jelly",
                                                          choices = c("Demographic", "Enrollment", "Dispensing", "Encounter", "Diagnosis", "Procedure", "Laboratory_result", "Vital_signs")))),
                  tabPanel("OMOP", div(style = 'overflow-y:scroll;height:212px;',
                                       prettyRadioButtons(inputId = "omop_table", label = "Choose:", icon = icon("check"), bigger = TRUE, status = "info", animation = "jelly",
                                                          choices = c("person", "observation_period", "drug_exposure", "visit_occurrence", "condition_occurrence", "procedure_occurrence", "measurement"))))
           ))),
  fluidRow(plotOutput('plot_show', width="100%"))
)))}

shinyUI(dashboardPage(dashboardHeader(title="MOA CDM DQM"), dashboardSidebar(ui.sidebar),
                   dashboardBody(tabItems(ui.dashboard, ui.lv1, ui.lv2, ui.configuration))))
