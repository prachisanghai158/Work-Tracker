ui <-
  fluidPage(
    tags$head(tags$style(
      HTML(
        ".shiny-notification {
           position:fixed;
           top: calc(50%);
           left: calc(50%);
           }
           .Actual { background: #808080; }
           .Planned { background: #C0C0C0; }
           .vis-item .vis-item-overflow {overflow: visible;}
           "
      )
    )),
    
    
    dashboardPage(
      dashboardHeader(
        title = span(
          tagList(img(
            src = 'Merck.png',
            height = 50,
            width = 50
          )),
          "GCF Work Scoping, Planning and Tracking Tool"
          ,
          style = "color: white; font-size: 25px; font-weight: bold; font-family:Calibri"
        ),
        titleWidth = 500,
        tags$li(
          img(
            src = 'GCFLogoFinal (2).png',
            title = "Company Home",
            height = 50,
            width = 140
          ),
          class = "dropdown"
        )
      ),
      dashboardSidebar(
        width = 245,
        tags$head(tags$style(
          HTML(
            '.logo {background-color: #008080 !important;}
                                               .navbar {background-color: #008080 !important;}
                                               .main-header .logo {padding: 0px 0px;}'
          )
        )),
        sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",

          menuItem(
            h4(tags$b("Projects Scope and Plan")),
            tabName = "dataEntry",
            icon = icon("edit", "fa-2x")
          ),
          menuItem(
            h4(tags$b("Resource Plan and Actuals")),
            icon = icon("hourglass-half", "fa-2x"),
            tabName = "resoucePlanning"
          ),
          menuItem(
            h4(tags$b("Reports")),
            icon = icon("chart-bar", "fa-2x"),
            tabName = "reports"
          ),
          menuItem(
            h4(tags$b("Leadership Reports")),
            icon = icon("chart-bar", "fa-2x"),
            tabName = "gcfreports"
          ),
          menuItem(
            h4(tags$b("Glossary")),
            icon = icon("book", "fa-2x"),
            tabName = "glossary"
          ),
          menuItem(
            h4(tags$b("Backend")),
            tabName = "backend",
            icon = icon("edit", "fa-2x")
          )
        )#closing parantheses for sidebarMenu
      ),
      #closing paranthese for sidebar
      dashboardBody(
        tabItems(
          tabItem("dataEntry",
                  
                  tabsetPanel(
                    id = "abc",
                    tabPanel(
                      id = "pot",
                      
                      h4(tags$b("Add Scope")),
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        background = 'light-blue',
                        fluidRow(
                          column(
                            2,
                            selectizeInput(
                              inputId = "mainTeam",
                              label = "Select Team",
                              choices = c("", (as.vector(
                                unique(team_names$Team)
                              ))),
                              selected = NULL
                            )
                          ),
                          # column(2,selectizeInput(inputId = "Subteam", label = "Select Sub Team", choices = c("",as.vector(unique(team_information$Sub_Team))), selected = NULL)),
                          column(
                            2,
                            selectInput(
                              inputId = "Subteam",
                              label = "Select Sub Team",
                              choices = c(""),
                              selected = NULL
                            )
                          ),
                          actionButton(
                            "Save_scoping",
                            tags$b("Save"),
                            style = "color: #0a0a0a; background-color:  #ffe6e6; height:60px",
                            width = 100
                          ),
                          actionButton(
                            "clear_scoping",
                            tags$b("Clear"),
                            style = "color: #0a0a0a; background-color: #aee0e8; height:60px",
                            width = 100
                          )
                        )
                      ),
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        fluidRow(#column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
                          column(
                            2,
                            div(
                              style = "padding: 3px 3px; width: 250px; margin-top:-1em; margin-left:-3em,margin-right:-8em ; serif;font-weight: 20pt; font-size: 20pt;
                                                 text-shadow: 1px 1px 1px #aaa; line-height: 1;
                                                 color: #404040; ",
                              h5(tags$b("Enter Project Scope Details"))
                            )
                          )),
                        br(),
                        
                        
                        
                        fluidRow(
                          column(1, (
                            div(style = "padding: 0px 0px;margin-top:-1em; margin-right:-10em", h5(tags$b("")))
                          )),
                          column(
                            2,
                            div(style = "padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em", (textInput(
                              paste0("projectname_scope"), label = "Project Name"
                            )))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ", (
                              selectInput(
                                paste0("projectclassification_scope"),
                                label = "Classification",
                                choices = c("", unique(Projectclassification)),
                                selected = NULL
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ", (
                              selectInput(
                                paste0("targetaudience_scope"),
                                label = "Target Audience",
                                selected = NULL,
                                choices = c("", unique(Targetaudience))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ", (
                              selectInput(
                                paste0("geography_scope"),
                                multiple = TRUE,
                                label = "Geography",
                                selected = NULL,
                                choices = c("", geography_list)
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ", (
                              selectInput(
                                paste0("stakeholdergroup_scope"),
                                label = "Functional Group",
                                selected = NULL,
                                choices = c("", unique(Stakeholder))
                              )
                            ))
                          ),
                          column(
                            2,
                            div(style = "padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ", (
                              textInput(paste0("stakeholdernames_scope"), label = "Stakeholders")
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ", (
                              selectInput(
                                paste0("TA_scope"),
                                label = "TA",
                                selected = NULL,
                                choices = c("", TA)
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-3em ", (
                              selectInput(
                                paste0("Brand_scope"),
                                label = "Brand",
                                selected = NULL,
                                choices = c("", brand),
                                multiple = FALSE
                              )
                            ))
                          ),
                          column(
                            1,
                            div(
                              style = "padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-3em ",
                              selectInput(
                                paste0("selectedMonth_scope"),
                                "Project End Month",
                                choices = c("", month_list_scope),
                                selected = "Project End Month"[6]
                              )
                            )
                          )
                          
                          
                        )
                      ),
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        fluidRow(#column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
                          column(
                            2,
                            div(
                              style = "padding: 3px 3px; width: 250px; margin-top:-1em; margin-left:-3em,margin-right:-8em ; serif;font-weight: 20pt; font-size: 20pt;
                                                 text-shadow: 1px 1px 1px #aaa; line-height: 1;
                                                 color: #404040; ",
                              h5(tags$b("TASK PLANNING"))
                            )
                          )),
                        br(),
                        fluidRow(column(
                          2,
                          div(style = "padding: 3px 3px; width: 250px; margin-top:-1em; margin-left:-3em,margin-right:-8em ; serif;font-weight: 20pt; font-size: 20pt;
                                                 line-height: 1;
                                                 color: #404040; ", h5(
                                                   tags$b("Enter Task Details for the scope above")
                                                 ))
                        )),
                        br(),
                        
                        
                        fluidRow(
                          column(1, (
                            div(style = "padding: 0px 0px;margin-top:-1em; margin-right:-10em", h5(tags$b("")))
                          )),
                          column(
                            2,
                            div(style = "padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em", (
                              selectInput(
                                paste0("projectname_taskadd"),
                                label = "Project Name",
                                choices = "",
                                selected = ""
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ", (textInput(
                              paste0("details_taskadd"), label = "Task Details"
                            )))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ", (
                              selectInput(
                                paste0("category_taskadd"),
                                label = "Task Category",
                                choices = c("", unique(task_type))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ", (
                              selectizeInput(
                                paste0("priority_taskadd"),
                                label = "Priority",
                                choices = c("", priority)
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ", (
                              numericInput(paste0("fte_taskadd"), label = "Total FTE days", value = "")
                            ))
                          ),
                          column(
                            2,
                            div(style = "padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ", (
                              selectizeInput(
                                paste0("status_taskadd"),
                                label = "Status",
                                choices = c("", status)
                              )
                            ))
                          )
                          
                          
                          
                        ),
                        
                        fluidRow(tags$div(id = "myId_scoping")),
                        br(),
                        
                        fluidRow(
                          column(1, actionButton(
                            "AddRow_scoping", "", icon = icon("plus-square", "fa-3x")
                          ), offset = 0),
                          column(1, actionButton(
                            "delRow_scoping", "", icon = icon("minus-square", "fa-3x")
                          ), offset = 0)
                        )
                      )
                      
                    ),
                    
                    tabPanel(
                      id = "xyz",
                      
                      
                      h4(tags$b("Modify Scope")),
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        background = 'light-blue',
                        fluidRow(
                          column(
                            2,
                            selectInput(
                              label = "Select Team",
                              inputId = "mainTeam_modify",
                              choices = c("", (as.vector(
                                unique(team_names$Team)
                              ))),
                              selected = NULL
                            )
                          ),
                          column(
                            2,
                            selectInput(
                              inputId = "Subteam_modify",
                              label = "Select Sub Team",
                              choices = c("")
                            ),
                            selected = NULL
                          ),
                          actionButton("retrieve_modify", tags$b("Retrieve"), style =
                                         "color: #0a0a0a; background-color:  #ffe6e6; height:60px"),
                          actionButton("Save_modify", tags$b("Save Project"), style =
                                         "color: #0a0a0a; background-color: #aee0e8; height:60px"),
                          actionButton("del_modify", tags$b("Delete Project"), style =
                                         "color: #0a0a0a; background-color: #aee0e8; height:60px"),
                          actionButton(
                            "clear_modify",
                            tags$b("Clear Selection"),
                            style = "color: #0a0a0a; background-color: #aee0e8; height:60px",
                            width = 120
                          )
                        ),
                        br(),
                        fluidRow(column(
                          2,
                          div(
                            style = "padding: 5px 5px; width: 650px; margin-top:-1em; margin-left:-3em,margin-right:-8em ; serif;font-weight: 30pt;
                                                 font-size: 20pt;line-height: 1;color: #ffffff; text-shadow: 1px 1px 1px #aaa;",
                            width = 300,
                            h5(
                              tags$b(
                                "Filter previously defined project scope: (from below filters) and click RETRIEVE"
                              )
                            )
                          )
                        )),
                        br(),
                        
                        fluidRow(
                          column(1, (
                            div(style = "padding: 0px 0px;margin-top:-1em; margin-right:-10em", h5(tags$b("")))
                          )),
                          column(
                            2,
                            div(style = "padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em", (
                              selectInput(
                                paste0("projectname_modify"),
                                label = "Project Name",
                                selected = NULL ,
                                choices = c("", unique(pM[, 3]))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-8em", (
                              selectInput(
                                paste0("classification_modify"),
                                label = "Classification",
                                selected = NULL ,
                                choices = c("", unique(pM[, 4]))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-6em ", (
                              selectInput(
                                paste0("targetaudience_modify"),
                                label = "Target Audience",
                                selected = NULL ,
                                choices = c("", unique(pM[, 5]))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ", (
                              selectInput(
                                paste0("geography_modify"),
                                label = "Geography",
                                selected = NULL ,
                                choices = c("", unique(pM[, 6]))
                              )
                            ))
                          ),
                          
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ", (
                              selectInput(
                                paste0("stakeholdergroup_modify"),
                                label = "Functional Group",
                                selected = NULL ,
                                choices = c("", unique(pM[, 7]))
                              )
                            ))
                          ),
                          column(
                            2,
                            div(style = "padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ", (
                              selectInput(
                                paste0("stakeholdernames_modify"),
                                label = "Stakeholders",
                                selected = NULL ,
                                choices = c("", unique(pM[, 8]))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ", (
                              selectInput(
                                paste0("TA_modify"),
                                label = "TA",
                                selected = NULL ,
                                choices = c("", unique(pM[, 9]))
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-3em ", (
                              selectInput(
                                paste0("Brand_modify"),
                                label = "Brand",
                                selected = NULL ,
                                choices = c("", unique(pM[, 10])),
                                multiple = FALSE
                              )
                            ))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-3em ", (
                              selectInput(
                                paste0("selectedMonth_modify"),
                                label = "Project End Month",
                                choices = c("", month_list_scope),
                                selected = NULL
                              )
                            ))
                          )
                          
                          
                          
                          
                        )
                      ),
                      
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        fluidRow(column(
                          2,
                          div(style = "padding: 3px 3px; width: 250px; margin-top:-1em; margin-left:-3em,margin-right:-8em ; serif;font-weight: 20pt; font-size: 20pt;
                                                 line-height: 1;
                                                 color: #404040; ", h5(
                                                   tags$b("Select Projects to edit/modify scope")
                                                 ))
                        )),
                        br(),
                        
                        fluidRow(column(6, withSpinner((DT::dataTableOutput("temp1", width = 1500))
                        )))
                      ),
                      
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        fluidRow(column(
                          2,
                          div(style = "padding: 3px 3px; width: 250px; margin-top:-1em; margin-left:-3em,margin-right:-8em ; serif;font-weight: 20pt; font-size: 20pt;
                                                 line-height: 1;
                                                 color: #404040; ", h5(tags$b(
                                                   "Edit The Scope Details"
                                                 )))
                        )),
                        br(),
                        withSpinner(uiOutput("temp3UI")),
                        fluidRow(tags$div(id = "myId_planning"))
                      ),
                      
                      box(
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 600,
                        closable = FALSE,
                        fluidRow(column(
                          2,
                          div(
                            style = "padding: 3px 3px; width: 250px; margin-top:-0em; margin-left:-10em,margin-right:-8em ; serif;font-weight: 20pt; font-size: 20pt;
                                                 line-height: 0;
                                                 color: #404040; ",
                            h5(tags$b("TASK PLANNING")),
                            h5(tags$b("Add/Edit the Task Details"))
                          )
                        ),
                        column(
                          6,
                          actionButton("del_taskplan", tags$b("Delete Task"), style = "color: #0a0a0a; background-color:  #aee0e8; height:40px")
                        )),
                        br(),
                        
                        fluidRow(
                          column(
                            1,
                            align = "left",
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-right:-10em", h5(tags$b("")))
                          ),
                          column(
                            2,
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-left:-8em; width: 300px", h5(tags$b("Project Name")))
                          ),
                          column(
                            2,
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-left:-8em; width: 250px", h5(tags$b("Task Details")))
                          ),
                          column(
                            3,
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-left:-15em; width: 200px", h5(tags$b("Task Category")))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-left:-30em;margin-right:-10em width: 200px", h5(tags$b("Priority")))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-left:-30em; width: 100px", h5(tags$b("Total FTE days")))
                          ),
                          column(
                            1,
                            div(style = "padding: 0px 0px; margin-top:-1em; margin-left:-25em; width: 250px", h5(tags$b("Status")))
                          )
                        ),
                        br(),
                        uiOutput("taskUI"),
                        
                        
                        fluidRow(tags$div(id = "myId_taskplan")),
                        br(),
                        fluidRow(column(
                          1, actionButton("AddRow_taskplan", "", icon = icon("plus-square", "fa-3x")), offset = 0
                        )) 
                      )
                      
                      
                      
                      
                    )
                  )),
          
          
          
          tabItem(
            "resoucePlanning",
            
            tabsetPanel(
              id = "rsc",
              
              
              box(
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 600,
                closable = FALSE,
                background = 'light-blue',
                fluidRow(column(
                  2,
                  selectizeInput(
                    inputId = "rscmainTeam",
                    label = "Select Team",
                    choices = c("", (as.vector(
                      unique(team_names$Team)
                    ))),
                    selected = NULL
                  )
                ),
                column(
                  2,
                  selectInput(
                    inputId = "rscSubteam",
                    label = "Select Sub Team",
                    choices = c(""),
                    selected = NULL
                  )
                ))
              ),
              
              fluidRow(uiOutput("filter2")),
              
              
              fluidRow(
                uiOutput("boxes1"),
                uiOutput("boxes2"),
                
                column(
                  1,
                  offset = 0,
                  selectInput(
                    inputId = "rscWDmonth2",
                    label = "Select Month",
                    choices = c("", month_list_scope),
                    selected = NULL
                  )
                ),
                actionButton(
                  "rscRetrievePlan",
                  tags$b("Retrieve Plan"),
                  style = "color: #0a0a0a; background-color:  #ffe6e6; height:50px",
                  width = 200
                ),
                actionButton(
                  "rscUpdatePlan",
                  tags$b("Update Plan"),
                  style = "color: #0a0a0a; background-color:  #aee0e8; height:50px",
                  width = 200
                )
              ),
              
              
              
              
              br(),
              
              fluidRow(uiOutput("rscfilter2"))
              
            )
            
          ),
          
          
          tabItem("reports",
                  tabsetPanel(
                    tabPanel(
                      h4(tags$b("Leadership Reports")),
                      fluidRow(
                        column(
                          2,
                          selectizeInput(
                            inputId = "mainTeamreport",
                            label = "Select Team",
                            choices = c("", (as.vector(
                              unique(team_names$Team)
                            ))),
                            selected = NULL
                          )
                        ),
                        column(
                          2,
                          selectInput(
                            inputId = "Subteamreport",
                            label = "Select Sub Team",
                            choices = c(""),
                            selected = NULL
                          )
                        ),
                        column(
                          1,
                          selectInput(
                            inputId = "startreport",
                            label = "Start Month",
                            choices = c("", month_list_scope),
                            selected = NULL
                          )
                        ),
                        column(
                          1,
                          selectInput(
                            inputId = "endreport",
                            label = "End Month",
                            choices = c("", month_list_scope),
                            selected = NULL
                          )
                        ),
                        uiOutput("editstatus")
                        
                      ),
                      
                      
                      fluidRow(
                        uiOutput("changestatustable")),
                        
                        
                        wellPanel(
                          style = "background: #cdcdcd",
                          h2("Planned Projects", align = "center"),
                          fluidRow(column(width = 12, withSpinner((
                            div(
                              style = 'overflow-x: scroll;margin-top:-25em',
                              DT::dataTableOutput("Leadership_data_reports_tasks", width = 1500)
                            )
                          ))
                          ))),
                        
                        
                        # column(width = 12, withSpinner((
                        #   div(
                        #     style = 'overflow-x: scroll;margin-top:-25em',
                        #     DT::dataTableOutput("Leadership_data_reports_tasks", width = 1500)
                        #   )
                        # )))

                        
                        
                      wellPanel(
                        style = "background: #cdcdcd",
                        h2("Planned Tasks", align = "center"),
                        fluidRow(column(width = 12, withSpinner((
                          div(
                            style = 'overflow-x: scroll;margin-top:-25em',
                            DT::dataTableOutput("Leadership_data_reports", width = 1500)
                          )
                        ))
                        ))),
                        
                        
                      
                      # fluidRow(column(width = 12, withSpinner((
                      #   div(
                      #     style = 'overflow-x: scroll;margin-top:-25em',
                      #     DT::dataTableOutput("Leadership_data_reports", width = 1500)
                      #   )
                      # )))),
                      
                      
                      
                      
                      
                      
                      fluidRow(column(
                        plotOutput("Utilization_by_task_type"), width = 12
                      ))
                    )
                  )),
          
          tabItem("gcfreports",
                  tabsetPanel(
                    tabPanel(
                      h4(tags$b("Overall")),
                      
                      fluidRow(#column(2,selectizeInput( inputId ="mainTeamreport2",label = "Select Team", choices = c("",(as.vector(unique(team_names$Team)))), selected = NULL)),
                        
                        #column(2,selectInput( inputId ="Subteamreport2",label = "Select Sub Team", choices = c(""), selected = NULL)),
                        column(
                          1,
                          selectInput(
                            inputId = "startreport2",
                            label = "Start Month",
                            choices = c("", month_list_scope),
                            selected = NULL
                          )
                        ),
                        column(
                          1,
                          selectInput(
                            inputId = "endreport2",
                            label = "End Month",
                            choices = c("", month_list_scope),
                            selected = NULL
                          )
                        ))
                      ,
                      
                      br(),
                      
                      
                      fluidRow(column(
                        6, plotOutput("Utilization_by_proj_class_GCFall")
                      ),
                      
                      column(
                        6, plotOutput("Utilization_by_task_typeGCF_GCFall")
                      )),
                      
                      wellPanel(
                        style = "background: #cdcdcd",
                        h2("Projects under various Classifications", align = "center"),
                        fluidRow(column(12, ((DT::dataTableOutput("UTPCGCF_GCFall"))
                        )))
                      )
                      
                      ,
                      br(),
                      
                      wellPanel(
                        style = "background: #cdcdcd",
                        h2("Tasks under various Categories", align = "center"),
                        
                        fluidRow(column(12, ((DT::dataTableOutput("UTPCGCFtask_GCFall"))))))
                      
                      
                    ),
                    
                     
                    
                    tabPanel(
                      id = "fte",
                      h4(tags$b("By Teams")),
                      
                      fluidRow(
                        column(
                          2,
                          selectizeInput(
                            inputId = "mainTeamreport2",
                            label = "Select Team",
                            choices = c("", (as.vector(
                              unique(team_names$Team)
                            ))),
                            selected = NULL
                          )
                        ),
                        
                        #column(2,selectInput( inputId ="Subteamreport2",label = "Select Sub Team", choices = c(""), selected = NULL)),
                        column(
                          1,
                          selectInput(
                            inputId = "startreport3",
                            label = "Start Month",
                            choices = c("", month_list_scope),
                            selected = NULL
                          )
                        ),
                        column(
                          1,
                          selectInput(
                            inputId = "endreport3",
                            label = "End Month",
                            choices = c("", month_list_scope),
                            selected = NULL
                          )
                        ),
                        uiOutput("boxesfte"),
                        uiOutput("boxesfte2"),
                        uiOutput("boxesfte3")
                      )
                      ,
                      
                      fluidRow(column(
                        12, plotOutput("Utilization_by_proj_class")
                      )),
                      
                      fluidRow(column(
                        12, plotOutput("Utilization_by_task_typeGCF")
                      )),
                      
                      wellPanel(
                        style = "background: #cdcdcd",
                        h2("Projects under various Subteams", align = "center"),
                        fluidRow(column(12, ((DT::dataTableOutput("UTPCGCF"))
                        )))
                      )
                      
                     
                    )
                  )),
          
          
          tabItem(
            "backend",
            box(
              fluidRow(column(2, h4(
                tags$b("Update Dropdowns")
              ))),
              collapsible = TRUE,
              width = 600,
              background = 'light-blue',
              fluidRow(
                column(
                  2,
                  selectInput(
                    "backparameter",
                    "Select Variable",
                    selected = "",
                    choices = c("", colnames(dropdowns))
                  )
                ),
                column(2, selectInput(
                  "backlist", "Check existing list", choices = ""
                )),
                column(2, textInput("backentry", "Add New")),
                column(
                  2,
                  actionButton("Save_back", "Update List", style = "color: #0a0a0a; background-color: #aee0e8; height:60px")
                )
              )
            ),
            box(
              fluidRow(column(2, h4(
                tags$b("Update Team Information")
              ))),
              collapsible = TRUE,
              width = 600,
              background = 'light-blue',
              fluidRow(
                column(2, selectInput("backteam", "Select Team", choices = c("", (
                  as.vector(unique(team_names$Team))
                )))),
                column(
                  2,
                  selectInput("backsubteam", "Check existing Subteams", choices = "")
                ),
                column(2, textInput("backentrysub", "Add New")),
                column(
                  2,
                  actionButton("Save_back2", "Update Subteam List", style = "color: #0a0a0a; background-color: #aee0e8; height:60px")
                )
              ),
              fluidRow(
                #column(2,selectInput("backteam","Select Team",choices = colnames(dropdowns))),
                column(2, selectInput(
                  "backsubteam1", "Select Subteam", choices = ""
                )),
                column(
                  2,
                  selectInput("backemp", "Check existing Employees", choices = "")
                ),
                column(2, textInput("backentryemp", "Add New")),
                
                column(
                  2,
                  actionButton("Save_back3", "Update Employee list", style = "color: #0a0a0a; background-color: #aee0e8; height:60px")
                )
              ),
              fluidRow(
                #column(2,selectInput("backteam","Select Team",choices = colnames(dropdowns))),
                column(2, selectInput(
                  "backempdel", "Select Employee", choices = ""
                )),
                
                column(
                  2,
                  actionButton("Save_back4", "Remove Employee from list", style = "color: #0a0a0a; background-color: #f5a195; height:60px")
                )
              )
              
              
              
              
              
            )
          ),
          tabItem("glossary",
                  dataTableOutput("glossary"))
          
        )
      )#closing parantheses for dashboardBody
    )#closing parantheses for dashboardPage
  )#closing parantheses for fluidPage 