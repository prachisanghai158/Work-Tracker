server <- function(input, output, session) 
{

  source('vectors_list.R',local=TRUE)

 
  team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)
  taskresource<<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
  
  vals <- reactiveValues(x = NULL)
  refresh_check <- reactiveValues()
  leaves<<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
  team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)
  fteplanned<<-0
  idx<<-0
  fte<<-0
  fteactual<<-0
  dropdowns<-read.csv("./data/dropdowns.csv", header = TRUE, stringsAsFactors = FALSE)
  dropdowns[is.na(dropdowns) | dropdowns=="" ]<-"None"
  globaladhoc<<-c()

  team_names <- data.frame("Team"= c(unique(team_information$Team)))
  RVname<- reactiveValues(data = NULL)
  RVxyz<- reactiveValues(data = NULL)
  
  projectsMain <<-read.csv("./data/Projects.csv",header = TRUE, stringsAsFactors = FALSE)
  
  RV <- reactiveValues(data = NULL)
  RVpkk<-reactiveValues(data = NULL)
  RV2 <- reactiveValues(data = NULL)
  RVrsctemp1 <- reactiveValues(data = NULL)
  RVrsctemp2 <- reactiveValues(data = NULL)
  globalpk<<-NULL
  thispk<<-NULL
  RVrsctemp2edited<-reactiveValues(data = NULL)
  rowsum1<<-NULL
  Total<<-NULL
  
  a<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
  colnames(a)<-c("Holidays+Weekends","Working Days")
  
  
  
  # RVprojectnameid<-reactiveValues(data = NULL)
  # RVprojectnameid$data<-"input$projectname_taskadd"
  # RVprojectnameid$data2<-reactiveValues(data = NULL)
  # 
  # 
  # observeEvent(input$projectname_taskadd,{
  # RVprojectnameid$data2<-(eval(parse(text = paste0("input$projectname_taskadd"))))
  # })
  
  outputId<<-""
  
  RVholWD<-reactiveValues(data = a)
  #DYNAMICALLY UPDATE SUBTEAM OPTIONS BASED ON TEAM SELECTION
  tasksMain <<-read.csv("./data/Tasks.csv",header = TRUE, stringsAsFactors = FALSE)
  
  selproject<<-NULL
  
  observeEvent(list(input$mainTeam), {
    filtered <- team_information[team_information$Team == input$mainTeam,]
    updateSelectInput(session,"Subteam", choices = c(as.vector(unique(filtered[,4]))), selected = '')
  })
  
  
  observeEvent(list(input$mainTeam_modify), {
    filtered <- team_information[team_information$Team == input$mainTeam_modify,]
    updateSelectInput(session,"Subteam_modify", choices = c(as.vector(unique(filtered[,4]))), selected = '')
  })
  
  #### Dynamic Dropdowns
  
  observeEvent(list(input$mainTeam_modify,input$Subteam_modify,input$projectname_modify,input$classification_modify,input$targetaudience_modify,paste(input$geography_modify, collapse = ','),input$stakeholdergroup_modify,
                    paste(input$stakeholdernames_modify, collapse = ','),paste(input$TA_modify, collapse = ','),paste(input$Brand_modify, collapse = ','),input$selectedMonth_modify), {
                      
                      
                      
                      
                      
                      list<- c(input$mainTeam_modify,input$Subteam_modify,input$projectname_modify,input$classification_modify,input$targetaudience_modify,paste(input$geography_modify, collapse = ','),input$stakeholdergroup_modify,
                               paste(input$stakeholdernames_modify, collapse = ','),paste(input$TA_modify, collapse = ','),paste(input$Brand_modify, collapse = ','),input$selectedMonth_modify)
                      
                      
                      tempfile<- projectsMain[projectsMain$Project_Name!="Adhoc",]
                      
                      
                      for(i in 1:length(list) )
                      {
                        if(list[i]!="")
                        {
                          
                          tempfile<- tempfile[tempfile[i]==list[i],]
                          
                          
                        }
                        
                        
                      }
                      
                      
                      updateSelectInput(session,"mainTeam_modify", choices = c(as.vector(unique(tempfile[,1]))), selected = ifelse(input$mainTeam_modify %in% unique(tempfile[,1]),input$mainTeam_modify,""))
                      updateSelectInput(session,"Subteam_modify", choices = c(as.vector(unique(tempfile[,2]))), selected = ifelse(input$Subteam_modify %in% unique(tempfile[,2]),input$Subteam_modify,""))
                      updateSelectInput(session,"projectname_modify", choices = c(as.vector(unique(tempfile[,3]))), selected = ifelse(input$projectname_modify %in% unique(tempfile[,3]),input$projectname_modify,""))
                      updateSelectInput(session,"classification_modify", choices = c(as.vector(unique(tempfile[,4]))), selected = ifelse(input$classification_modify %in% unique(tempfile[,4]),input$classification_modify,""))
                      updateSelectInput(session,"targetaudience_modify", choices = c(as.vector(unique(tempfile[,5]))), selected = ifelse(input$targetaudience_modify %in% unique(tempfile[,5]),input$targetaudience_modify,""))
                      updateSelectInput(session,"geography_modify", choices = c(as.vector(unique(tempfile[,6]))), selected = ifelse(paste(input$geography_modify,collapse = ',') %in% unique(tempfile[,6]),paste(input$geography_modify,collapse = ','),""))
                      updateSelectInput(session,"stakeholdergroup_modify", choices = c(as.vector(unique(tempfile[,7]))), selected = ifelse(input$stakeholdergroup_modify %in% unique(tempfile[,7]),input$stakeholdergroup_modify,""))
                      updateSelectInput(session,"stakeholdernames_modify", choices = c(as.vector(unique(tempfile[,8]))), selected = ifelse(paste(input$stakeholdernames_modify,collapse = ',') %in% unique(tempfile[,8]),paste(input$stakeholdernames_modify,collapse = ','),""))
                      updateSelectInput(session,"TA_modify", choices = c(as.vector(unique(tempfile[,9]))), selected = ifelse(input$TA_modify %in% unique(tempfile[,9]),input$TA_modify,""))
                      updateSelectInput(session,"Brand_modify", choices = c(as.vector(unique(tempfile[,10]))), selected = ifelse(input$Brand_modify %in% unique(tempfile[,10]),input$Brand_modify,""))
                      updateSelectInput(session,"selectedMonth_modify", choices = c(as.vector(unique(tempfile[,11]))), selected = ifelse(input$selectedMonth_modify %in% unique(tempfile[,11]),input$selectedMonth_modify,""))
                      
                      
                      
                    })
  
  ####Retrieve rable
  
  observeEvent(input$retrieve_modify,{
    
    
  
    list2<- c(input$mainTeam_modify,input$Subteam_modify,input$projectname_modify,input$classification_modify,input$targetaudience_modify,paste(input$geography_modify,collapse = ','),input$stakeholdergroup_modify,
              paste(input$stakeholdernames_modify,collapse = ','),input$TA_modify,input$Brand_modify,input$selectedMonth_modify,input$status_modifying2)
    
    table1<-projectsMain[,c(1:11,14)]
    
    for( i in 1:length(list2))
    {
      if(list2[i]!=""){
        
        table1<-table1[table1[,i] == list2[i],]
        
      }
    }
    #  
    
    RV$data <- table1
    
  })
  
  observeEvent(input$temp1_rows_selected,{
    
    
   ####
    a<-as.numeric(input$temp1_rows_selected)
    x<-RV$data[a,]
    selproject <<- RV$data[a,]
    
    #choices = c(as.vector(unique(Projectclassification))),
    updateTextInput(session,"projectname_modifying2", value = as.character(x[3]))
    
    updateSelectInput(session,paste0("projectclassification_modifying2") ,choices = c("",unique(Projectclassification)),selected = as.character(x[4]))
    
    updateSelectInput(session,"targetaudience_modifying2", choices = c("",unique(Targetaudience)), selected = as.character(x[5]))
    
    updateSelectInput(session,"geography_modifying2", choices = c("",as.character(x[6]),geography_list), selected = as.character(x[6]))
    
    updateSelectInput(session,"stakeholdergroup_modifying2", choices = c("",unique(Stakeholder)), selected = as.character(x[7]))
    
    updateSelectInput(session,"stakeholdernames_modifying2", choices = c("",as.character(x[8]),unique(projectsMain[,8])), selected = as.character(x[8]))
    
    updateSelectInput(session,"TA_modifying2", choices =c("", as.character(x[9]),TA), selected = as.character(x[9]))
    

    updateSelectInput(session,"Brand_modifying2", choices = c("",as.character(x[10]),brand), selected = as.character(x[10]))
    
    updateSelectInput(session,"selectedMonth_modifying2", choices = c("",month_list_scope), selected = as.character(x[11]))
    
    updateSelectInput(session,"status_modifying2", choices = c("",unique(status)), selected = x[12])
    
    #updateSelectInput(session,paste0("projectclassification_scope"),label="Classification",choices=c("",unique(Projectclassification)),selected=NULL)
    
    
    
    
    
  })
  
  ###Save changes in modify
  
  
  observeEvent(input$Save_modify,{
    
    
 
    
   
    rown<-as.numeric(rownames(selproject))
    newp<-c(selproject[1,1],selproject[1,2],input$projectname_modifying2,input$projectclassification_modifying2,input$targetaudience_modifying2,paste(input$geography_modifying2,collapse = ','),
            input$stakeholdergroup_modifying2,paste(input$stakeholdernames_modifying2,collapse = ','),paste(input$TA_modifying2, collapse = ','),paste(input$Brand_modifying2, collapse = ','),input$selectedMonth_modifying2) 
    
    pknew<-as.character("")
    
    
     
    
    for( i in 1:length(newp)){
      
      
      pknew<-paste0(pknew,newp[i],sep = "_")
      
    } 
    
    
    
    pknew<-paste0(pknew,input$status_modifying2,sep = "_")
    pknew<-paste0(pknew,projectsMain[rown,13],sep = "")
    newp<-c(newp,pknew)
    
    if((!grepl("_",input$projectname_modifying2,  fixed=TRUE) & !grepl("_",input$stakeholdernames_modifying2,  fixed=TRUE)) ){
    if(! ""%in% newp){
      
      oldpk<-projectsMain[rown,12]
      ##
      projectsMain[rown,]<-c(newp,projectsMain[rown,13],input$status_modifying2)
      
      
      
      
      
     
      
      
      rowT<-nrow(RV2$data)
      
      lis<-as.numeric(rownames(RV2$data))
      
      
      e <- as.data.frame(matrix(0, ncol = 9, nrow = rowT))
      check<-c()
      if(rowT!=0){
        for(i in 1:length(lis)){
          
          
          
          temptask<-c(selproject[1,1],selproject[1,2],(eval(parse(text = paste0("input$projectname_taskmodify" ,lis[i])))), (eval(parse(text =paste0("input$details_taskmodify" ,lis[i])))),  
                      (eval(parse(text =paste0("input$category_taskmodify" ,lis[i])))), (eval(parse(text =paste0("input$priority_taskmodify" ,lis[i])))), 
                      (eval(parse(text =paste0("input$fte_taskmodify" ,lis[i])))), (eval(parse(text =paste0("input$status_taskmodify" ,lis[i])))))
          
          check<-c(check,sum((eval(parse(text = paste0("input$projectname_taskmodify" ,lis[i]))))!=""),
                   sum((eval(parse(text =paste0("input$details_taskmodify" ,lis[i]))))!=""),
                   sum((eval(parse(text =paste0("input$category_taskmodify" ,lis[i]))))!=""),
                   sum((eval(parse(text =paste0("input$priority_taskmodify" ,lis[i]))))!=""),
                   
                   sum(!is.na((eval(parse(text =paste0("input$fte_taskmodify" ,lis[i])))))),
                   sum((eval(parse(text =paste0("input$status_taskmodify" ,lis[i]))))!=""))
          pknewtask<-paste0(pknew,"T",i)
          
          
          e[i,]<-c(temptask,pknewtask)
          
          
        }}
      
      if((! FALSE %in% !grepl("_",e[,4],fixed=TRUE) )){
        
      if(!0%in%check || is.null(check)){
        h<-tasksMain
        
        h[,ncol(h)+1]<-substr(h$Primary.Key,1,nchar(h$Primary.Key)-2)
        h <- filter(h, !(V10 %in% oldpk))
        h<-h[,-10]
        
        
        
        tasksMain<-h
        
        tasksMain<-rbind(tasksMain,setnames(e,names(tasksMain)))
        
        
        
        
        # 
        
        
        projectname_taskaddlog2<-c()
        details_taskaddlog2<-c()
        category_taskaddlog2<-c()
        priority_taskaddlog2<-c()
        fte_taskaddlog2<-c()
        status_taskaddlog2<-c()
        
        
        
        
        
        # 
        
        
        
        if (length(projectname_taskadds) != 0)
        {
          for(i in 1:length(projectname_taskadds))
          {
            
            projectname_taskaddlog2<-c(projectname_taskaddlog2,(eval(parse(text = paste0("input$",projectname_taskadds[i],sep="")))))
            details_taskaddlog2<-c(details_taskaddlog2,(eval(parse(text = paste0("input$",details_taskadds[i],sep="")))))
            category_taskaddlog2<-c(category_taskaddlog2,(eval(parse(text = paste0("input$",category_taskadds[i],sep="")))))
            priority_taskaddlog2<-c(priority_taskaddlog2,(eval(parse(text = paste0("input$",priority_taskadds[i],sep="")))))
            fte_taskaddlog2<-c(fte_taskaddlog2,(eval(parse(text = paste0("input$",fte_taskadds[i],sep="")))))
            status_taskaddlog2<-c(status_taskaddlog2,(eval(parse(text = paste0("input$",status_taskadds[i],sep="")))))
            
          }}
        
        
        #  
        
        a<-c()
        if(length(details_taskaddlog2) != 0){
          for( i in 1:length(details_taskaddlog2)){
            
            
            a<-c(a,sum((projectname_taskaddlog2[i])!=""),sum((details_taskaddlog2[i])!=""),sum((category_taskaddlog2[i])!=""),sum((priority_taskaddlog2[i])!=""),
                 sum(!is.na(fte_taskaddlog2[i])),sum((status_taskaddlog2[i])!=""))
          }}
        
        if( (! FALSE %in% !grepl("_",details_taskaddlog2,  fixed=TRUE) )){
          
        
        
        if(! 0%in%a || is.null(a))
        {
          
          n<-nrow(RV2$data)
          #update tasks
          if(length(details_taskaddlog2)!=0){
            for( i in 1:length(details_taskaddlog2)){
              
              b<-c(selproject[1,1],selproject[1,2],projectname_taskaddlog2[i],details_taskaddlog2[i],category_taskaddlog2[i],priority_taskaddlog2[i],fte_taskaddlog2[i],status_taskaddlog2[i])
              
              pk2mod<-paste(pknew,"T",n+i, sep = "")
              b<-c(b,pk2mod)
              
              tasksMain[nrow(tasksMain)+1,] <- b
            }}
           
          taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
          data<-taskresource %>% separate(Primary.Key, 
                                          c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                          extra='drop',fill = "right",sep ='_') #%>% data
          
          
          da<-data %>% separate(Task, c("Ser","Project Name"),
                                                 extra='drop',sep ='T') #%>% data
          indexx<-c()
          
          for(k in 1:nrow(data))
            {
            pkneww<-""
            for(j in 1:12 ){
              
              pkneww<-paste0(pkneww,data[k,j],"_")
              
              
            }
            pkneww<-paste0(pkneww,da$Ser[k])
            if(pkneww==oldpk){
              indexx<-c(indexx,k)
              
            }
            
            
          }
         
          for(i in indexx){
            
            
            newpk<-paste0(pknew,"T",da$`Project Name`[i],"_",data$`Employee Name`[i],"_",data$Category[i],"_",data$Month[i])
            taskresource[i,1]<-newpk
            
           
          }
          
          
          
          
          write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
          write.table(x = projectsMain,file = "./data/Projects.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
          write.table(x = tasksMain,file = "./data/Tasks.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
          session$reload()
          
          refreshvectors2()
        }   
        else{
          showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
        }
        }
        
        else{
          
          showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error")
        }
        
        #
      }
      else{
        showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
      }
        
      }
      
      else{
        
        showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error")
      } 
        
        
        }
    else{
      showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
    }
    }
    else{

      showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error")
    }
    
    
  })
  
  
  
  
  observeEvent(input$del_modify,{
    
   #
    rown<-as.numeric(rownames(selproject))
    pm<-projectsMain
    num<-pm[rown,13]
    newp<-c(selproject[1,1],selproject[1,2],input$projectname_modifying2,input$projectclassification_modifying2,input$targetaudience_modifying2,paste(input$geography_modifying2,collapse = ','),
            input$stakeholdergroup_modifying2,paste(input$stakeholdernames_modifying2,collapse = ','),paste(input$TA_modifying2, collapse = ','),paste(input$Brand_modifying2, collapse = ','),input$selectedMonth_modifying2,paste(input$status_modifying2, collapse = ',')
    ) 
    #
    pknew<-as.character("")
    
    for( i in 1:length(newp)){
      
      if(newp[i]=="")
      {
        newp[i]<-NA
      }
      pknew<-paste0(pknew,newp[i],sep = "_")
      
    }
    
    pknew<-paste0(pknew,num,sep = "")
    
    
    
    
    projectsMain<-projectsMain[c(-rown),]
    
    
    h<-tasksMain
    
    h[,ncol(h)+1]<-substr(h$Primary.Key,1,nchar(h$Primary.Key)-2)
   
    
    
    
    
    taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    t<-tasksMain
    
    datatask<-t %>% separate(Primary.Key, 
                             c("Team", "Sub Team","Project Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber"),
                             extra='drop',fill = "right",sep ='_')
    
    
    
    
    
    
    
    
    
    
    
   indexx<-which((h$V10 ==pknew))
    taskid<-datatask$Tasknumber[indexx]
    
    h <- filter(h, !(V10 %in% pknew))
    h<-h[,-10]
    
    #
    tasksMain<-h
    index2<-c()
    for(i in 1:length(taskid))
    {
      ind<-which(data$Task==taskid[i])
      index2<-c(index2,ind)
      
      
    }
    
    
    
    
    
    
    #
   
    taskresource<-taskresource[-index2,]
    
    write.table(x = tasksMain,file = "./data/Tasks.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    
    
    write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    write.table(x = projectsMain,file = "./data/Projects.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    
    
    
    
    
    
    
    refreshvectors2()
    
    
    session$reload()
    
    
  })
  
  
  observeEvent(list(input$temp1_rows_selected,input$projectname_modifying2),{
    #observeEvent(input$projectname_modifying2,{
    
    
    
    a<-input$projectname_modifying2
    if(!is.null(RV2$data)){
      if(nrow(RV2$data)!=0){
        
        
        return( lapply(1:nrow(RV2$data),function(i){
          
          
          updateSelectInput(session,paste0("projectname_taskmodify",i),label=NULL,choices= paste0(a),selected =  paste0(a))
          
          
        } )) }}
    
    # #
    # 
    # id <- input$AddRow_taskplan
    # 
    # for(i in 1:id){
    # 
    #   updateTextInput(session,paste0("projectname_taskadds",i),label=NULL,value = paste0(a))
    # }
    
    
    
  })
  
  observeEvent(input$projectname_taskmodify1,{
    
    if(!is.na(input$AddRow_taskplan)){
      id <- input$AddRow_taskplan
      a<-input$projectname_modifying2
      for(i in 1:id){
        
        updateSelectInput(session,paste0("projectname_taskadds",i),label=NULL,choices=paste0(a), selected = paste0(a))
      }
    }
    
    
    
    
    
    
  })
  
  
  
  observeEvent(input$AddRow_taskplan, 
               {
                 id <- input$AddRow_taskplan
                 v<-input$projectname_modifying2
                 
                 insertUI(
                   selector = "#myId_taskplan",
                   where="beforeBegin",
                   ui = fluidPage(
                     fluidRow(
                       tags$div(id = paste0("Div",id),
                                
                                
                                #column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
                                column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",checkboxInput(paste0("checkbox_taskadds",id), label=NULL, value=FALSE, width=50)))),
                                column(2,div(style="padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em",(selectInput(paste0("projectname_taskadds",id),label=NULL,choices=v,selected=v )))), 
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ",(textInput(paste0("details_taskadds",id), label=NULL)))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ",(selectInput(paste0("category_taskadds",id), label=NULL,choices=c("",unique(task_type)))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ",(selectizeInput(paste0("priority_taskadds",id), label=NULL,choices = c("",priority), options = list(create = TRUE))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ",(numericInput(paste0("fte_taskadds",id),label=NULL,value="")))),
                                column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ",(selectizeInput(paste0("status_taskadds",id),label=NULL, choices = c("",status),options = list(create = TRUE)))))
                                
                       )
                       
                     )
                   )
                 )
                 
                 
                 chckbox_taskadds               <<- c(chckbox_taskadds, paste0("checkbox_taskadds",id))
                 projectname_taskadds           <<- c(projectname_taskadds,paste0("projectname_taskadds",id))
                 details_taskadds <<- c(details_taskadds,paste0("details_taskadds",id))
                 category_taskadds        <<- c(category_taskadds,paste0("category_taskadds",id))
                 priority_taskadds             <<- c(priority_taskadds,paste0("priority_taskadds",id))
                 fte_taskadds      <<- c(fte_taskadds,paste0("fte_taskadds",id))
                 status_taskadds       <<- c(status_taskadds,paste0("status_taskadds",id))
                 divList_tasks     <<- c(divList_tasks,paste0(id))
                 
                 
                 
                
                 
                 
               })
  
  
  
  observeEvent(input$del_taskplan, 
               {
                 
                 
                 
                 
                 v<-input$checkbox_taskmodify1
                 indexx<-c()
                 n1<-nrow(RV2$data)
                 
                
                 
                 
                 
                 if(!is.null(n1)){
                 if(!is.na(n1) && n1!=0){
                   for(j in 1:n1)
                   {
                     if (eval(parse(text = paste0("input$checkbox_taskmodify",j,sep="")))==TRUE)
                     {
                       
                       indexx<-c(indexx,j)
                       
                       
                       
                       removeUI(
                         selector = paste0("div:has(> #checkbox_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #projectname_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #details_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       removeUI(
                         selector = paste0("div:has(> #category_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #priority_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #fte_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       removeUI(
                         selector = paste0("div:has(> #status_taskmodify",j,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       
                       
                       # projectname_taskadds<-projectname_taskadds[!projectname_taskadds %in% paste0("projectname_taskmodify",j)]
                       # details_taskadds<-details_taskadds[!details_taskadds %in% paste0("details_taskmodify",j)]
                       # category_taskadds<-category_taskadds[!category_taskadds %in% paste0("category_taskmodify",j)]
                       # priority_taskadds<-priority_taskadds[!priority_taskadds %in% paste0("priority_taskmodify",j)]
                       # fte_taskadds<-fte_taskadds[!fte_taskadds %in% paste0("fte_taskmodify",j)]
                       # status_taskadds<-status_taskadds[!status_taskadds %in% paste0("status_taskmodify",j)]
                       # divList_tasks<-divList_tasks[!divList_tasks %in% paste0(j)]
                       
                     }}}}
                 
                 
                 if(!is.null(indexx)){
                   RV2$data<-RV2$data[-indexx,]}
                 
                 
                 n2<-length(projectname_taskadds)
                 
                 if(n2!=0){
                   for(k in 1:n2)
                     
                   { 
                     if (eval(parse(text = paste0("input$checkbox_taskadds" , k,sep="")))==TRUE)
                     {
                       
                       
                       
                       
                       removeUI(
                         selector = paste0("div:has(> #checkbox_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #projectname_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #details_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       removeUI(
                         selector = paste0("div:has(> #category_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #priority_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       removeUI(
                         selector = paste0("div:has(> #fte_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       removeUI(
                         selector = paste0("div:has(> #status_taskadds",k,")"),
                         multiple = TRUE,
                         immediate = TRUE,
                         session
                       )
                       
                       
                       
                       projectname_taskadds<<-projectname_taskadds[!projectname_taskadds %in% paste0("projectname_taskadds",k)]
                       details_taskadds<<-details_taskadds[!details_taskadds %in% paste0("details_taskadds",k)]
                       category_taskadds<<-category_taskadds[!category_taskadds %in% paste0("category_taskadds",k)]
                       priority_taskadds<<-priority_taskadds[!priority_taskadds %in% paste0("priority_taskadds",k)]
                       fte_taskadds<<-fte_taskadds[!fte_taskadds %in% paste0("fte_taskadds",k)]
                       status_taskadds<<-status_taskadds[!status_taskadds %in% paste0("status_taskadds",k)]
                       divList_tasks<<-divList_tasks[!divList_tasks %in% paste0(k)]
                       
                     }}}
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               })
  
  
  
  #################   MODIFY SCOPE DATATABLE
  
  
  output$temp1<-DT :: renderDataTable({
    
    #data1<<-retrievedtable
    #summarize by project name and hours
    
   
    RV$data<-RV$data[RV$data$Project_Name!="Adhoc",]
    #out<-out %>% distinct(Project_Name, .keep_all = TRUE)
    
    datatable(RV$data[,c(-12)], rownames= FALSE ,selection = 'single',options = list(autoWidth=TRUE, pageLength = 5)) 
    
    
  })
  
  
  
  #PROJECT SCOPE TABLE: MODIFY
  
  output$temp3UI<-renderUI({  
    
    
    
    
    fluidRow(
      
      
      column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
      column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-9em",(textInput(paste0("projectname_modifying2"),label="Project Name")))),
      column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-13em ",(selectInput(paste0("projectclassification_modifying2"), label="Classification",choices=c("",unique(Projectclassification)),selected=NULL)))),   
      column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-11em ",(selectInput(paste0("targetaudience_modifying2"), label="Target Audience",selected=NULL,choices=c("",unique(Targetaudience)))))),
      column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ",(selectInput(paste0("geography_modifying2"), label="Geography",selected=NULL,choices = c("",geography_list),multiple = TRUE)))),
      column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ",(selectInput(paste0("stakeholdergroup_modifying2"),label="Functional Group",selected=NULL, choices=c("",unique(Stakeholder)))))),
      column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-5em ",(selectInput(paste0("stakeholdernames_modifying2"),label="Stakeholders", selected=NULL,choices = c("",stakeholdernameList))))),
      column(1,div(style="padding: 0px 0px; width: 100px; margin-top:-1em; margin-left:-9em ",(selectInput(paste0("TA_modifying2"),label="TA", selected=NULL,choices=c("",TA))))),
      column(1,div(style="padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-11em ",(selectInput(paste0("Brand_modifying2"),label="Brand", selected=NULL,choices =c("",brand),multiple = FALSE)))),
      column(1,div(style="padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-11em ",selectInput(paste0("selectedMonth_modifying2"),"Project End Month", choices=c("",month_list_scope), selected = "Project End Month"[6]))),   #CHECK
      column(1,div(style="padding: 0px 0px; width: 100px; margin-top:-1em; margin-left:1em ",(selectizeInput(paste0("status_modifying2"),label="Status", choices = c("",status), options = list(create = TRUE)))))
      
      
    )
    
    
    
  })
  
  
  
  
  
  #TASK TABLE: MODIFY
  
  
  observeEvent(input$temp1_rows_selected,{
    observeEvent(input$projectname_modifying2,{
      
      output$taskUI<-renderUI({
        
        
        log<-c()
        pkcheck<-as.character("")
        c<-as.character("")
        for(i in 1:(length(selproject)-1)){
          log<-c(log,selproject[1,i])
        }
        if(selproject[3]!="Adhoc")
        {
          log<-c(log,selproject[1,length(selproject)])
        }
        # 
        for(i in 1:length(log)){
          pkcheck<-paste0(pkcheck,log[i],sep = "_")
        }
        
        
        pm<-projectsMain
       
        if(log[3]!="Adhoc")
        {
          rown<-as.numeric(rownames(selproject))
          number<-pm[rown,13]
          pkcheck<-paste0(pkcheck,number,sep = "")
        }
        
        
        
        data<-tasksMain
        data$Primary.Key<-substr(data$Primary.Key,1,nchar(data$Primary.Key)-2)
        if(is.na(pkcheck)){}
        else{
          
          data<-data %>% filter(Primary.Key == pkcheck)
          RV2$data<-data
          if(nrow(data)!=0){
            # 
            #return(for(i in 1:nrow(data)){
            return( lapply(1:nrow(data),function(i){
              #tags$div(id = paste0("c_PlanningDiv",i),
              fluidRow(
                
                #column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
                column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",checkboxInput(paste0("checkbox_taskmodify",i), label=NULL, value=FALSE, width=50)))),
                
                column(2,div(style="padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em",(selectInput(paste0("projectname_taskmodify",i),label=NULL,choices=data[i,3], selected = data[i,3])))),
                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ",(textInput(paste0("details_taskmodify",i), label=NULL,value= data[i,4])))),
                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ",(selectInput(paste0("category_taskmodify",i), label=NULL,selected = data[i,5],choices=c("",unique(task_type)))))),
                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ",(selectizeInput(paste0("priority_taskmodify",i),label=NULL,selected = data[i,6],choices = c("",priority))))),
                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ",(numericInput(paste0("fte_taskmodify",i),label=NULL,value=data[i,7])))),
                column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ",(selectizeInput(paste0("status_taskmodify",i),label=NULL,selected = data[i,8], choices = c("",status)))))
                
                
              )} )) }}
        
        
        
        
      })})})
  
  
  
  
  
  
  ###SAVE SCOPE
  
  
  
  observeEvent(input$Save_scoping,{
    
    refresh_check$scoping<-6+input$Save_scoping
    
  
    mainteamcheck<-input$mainTeam
    subteamcheck<-input$Subteam
    
    
    checkdetails_taskaddlog<-c()
    checkdetails_taskaddlog<-c(checkdetails_taskaddlog,input$details_taskadd)
    
    
    
    if (length(divList_s) != 0)
    {
      for(i in 1:length(divList_s))
      {
        
       
        checkdetails_taskaddlog<-c(checkdetails_taskaddlog,(eval(parse(text = paste0("input$details_taskadd",i,sep="")))))
        
        
      }}
    
    
     #
    
    
    
    if((!grepl("_",input$projectname_scope,  fixed=TRUE) & (! FALSE %in% !grepl("_",checkdetails_taskaddlog,  fixed=TRUE)) & !grepl("_",input$stakeholdernames_scope,  fixed=TRUE)) ){
     
    
    projectNameInputData<-input$projectname_scope
    projectClassificationData<-input$projectclassification_scope 
    targetAudienceData<-input$targetaudience_scope 
    geographyData<-paste(input$geography_scope, collapse = ',')
    stakeholderGroupData<-paste(input$stakeholdergroup_scope , collapse = ',')
    stakeholderNameData<-paste(input$stakeholdernames_scope , collapse = ',')
    TAData<-paste(input$TA_scope , collapse = ',')
    brandData<-paste(input$Brand_scope , collapse = ',')
    monthData<-input$selectedMonth_scope 
    
    # 
    tasksMain <-read.csv("./data/Tasks.csv",header = TRUE, stringsAsFactors = FALSE)
    projectsMain <-read.csv("./data/Projects.csv",header = TRUE, stringsAsFactors = FALSE)
    #CHECK IF USER HAS ENTERED VALUES FOR ALL FIELDS
    
    
    
   
    
    projectname_taskaddlog<-c()
    details_taskaddlog<-c()
    category_taskaddlog<-c()
    priority_taskaddlog<-c()
    fte_taskaddlog<-c()
    status_taskaddlog<-c()
    
    projectname_taskaddlog<-c(projectname_taskaddlog,input$projectname_taskadd)
    details_taskaddlog<-c(details_taskaddlog,input$details_taskadd)
    category_taskaddlog<-c(category_taskaddlog,input$category_taskadd)
    priority_taskaddlog<-c(priority_taskaddlog,input$priority_taskadd)
    fte_taskaddlog<-c(fte_taskaddlog,input$fte_taskadd)
    status_taskaddlog<-c(status_taskaddlog,input$status_taskadd)
    # 
    
    #id<-as.integer(input$AddRow_scoping)
    
    if (length(divList_s) != 0)
    {
      for(i in 1:length(divList_s))
      {
        
        projectname_taskaddlog<-c(projectname_taskaddlog,(eval(parse(text = paste0("input$projectname_taskadd",i,sep="")))))
        details_taskaddlog<-c(details_taskaddlog,(eval(parse(text = paste0("input$details_taskadd",i,sep="")))))
        category_taskaddlog<-c(category_taskaddlog,(eval(parse(text = paste0("input$category_taskadd",i,sep="")))))
        priority_taskaddlog<-c(priority_taskaddlog,(eval(parse(text = paste0("input$priority_taskadd",i,sep="")))))
        fte_taskaddlog<-c(fte_taskaddlog,(eval(parse(text = paste0("input$fte_taskadd",i,sep="")))))
        status_taskaddlog<-c(status_taskaddlog,(eval(parse(text = paste0("input$status_taskadd",i,sep="")))))
        
      }}
    
  
    len_list<-c(sum(projectNameInputData!=""), sum(projectClassificationData!=""), sum(targetAudienceData!=""),  sum(geographyData!=""),  sum(stakeholderGroupData!=""),
                sum(stakeholderNameData!=""),  sum(TAData!=""), sum(brandData!=""),sum(monthData!=""),sum(mainteamcheck!=""),sum(subteamcheck!=""))
    
    a<-c()
    
    for( i in 1:length(details_taskaddlog)){
      
      
      a<-c(a,sum((projectname_taskaddlog[i])!=""),sum((details_taskaddlog[i])!=""),sum((category_taskaddlog[i])!=""),sum((priority_taskaddlog[i])!=""),
           sum(!is.na(fte_taskaddlog[i])),sum((status_taskaddlog[i])!=""))
    }
    
    
    len_list<-c(len_list,a)
    
    # 
    
    
    #f(var(len_list)==0)
    if(! 0%in%len_list)  
    {
      
      
      
      showNotification("All fields correctly entered",duration = 10, closeButton = TRUE,type ="message")
      
      #update projects
      xyz<-c(mainteamcheck,subteamcheck, projectNameInputData,projectClassificationData, targetAudienceData, geographyData, stakeholderGroupData, stakeholderNameData, TAData, brandData , monthData)
      
      # 
      pkc<-as.character("")
      for(j in 1:length(xyz)){
        
        
        
        pkc<- paste0(pkc,xyz[j],sep = "_")
        
        
      }
      
      # 
      
      
      h<-as.character("In Progress")
      
      # 
      
      pkc<- paste0(pkc,h,sep = "_")
      
      
      #
      p<-as.numeric(projectsMain[nrow(projectsMain),13])+1
      
      
      
      pkc<- paste0(pkc,p,sep = "")
      
      xyz<-c(xyz,pkc)
      
      projectsMain[nrow(projectsMain)+1,] <-c(xyz,as.numeric(projectsMain[nrow(projectsMain),13])+1,"In Progress")
      # projectsMain[nrow(projectsMain)+1,] <-c(xyz,10,"In Progress")
      
      # 
      
      write.table(x = projectsMain,file = "./data/Projects.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      
      
      #update tasks
      
      
      
      for( i in 1:length(details_taskaddlog)){
        
        b<-c(mainteamcheck,subteamcheck,projectname_taskaddlog[i],details_taskaddlog[i],category_taskaddlog[i],priority_taskaddlog[i],fte_taskaddlog[i],status_taskaddlog[i])
        
        pk<-paste(pkc,"T",i, sep = "")
        b<-c(b,pk)
        
        tasksMain[nrow(tasksMain)+1,] <- b
      }
      
      #   
      
      write.table(x = tasksMain,file = "./data/Tasks.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      
      
      updateTextInput(session,paste0("projectname_scope"),label="Project Name",value = "")
      updateSelectInput(session,paste0("projectclassification_scope"),label="Classification",choices=c("",unique(Projectclassification)),selected=NULL)
      updateSelectInput(session,paste0("targetaudience_scope"), label="Target Audience",selected=NULL,choices=c("",unique(Targetaudience)))
      updateSelectInput(session,paste0("geography_scope"), label="Geography",selected=NULL,choices = c("",geography_list))
      updateSelectInput(session,paste0("stakeholdergroup_scope"),label="Functional Group",selected=NULL, choices=c("",unique(Stakeholder)))
      updateTextInput(session,paste0("stakeholdernames_scope"),label="Stakeholders",value = "")
      updateSelectInput(session,paste0("TA_scope"),label="TA", selected=NULL,choices=c("",TA))
      updateSelectInput(session,paste0("Brand_scope"),label="Brand", selected=NULL,choices =c("",brand))
      updateSelectInput(session,paste0("selectedMonth_scope"),"Project End Month", choices=c("",month_list_scope), selected = "Project End Month"[6])  
      
      
      updateTextInput(session,paste0("projectname_taskadd"),label="Project Name",value = "")
      updateTextInput(session,paste0("details_taskadd"),label="Task Details",value = "")
      updateSelectInput(session,paste0("category_taskadd"), label="Task Category",selected=NULL,choices=c("",unique(Targetaudience)))
      updateSelectInput(session,paste0("priority_taskadd"), label="Priority",selected=NULL,choices = c("",priority))
      updateNumericInput(session,paste0("fte_taskadd"),label="Total FTE days",value="")
      updateSelectInput(session,paste0("status_taskadd"),label="Status", selected=NULL,choices = c("",status))
      
      
      #id<-as.integer(input$AddRow_scoping)
      
      
      if (length(divList_s) != 0)
      {
        for(i in length(divList_s):1)
        { 
          
          removeUI(
            selector = paste0("div:has(> #projectname_taskadd",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          removeUI(
            selector = paste0("div:has(> #details_taskadd",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          removeUI(
            selector = paste0("div:has(> #category_taskadd",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          removeUI(
            selector = paste0("div:has(> #priority_taskadd",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          removeUI(
            selector = paste0("div:has(> #fte_taskadd",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          removeUI(
            selector = paste0("div:has(> #status_taskadd",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          
          
          
        }}
      
      session$reload()
      
      
      #updatebs4TabSetPanel(session = session,"abc",   selected = "pot")
      
      refreshvectors()
    }
    else{
      showNotification("Please enter values in all fields." ,duration = 10, closeButton = TRUE,type ="error")
      
    }
    }
    
    
    else{
      
      showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error")
   
       }
    
    
    
  })
  
  
  
  ####CLEAR SCOPE
  
  
  observeEvent(input$clear_scoping,{
    
    
    updateTextInput(session,paste0("projectname_scope"),label="Project Name",value = "")
    updateSelectInput(session,paste0("projectclassification_scope"),label="Classification",choices=c("",unique(Projectclassification)),selected=NULL)
    updateSelectInput(session,paste0("targetaudience_scope"), label="Target Audience",selected=NULL,choices=c("",unique(Targetaudience)))
    updateSelectInput(session,paste0("geography_scope"), label="Geography",selected=NULL,choices = c("",geography_list))
    updateSelectInput(session,paste0("stakeholdergroup_scope"),label="Functional Group",selected=NULL, choices=c("",unique(Stakeholder)))
    updateTextInput(session,paste0("stakeholdernames_scope"),label="Stakeholders",value = "")
    updateSelectInput(session,paste0("TA_scope"),label="TA", selected=NULL,choices=c("",TA))
    updateSelectInput(session,paste0("Brand_scope"),label="Brand", selected=NULL,choices =c("",brand))
    updateSelectInput(session,paste0("selectedMonth_scope"),"Project End Month", choices=c("",month_list_scope), selected = "Project End Month"[6])  
    
    
    
    updateTextInput(session,paste0("projectname_taskadd"),label="Project Name",value = "")
    updateTextInput(session,paste0("details_taskadd"),label="Task Details",value = "")
    updateSelectInput(session,paste0("category_taskadd"), label="Task Category",selected=NULL,choices=c("",unique(Targetaudience)))
    updateSelectInput(session,paste0("priority_taskadd"), label="Priority",selected=NULL,choices = c("",priority))
    updateNumericInput(session,paste0("fte_taskadd"),label="Total FTE days",value="")
    updateSelectInput(session,paste0("status_taskadd"),label="Status", selected=NULL,choices = c("",status))
    
    
    #id<-as.integer(input$AddRow_scoping)
    
    
    for(i in 1:length(divList_s)){
      
      updateTextInput(session,paste0("projectname_taskadd",divList_s[i]),label=NULL,value = "")
      updateTextInput(session,paste0("details_taskadd",divList_s[i]),label=NULL,value = "")
      updateSelectInput(session,paste0("category_taskadd",divList_s[i]), label=NULL,selected=NULL,choices=c("",unique(Targetaudience)))
      updateSelectInput(session,paste0("priority_taskadd",divList_s[i]), label=NULL,selected=NULL,choices = c("",priority))
      updateNumericInput(session,paste0("fte_taskadd",divList_s[i]),label=NULL,value="")
      updateSelectInput(session,paste0("status_taskadd",divList_s[i]),label=NULL, selected=NULL,choices = c("",status))
      
      
    }
    
    # refreshvectors() 
    
    
    
  })
  
  
  observeEvent(input$clear_modify,{
    
    updateSelectInput(session,"mainTeam_modify", choices = c("",(as.vector(unique(team_names$Team)))), selected = NULL)
    updateSelectInput(session,"Subteam_modify", choices = c(""), selected = NULL)
    updateSelectInput(session,"projectname_modify",selected = NULL ,choices=c("",unique(projectsMain[,3])))
    updateSelectInput(session,"classification_modify", selected = NULL ,choices=c("",unique(projectsMain[,4])))
    updateSelectInput(session,"targetaudience_modify", selected = NULL ,choices=c("",unique(projectsMain[,5])))
    updateSelectInput(session,"geography_modify", selected = NULL ,choices=c("",unique(projectsMain[,6])))
    updateSelectInput(session,"stakeholdergroup_modify",  selected = NULL ,choices=c("",unique(projectsMain[,7])))
    updateSelectInput(session,"stakeholdernames_modify", selected = NULL ,choices=c("",unique(projectsMain[,8])))
    updateSelectInput(session,"TA_modify", selected = NULL , choices=c("",unique(projectsMain[,9])))
    updateSelectInput(session,"Brand_modify", selected = NULL , choices=c("",unique(projectsMain[,10])))
    updateSelectInput(session,"selectedMonth_modify", choices=c("",month_list_scope), selected = NULL )
    
    
    
    
    
    
    
  })
  
  
  #RESET VECTORS BASED ON TAB SELECTION
  observeEvent("pot",
               {
                 
                 
                 refreshvectors()
                 chckbox_s               <<- c()
                 projectname_adds           <<- c()
                 details_adds <<- c()
                 category_adds        <<- c()
                 priority_adds             <<- c()
                 fte_adds      <<- c()
                 status_adds       <<- c()
                 divList_s    <<- c()
                 
               })
  observeEvent("xyz",
               {
                 
                 
                 refreshvectors2()
                 
               })
  
  refreshvectors<-function(){
    
    chckbox_s               <<- c()
    projectname_adds           <<- c()
    details_adds <<- c()
    category_adds        <<- c()
    priority_adds             <<- c()
    fte_adds      <<- c()
    status_adds       <<- c()
    divList_s    <<- c()
    
    
    
  }
  
  
  
  
  refreshvectors2<-function(){
    
    chckbox_taskadds               <<- c()
    projectname_taskadds           <<- c()
    details_taskadds <<- c()
    category_taskadds        <<- c()
    priority_taskadds             <<- c()
    fte_taskadds      <<- c()
    status_taskadds       <<- c()
    divList_tasks    <<- c()
    
    
    
  }
  
  # Code on Add row button
  observeEvent(input$AddRow_scoping, 
               {
                 id <- input$AddRow_scoping
                 
                 
                 insertUI(
                   selector = "#myId_scoping",
                   where="beforeBegin",
                   ui = fluidPage(
                     fluidRow(
                       tags$div(id = paste0("Div",id),
                                
                                
                                column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
                                #column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",checkboxInput(paste0("c_chckbox_scoping",id), label=NULL, value=FALSE, width=50)))),
                                column(2,div(style="padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em",(selectInput(paste0("projectname_taskadd",id),label=NULL,choices = ifelse(!is.na(input$projectname_scope),paste0(input$projectname_scope),""),selected =  ifelse(!is.na(input$projectname_scope),paste0(input$projectname_scope),""))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ",(textInput(paste0("details_taskadd",id), label=NULL)))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ",(selectInput(paste0("category_taskadd",id), label=NULL,choices=c("",unique(task_type)))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ",(selectizeInput(paste0("priority_taskadd",id), label=NULL,choices = c("",priority), options = list(create = TRUE))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ",(numericInput(paste0("fte_taskadd",id),label=NULL,value="")))),
                                column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ",(selectizeInput(paste0("status_taskadd",id),label=NULL, choices = c("",status),options = list(create = TRUE)))))
                                
                       )
                       
                     )
                   )
                 )
                 
                 
                 #chckbox_s               <<- c(chckbox_s, paste0("chckbox_scoping",id))
                 projectname_adds           <<- c(projectname_adds,paste0("projectname_taskadd",id))
                 details_adds <<- c(details_adds,paste0("details_taskadd",id))
                 category_adds        <<- c(category_adds,paste0("category_taskadd",id))
                 priority_adds             <<- c(priority_adds,paste0("priority_taskadd",id))
                 fte_adds      <<- c(fte_adds,paste0("fte_taskadd",id))
                 status_adds       <<- c(status_adds,paste0("status_taskadd",id))
                 divList_s     <<- c(divList_s,paste0(id))
                 
                 # 
                 # RVprojectnameid$data<-c(RVprojectnameid$data,(eval(parse(text = paste0("input$projectname_taskadd" ,id)))))
                 # 
                 # RVprojectnameid$data2<-c(RVprojectnameid$data2,paste0("input$projectname_taskadd" ,id))
                 
                 
                 
                 
                 
               })
  
  
  observeEvent(input$delRow_scoping, 
               {
                 
                 
                 if (length(divList_s) != 0)
                 {x <- divList_s[length(divList_s)]
                 divList_s<<-divList_s[-length(divList_s)]
                 
                 removeUI(
                   selector = paste0("div:has(> #projectname_taskadd",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 removeUI(
                   selector = paste0("div:has(> #details_taskadd",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 removeUI(
                   selector = paste0("div:has(> #category_taskadd",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 removeUI(
                   selector = paste0("div:has(> #priority_taskadd",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 removeUI(
                   selector = paste0("div:has(> #fte_taskadd",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 removeUI(
                   selector = paste0("div:has(> #status_taskadd",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 
                 
                 
                 
                 projectname_adds<<-projectname_adds[-length(projectname_adds)]
                 details_adds<<-details_adds[-length(details_adds)]
                 category_adds<<-category_adds[-length(category_adds)]
                 priority_adds<<-priority_adds[-length(priority_adds)]
                 fte_adds<<-fte_adds[-length(fte_adds)]
                 status_adds<<-status_adds[-length(status_adds)]
                 
                 
                 
                 }
                 
                 
                 
                 
                 
                 
                 
                 
               })
  
  
  
  
  
  
  
  
  
  
  ##### AUTOMATIC UPDATE OF PROJECT NAME
  
  observeEvent(input$projectname_scope,{
    
    a<-input$projectname_scope
    
    updateSelectInput(session,paste0("projectname_taskadd"), label="Project Name",selected=input$projectname_scope,choices=c(input$projectname_scope))
    
    
  
    id<-as.integer(input$AddRow_scoping)


    for(i in 1:id){

      
      
      updateSelectInput(session,paste0("projectname_taskadd",i), label=NULL,selected=input$projectname_scope,choices=c(input$projectname_scope))
      



    }

    
    outputId<<-input$projectname_scope
    
    
   
    
  })
  
  
  ############Resource Planning
  
  observeEvent(list(input$rscmainTeam), {
    
    filtered <- team_information[team_information$Team == input$rscmainTeam,]
    updateSelectInput(session,"rscSubteam", choices = c(as.vector(unique(filtered[,4]))), selected = '')
    
    
  })
  
  
  observeEvent(input$Addtaskrsc , {
   
     
    if(!is.null(input$Addtaskrsc)){
    if(input$Addtaskrsc>0){
 #idx<<-idx+1
  insertUI(selector = '#filter2',
           where = "beforeBegin",
   ui=  box(id= "boxtask",solidHeader = TRUE, collapsible = TRUE, width=600, closable = FALSE,
    fluidRow(
     column(1, actionButton("rscaddnewtask",tags$b("Add selected tasks"), style="color: #0a0a0a; background-color:  #aee0e8; height:50px",width=200))),
      
      br(),
 
    fluidRow(
      column(6, (DT :: dataTableOutput("tempRsc", width = 1600 ))))
   
    ),
   immediate = TRUE,
   multiple = FALSE,
   session=session
    
    
  )
    }}
  })
  
  
  
  observeEvent(list(input$rscRetrievePlan), {
    
   
    mo<-input$rscWDmonth2
    
    if(input$rscRetrievePlan>0){
      
      output$rscfilter2<-renderUI({
        
        box(  solidHeader = TRUE, collapsible = TRUE, width=600, closable = FALSE,
            
            
            fluidRow(
             
             column(4,actionButton("Addtaskrsc","      Add Tasks to Month    ", style = "color: #0a0a0a; background-color: #aee0e8; height:40px", offset = 1),
             actionButton("Delltaskrsc","Delete Selected Tasks", style = "color: #0a0a0a; background-color: #aee0e8; height:40px", offset = 1)),
              
            column(4,h2(paste(monthoutput(mo)),align = "center"))),  
            
       
            
            
             fluidRow( 
              column(width = 12,
                     box(
                       width = NULL , status = "primary",(div(style = 'overflow-x: scroll',DT :: dataTableOutput("temp2Rsc", width = 1500 )))))  
           
            
            
            
            
            ),
            
            br(),
            
            fluidRow(
              
              column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
              column(2,div(style="padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em",(selectInput(paste0("projectname_taskadd_adhoc"),selected = "Adhoc",label="Project Name",choices = c("Adhoc"))))),
              column(1,div(style="padding: 0px 0px; margin-top:-1em; margin-left:-9em ",(textInput(paste0("details_taskadd_adhoc"), label="Task Details")))),
              column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ",(selectInput(paste0("category_taskadd_adhoc"), label="Task Category",choices=c("",unique(task_type)))))),
              column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ",(selectizeInput(paste0("priority_taskadd_adhoc"), label="Priority",choices = c("",priority))))),
              column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ",(numericInput(paste0("fte_taskadd_adhoc"),label="Total FTE days",value="")))),
              column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ",(selectizeInput(paste0("status_taskadd_adhoc"),label="Status", choices = c("",status))))),
              actionButton("Addadhoc",tags$b("Add Task"), style="color: #0a0a0a; background-color:  #aee0e8; height:50px",width=200)
              
              
            ), 
            
            fluidRow(tags$div(id = "myId_scoping_adhoc")),
            br(),
            
            fluidRow(column(1,actionButton("AddRow_scoping_adhoc","", icon=icon("plus-square","fa-3x")), offset = 0),
                     column(1,actionButton("delRow_scoping_adhoc","", icon=icon("minus-square","fa-3x")), offset = 0) )     
            
            
            
        )
  
  
  
  
  
  
  
  
  
  
      })
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  output$tempRsc<-DT :: renderDataTable({
    
    
    

    ##
    
    dff<-RVrsctemp1$data
#
    dff<-dff[dff$Project.Name!="Adhoc",]
    
    datatable(dff, filter = "top",rownames= FALSE ,selection = 'multiple',options = list(autoWidth=TRUE, pageLength = 5)) 
    
    
  })
  
  
  
  output$temp2Rsc<-DT :: renderDataTable({
    
    
   
    
    
   
    a<-RVrsctemp2$data
    
    if(!is.null(a)){
      a[,-c(1)] <- lapply(a[,-c(1)], function(x) as.numeric(as.character(x)))
      
      if(is.null(rowsum1)){
        a[,-c(1)] <- lapply(a[,-c(1)], function(x) as.numeric(as.character(x)))
        a[nrow(a)+1,-c(1)]<-colSums(a[,-c(1)],na.rm = FALSE)
        
      }
      else{
        a[nrow(a)+1,-c(1)]<-rowsum1
      }
      
     
    }
    
  
    
    Total<<-a[nrow(a),]
    
    
    Totalremaining<-Total
    for(i in 2:length(Totalremaining)){
      
      Totalremaining[i]<-RVholWD$data$`Working Days`-Total[i] }
    
    Totalremaining[1]<-"Leftover/Surplus"
    
    d<-rbind(RVrsctemp2$data,Total,Totalremaining)
    
    
    #
    ifelse(ncol(d)<16, n<-16-ncol(d),n<-0)
    
    
    
    
    
    
    
    m<-as.data.frame(matrix("", ncol = n, nrow = nrow(d)))
    
    
    colnames(m)<-rep(" ",n)
    
    
    d<-cbind(d,m)
  
    d<-as.data.frame(d,stringsAsFactors = FALSE)
    d[,1]<-as.character(d[,1])
    d[nrow(d)-1,1] <- "Total Planned Days"

   n<-nrow(d)
   
    if(nrow(d)==1){
      
      d<-NULL
      
    }
    #
   
   
   
    #
    datatable(d, rownames= FALSE ,
              selection = 'single',editable = TRUE, options = list( columnDefs = list(list(className = 'dt-center',width = "50px", targets = "_all")),scrollY = '450px',autoWidth=TRUE, pageLength = 10),fillContainer = T,class = "display")%>% formatStyle(
                names(d),
                backgroundColor = styleInterval(c(-1, 100), c('red', 'white', 'white')),
                fontWeight = 'bold'
              )
    
    #####
    
  })
  
  observeEvent(input$temp2Rsc_cell_edit, {
    
    
    
    ###
    RVrsctemp2edited$data<-RVrsctemp2$data
    leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
    info<-input$temp2Rsc_cell_edit
    edit_row <-  as.numeric(info$row)
    edit_col <-  as.numeric(info$col ) +1 
    edit_value <-  as.numeric(info$value)
    
    
    
    if(edit_row==1){
      
      emp<-colnames(RVrsctemp2edited$data)
      a1<-input$rscmainTeam
      a2<-input$rscSubteam
      m<-input$rscWDmonth2
      
      pkemp<-paste(a1,a2,m,emp[edit_col],sep="_")
      for(i in 2:length(emp)){
        
        
        
        
        for(k in 1:nrow(leaves)){
          
          
          if(leaves[k,1]==pkemp)
          {
            
            leaves[k,2]<-edit_value
          }
        } 
        
        
      }
      ######## 
      write.table(x = leaves,file = "./data/leaves.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    }
    
    
    
    
    
    ######## 
    
    RVrsctemp2edited$data[edit_row,edit_col]<-edit_value
    
    RVrsctemp2edited$data[,-c(1)] <- lapply(RVrsctemp2edited$data[,-c(1)], function(x) as.numeric(as.character(x)))
    rowsum1<<-colSums(RVrsctemp2edited$data[,-c(1)])
    ######## 
    
    Total<<-rowsum1
    RVrsctemp2$data<-RVrsctemp2edited$data
  })
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$rscUpdatePlan, {
    
    
    
   ###
    t<-RVrsctemp2edited$data
    if(nrow(t)!=0){
      col<-colnames(t)
      m<-input$rscWDmonth2
      
      g<-length(globalpk)+1
      
      tasksMain <-read.csv("./data/Tasks.csv",header = TRUE, stringsAsFactors = FALSE)
      
      projectsMain <-read.csv("./data/Projects.csv",header = TRUE, stringsAsFactors = FALSE)
      taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
      #
      adpk<-globaladhoc
      
     
      
      ###
        
      if(!is.null(globalpk)){
        for( i in 2:g){
          #  # # 
          for(j in 1:(ncol(t)-1)){
            check<-0
            tempk<-paste(globalpk[i-1],col[j+1],m,sep="_")
            
            
            for(k in 1:nrow(taskresource)){
              
              if(taskresource[k,1] == tempk){
                taskresource[k,2] <- t[i,j+1]
                check<-1
              }
              
              
            }
            
            if(check==0){
              taskresource[nrow(taskresource)+1,]<-c(tempk,t[i,j+1])}
            
            
            
          }
        }}
      
      ###
      a1<-input$rscmainTeam
      a2<-input$rscSubteam
      pkadho<-paste0(a1,"_",a2,sep="_")
      
      for (i in 1:10){
        pkadho<-paste0(pkadho,"Adhoc",sep="_")
      }
      #  
      pk1<-c()
      
      
      ####
      
      data1<-taskresource %>% separate(Primary.Key, 
                                       c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Month"),
                                       extra='drop',fill = "right",sep ='_') #%>% data
      
      
      
      data1<-data1[!is.na(data1$`Project Name`),]
      
      
      
      ###  
      
      
      
      
      
      
      
      
      
      h<- tasksMain[tasksMain$Team==a1 & tasksMain$Subteam==a2 & tasksMain$Project.Name=="Adhoc" & tasksMain$Primary.Key%in%adpk,]
      h<-h[!is.na(h$Project.Name),]
      n<-nrow(h)
      
    h$Primary.Key
      
      adho<-t[t$`Task Details`%in%h$Task.details,]
      
      # adho<-t[(g+1):nrow(t),]
      colname<-colnames(adho)
      
      
      data<-h %>% separate(Primary.Key, 
                           c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Month"),
                           extra='drop',fill = "right",sep ='_') #%>% data
      
      
      n1<-which(data$Month==m)
      
      for(i in 1:length(n1)){
        
        for(j in 2:ncol(adho)){
          
          ##
          pk1<-paste0(pkadho,data$Task[n1],"_",m,"_",colname[j],"_",m)
          
          
          
          
          
          
          
          
          
          for(k in 1:nrow(taskresource)){
            
            if(taskresource[k,1]==pk1){
              taskresource[k,2]<-adho[i,j]
              
            }
            
            
          }
          
          
          
        }
        
        
        
        
      }
      
      
      ##
      
      
      
      
      
      #   
      ###
      
      
      
      
      # 
      write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      
      session$reload()
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###retrieve tasks
  
  
  
  observeEvent(input$Addtaskrsc, {
    
    

    main<-projectsMain
    dat<-tasksMain
    tu<-RVpkk$data
    dat<-dat[!(dat$Primary.Key%in%tu[,ncol(tu)] ) & !(dat$Primary.Key%in%globalpk ) ,]
   dat<-dat[!is.na(dat$Project.Name),]
   

  dat<-dat[!grepl("Completed",dat$Primary.Key,  fixed=TRUE),]
   
  
    dat$EndMonth<-NA
    
    for(i in 1:nrow(dat)){
      
      main<-projectsMain
      
      pkrsc<-substr(dat[i,9],1,nchar(dat[i,9])-2)
      main<-main[main$Primary.Key == pkrsc,]
      dat[i,10]<-main$End_Month[1]
      
    }
    
    
    a1<-input$rscmainTeam
    a2<-input$rscSubteam
    a3<-input$rscMonth
    a4<-input$rscStatus
    
   
    if(is.null(input$rscMonth)){
      
      if(!is.null(input$rscStatus) ){
        
        
        dat<-dat[dat[,1]==a1,]
        dat<-dat[dat[,2]==a2,]
        dat<-dat[dat[,8]==a4,]
      }
      
      else{
        dat<-dat[dat[,1]==a1,]
        dat<-dat[dat[,2]==a2,]
        
        
      }
      
    }
    
    else{
      
      
      
      if(!s.null(input$rscStatus)){
        
        dat<-dat[dat[,1]==a1,]
        dat<-dat[dat[,2]==a2,]
        dat<-dat[dat[,8]==a4,]
        dat<-dat[dat[,10]==a3,]
        
        
      }
      
      else{
        
        dat<-dat[dat[,1]==a1,]
        dat<-dat[dat[,2]==a2,]
        
        dat<-dat[dat[,10]==a3,]
        
        
        
      }
      
      
      
    }
    
    thispk<<-dat[,9]
  
    dat<-dat[,-c(1,2,9)]
    dat<-dat[!is.na(dat[,1]),]
 ##
   
    RVrsctemp1$data <- dat
    
    
  })

  
  
  

  observeEvent(input$rscRetrievePlan, {

   
   
    
    
   
    
    m<-input$rscWDmonth2

    taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
    if(!is.null(m)){


      #e<-input$tempRsc_rows_selected

      #

      a1<-input$rscmainTeam
      a2<-input$rscSubteam

      t<-team_information
      t<-t[t[,3]==a1,]
      t<-t[t[,4]==a2,]
      #
      
      t<-t[t$Employee!="",]

      empname<- unique(t$Employee)

      data<-taskresource %>% separate(Primary.Key,
                                      c( "Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                      extra='drop',fill = "right",sep ='_' ) #%>% data

      
      tempdat<-data[data$Project_Name=="Adhoc", ]
      data<-data[!(data$Project_Name=="Adhoc"), ]
      tempdat<-tempdat[,c(1:13,15,16,14,17)]

      colnames(tempdat)<-colnames(data)
      data<-rbind(data,tempdat)
      #
      data<-data[!is.na(data$Project_Name),]
      ###CHECKK
      data<-data[data$Team==a1 & data$`Sub Team`==a2 & data$Month==m,]
      # data$task<-paste0(data$task)
      
      #
      t<-tasksMain
      t<-t[t$Team==a1 & t$Subteam==a2 & !is.na(t$Team),]
      
      # t1<-t[t$Project.Name=="Adhoc",]
      # t2<-t[!(t$Project.Name=="Adhoc"),]
      
     
      datatask<-t %>% separate(Primary.Key,
                               c("Team", "Sub Team","Project_Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber","extra"),
                               extra='drop',fill = "right",sep ='_')
      # 
      # datatask2<-t2 %>% separate(Primary.Key,
      #                            c("Team", "Sub Team","Project_Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber","extra"),
      #                            extra='drop',sep ='_')
      # 
      datatask<-datatask[!is.na(datatask$Project.Name),]
      #if(nrow(datatask)!=0){
      # for(i in 1:nrow(datatask)){
      #   
      #   
      #  
      #   if(datatask$Project.Name[i]=="Adhoc"){
      #     
      #     datatask$new[i]<-datatask$Tasknumber[i]
      #     
      #     
      #   }
      #   else{
      #     
      #     datatask$new[i]<-paste0(datatask$Tasknumber[i],datatask$extra[i])  
      #     
      #     
      #   }
      #   
      #   
      #   
      #   
      # }
      # 
      # }
      ##
      # datatask2$new<-paste0(datatask2$Tasknumber,datatask2$extra)
      # 
      # datatask1$new<-datatask1$Tasknumber
      
      #datatask<-rbind(datatask2,datatask1)
      
      ## 
      
      #
      
      
      datatask<-cbind(datatask,t$Primary.Key)
      x<-datatask[,19]
      datuse<-as.data.frame(cbind( datatask$Team, datatask$Subteam ,datatask$`Project_Name`,datatask$Task.category,datatask$Tasknumber , datatask$Task.details))
      datuse<-cbind(datuse,x)
      
      #
      datuse[] <- lapply(datuse, as.character)
      g<- left_join(data, datuse, by = c("Team" = "V1" ,"Sub Team" = "V2", "Project_Name" = "V3", "Task" = "x" ))

      data<-g
      data<-data[!is.na(data$Project_Name),]
     tu1<-data
     tu1<-tu1[-c(1:(nrow(tu1))),]
      tu<-unique(data[,c(19,20)])
      b<-unique(tu[,1])
      
      ###########CHECK
      #
      #
      
      
      if(length(b)>=1 & !is.na(b[1])){
      for(i in 1:nrow(tu)){
        
        
        for(j in 1:nrow(data)){
          
        
          if(data$V5[j]==tu[i,1] & data$V6[j]==tu[i,2]){
            
            tu1[nrow(tu1)+1,]<-data[j,]
            
            
            
          }
          
          
          
          
          
        }
        
        
        
        
      }
      
      
        
        ###
      tu<-tu1
      tu$pk<-""
      
      
      tu2<-tu %>% separate(Task, c("Ser","Project Name"),
                           extra='drop',sep ='T') #%>% data
      for(i in 1:nrow(tu)){
        pk1<-""
        for(j in 1:12){
          
          pk1<-paste0(pk1,tu[i,j],sep="_")
          
          
          
          
        }
        
       
        
        
        
        
        
        
        
        pk1<-paste0(pk1,tu2[i,13],"T",tu2[i,14])
        if(tu$Project_Name[i]=="Adhoc"){
          
          
          
            pk1<-paste0(pk1,"_",m)
          
        }
        
        tu$pk[i]<-pk1
        
        
        
        
      }
      }
     
      
      #
    
      tu<-as.data.frame(tu)
      
      tu<-tu[tu$`Employee Name`%in%empname,]
      
      
      
      
      
      if(nrow(tu)>0){
        
        
        tutask<-unique(tu[,c(19,20)])
        
        
        
        
      }
     
      else{
        
        
        tutask<-as.data.frame(tu)
      }
     
      
      globalpk<<-unique(tu$pk )
      
      
     
      ###
      
      rsctemp2<- as.data.frame(matrix(0, ncol = (length(empname)*2)+1, nrow = ifelse(is.null(nrow(tutask)),0,nrow(tutask))))
      
    if(!is.null(nrow(tutask))){
      rsctemp2[,1]<-tutask[,2]
    }

      empname1<-c()

      for(i in 1:length(empname)){

        empname1<-c(empname1,paste0(empname[i],"_Planned"),paste0(empname[i],"_Actuals"))

      }
    
      rsctemp2<-rsctemp2[!is.na(rsctemp2[,1]),]
      
     
      colnames(rsctemp2) <- c("Task Details",empname1)



      t<-t[t$Team==a1 & t$Subteam==a2,]

      ###

      for(k in 1:length(globalpk)){
        
        pk5<-globalpk[k]

       for( f in 2:ncol(rsctemp2)){
         
       
        pk6<-paste0(pk5,"_",empname1[f-1],"_",m)

        for(h in 1:nrow(taskresource)){

        if(taskresource[h,1]==pk6){

          rsctemp2[k,f]<-taskresource[h,2]
        }

        }


      }


      }
     
      ###
      
      leavecopy<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
      l<-c()
      for(i in 1:length(empname1)){
        
        
        check<-0
        
        pkemp<-paste(a1,a2,m,empname1[i],sep="_")
        
        if(nrow(leavecopy)!=0){
          
          for(k in 1:nrow(leavecopy)){
            
            
            if(leavecopy[k,1]==pkemp)
            {
              
              l<-c(l,leavecopy[k,2])
              check<-1
            }
          } 
          
          if(check==0){
            
            leavecopy[nrow(leavecopy)+1,]<-c(pkemp,0)
            l<-c(l,0)
          }
          
        }
        else{
          
          leavecopy[nrow(leavecopy)+1,]<-c(pkemp,0)
          l<-c(l,0)
        }
      }
      
      
###
    
      l<-c("Leaves",l)
      k<-as.data.frame(matrix(0, ncol = 7, nrow = 0))
      k<-rbind(k,l)
      
      k[,-c(1)] <- lapply(k[,-c(1)], function(x) as.numeric(as.character(x)))
      
      colnames(k)<-colnames(rsctemp2)
      #
      leaves<<-leavecopy
      write.table(x = leaves,file = "./data/leaves.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      
      ddd<-rbind(k,rsctemp2)
      ddd<-ddd[(!is.na(ddd[,1])),]
      
      RVrsctemp2$data <-ddd
      RVrsctemp2edited$data<-ddd
     RVpkk$data<-tu





    }

    else{
      showNotification("Please select month",duration = 10, closeButton = TRUE,type ="error")
      
    }
    

  })


  
  
  
  
  
  
  observeEvent(input$rscaddnewtask,  {  
    
    
    
    
    
  
    
    m<-input$rscWDmonth2
    dat<-RVrsctemp2$data
    e<-input$tempRsc_rows_selected
    tu<-RVpkk$data
    dff<-RVrsctemp1$data
    dff<-dff[!is.na(dff$Project.Name),]
    dff<-dff[dff$Project.Name!="Adhoc",]
    
    
    
    
 
    ##
    RVrsctemp1$data<-dff[-e,]
    
    dff<-dff[e,]
    dd <- as.data.frame(matrix(0, ncol = ncol(dat), nrow = nrow(dff)))
    dd[,1]<-dff$Task.details
    
    colnames(dd)<-colnames(dat)
    dat<-rbind(dat,dd)
    RVrsctemp2$data<-dat
    RVrsctemp2edited$data<-dat
    
    ##
    
    ##
    dat<-tasksMain
    dat<-dat[!is.na(dat[,1]),]
    dat<-dat[!(dat$Primary.Key%in%tu[,ncol(tu)]),]
    a1<-input$rscmainTeam
    a2<-input$rscSubteam
    dat<-dat[dat[,1]==a1,]
    dat<-dat[dat[,2]==a2,]
    dat<-dat[!is.na(dat$Project.Name),]
    
    dat<-dat[dat$Project.Name!="Adhoc",]
    
    dat<-dat[e,]
    tu<-tu[e,]
    
    
    col<-colnames(dd)
    tu<-c()
    
    ##
    
    for(i in 1:nrow(dat)){
      
      tu<-c(tu,dat$Primary.Key[i])
      for(j in 2:ncol(dd)){
        
        pk<-paste0(dat$Primary.Key[i],"_",col[j],"_",m)
        taskresource[nrow(taskresource)+1,]<-c(pk,0)
        
      }
    }
    
    write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    
    globalpk<<-c(globalpk,tu)
    ##
    g<-RVpkk$data
    a<-as.data.frame(matrix(0, ncol = 21, nrow = nrow(dat)))
    a[,21]<-dat$Primary.Key
   
    if(nrow(g)!=0){
   
   colnames(a)<-colnames(RVpkk$data)
    RVpkk$data<-rbind(RVpkk$data,a)
    
    }
    
    else{
      RVpkk$data<-a
      
    }

    removeUI(
      selector = paste0("div:has(> #rscaddnewtask",")"),
      multiple = FALSE,
      immediate = TRUE,
      session
    )
    removeUI(
      selector = paste0("div:has(> #tempRsc",")"),
      multiple = FALSE,
      immediate = TRUE,
      session
    )
    removeUI(
      selector = paste0("div:has(> #boxtask",")"),
      multiple = FALSE,
      immediate = TRUE,
      session
    )

    
    
  })
  
  
  
  
  
  
  
  ###selected tasts table update
 
  
  
  
  
  
  
  
  
  
  
  
  output$boxes1 <- renderUI({
  
    
    b<-input$rscWDmonth2
    if(input$rscWDmonth2 != ""){
     
      a<-as.Date(paste(as.character(b),"01"), format = "%B %Y %d")
      # a<-as.POSIXct(paste(as.character(b),"01"), format = "%B %Y %d")
      start_date <- as.Date(a, "%m/%d/%Y")
      num<-lubridate::days_in_month(start_date)
      end_date<-ceiling_date(start_date,"month")-1
      sat_sun_cnt<-Nweekends(start_date, end_date)
      holiday_cnt_MSD<-as.numeric(Nholidays(start_date, end_date, "MSD"))
      holiday_cnt_AR<-as.numeric(Nholidays(start_date, end_date, "AR"))
      
      total_WD<-as.numeric(num-sat_sun_cnt-max(holiday_cnt_MSD,holiday_cnt_AR))
      
      handq<-sat_sun_cnt+max(holiday_cnt_MSD,holiday_cnt_AR)
      # 
      # #
      a<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
      colnames(a)<-c("Holidays+Weekends","Working Days")
      
      a[1,1]<-handq
      a[1,2]<-total_WD
      RVholWD$data<-a
    }
    
    
    
    
    else
    {
      handq<-0
      total_WD <-0
      
      
    }
      
      
  box(id="box1",
          title = handq, width = 2, background = "light-blue",
          "Holidays+Weekends"
      )
      # 
      # box(id="box2",
      #     title = "Working Days", width = 2, background = "light-blue",
      #     ""
      # )
      
      
      
   })
  output$boxes2 <- renderUI({
    
    
    b<-input$rscWDmonth2
    if(input$rscWDmonth2 != ""){
      
      a<-as.Date(paste(as.character(b),"01"), format = "%B %Y %d")
      # a<-as.POSIXct(paste(as.character(b),"01"), format = "%B %Y %d")
      start_date <- as.Date(a, "%m/%d/%Y")
      num<-lubridate::days_in_month(start_date)
      end_date<-ceiling_date(start_date,"month")-1
      sat_sun_cnt<-Nweekends(start_date, end_date)
      holiday_cnt_MSD<-as.numeric(Nholidays(start_date, end_date, "MSD"))
      holiday_cnt_AR<-as.numeric(Nholidays(start_date, end_date, "AR"))
      
      total_WD<-as.numeric(num-sat_sun_cnt-max(holiday_cnt_MSD,holiday_cnt_AR))
      
      handq<-sat_sun_cnt+max(holiday_cnt_MSD,holiday_cnt_AR)
      # 
      # #
      a<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
      colnames(a)<-c("Holidays+Weekends","Working Days")
      
      a[1,1]<-handq
      a[1,2]<-total_WD
      RVholWD$data<-a
    }
    
    else
    {
      handq<-0
      total_WD <-0
      
      
    }
    
    


    box(id="box2",
        title=total_WD, width = 2, background = "light-blue",
        "Working Days"
    )
    
    
    
  })
  
  
  observeEvent(list(input$HandW,input$WD), {
    
    if(input$rscWDmonth2 != ""){
      b<-input$rscWDmonth2
      a<-as.Date(paste(as.character(b),"01"), format = "%B %Y %d")
      # a<-as.POSIXct(paste(as.character(b),"01"), format = "%B %Y %d")
      start_date <- as.Date(a, "%m/%d/%Y")
      num<-lubridate::days_in_month(start_date)
      end_date<-ceiling_date(start_date,"month")-1
      sat_sun_cnt<-Nweekends(start_date, end_date)
      holiday_cnt_MSD<-as.numeric(Nholidays(start_date, end_date, "MSD"))
      holiday_cnt_AR<-as.numeric(Nholidays(start_date, end_date, "AR"))
      
      total_WD<-as.numeric(num-sat_sun_cnt-max(holiday_cnt_MSD,holiday_cnt_AR))
      
      handq<-sat_sun_cnt+max(holiday_cnt_MSD,holiday_cnt_AR)
      # 
      # #
      updateTextInput(session,"HandW", value = handq)
      updateTextInput(session,"WD", value = total_WD)
    }
    
  })
  
  refreshvectors<-function(){
    
    chckbox_s_adhoc               <<- c()
    projectname_adds_adhoc           <<- c()
    details_adds_adhoc <<- c()
    category_adds_adhoc        <<- c()
    priority_adds_adhoc             <<- c()
    fte_adds_adhoc      <<- c()
    status_adds_adhoc       <<- c()
    divList_s_adhoc    <<- c()
    
    
    
  }  
  
  observeEvent(input$AddRow_scoping_adhoc, 
               {
                 id <- input$AddRow_scoping_adhoc
                 
                 
                 insertUI(
                   selector = "#myId_scoping_adhoc",
                   where="beforeBegin",
                   ui = fluidPage(
                     fluidRow(
                       tags$div(id = paste0("Div",id),
                                
                                
                                column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(""))))),
                                #column(1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",checkboxInput(paste0("c_chckbox_scoping",id), label=NULL, value=FALSE, width=50)))),
                                column(2,div(style="padding: 0px 0px; width: 250px; margin-top:-1em; margin-left:-8em",(textInput(paste0("projectname_taskadd_adhoc",id),label=NULL,value= ifelse(!is.na(input$projectname_scope),paste0(input$projectname_scope),""))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-9em ",(textInput(paste0("details_taskadd_adhoc",id), label=NULL)))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-7em ",(selectInput(paste0("category_taskadd_adhoc",id), label=NULL,choices=c("",unique(task_type)))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-5em ",(selectizeInput(paste0("priority_taskadd_adhoc",id), label=NULL,choices = c("",priority), options = list(create = TRUE))))),
                                column(1,div(style="padding: 0px 0px; width: 150px; margin-top:-1em; margin-left:-3em ",(numericInput(paste0("fte_taskadd_adhoc",id),label=NULL,value="")))),
                                column(2,div(style="padding: 0px 0px; width: 200px; margin-top:-1em; margin-left:-1em ",(selectizeInput(paste0("status_taskadd_adhoc",id),label=NULL, choices = c("",status),options = list(create = TRUE)))))
                                
                       )
                       
                     )
                   )
                 )
                 
                 
                 #chckbox_s               <<- c(chckbox_s, paste0("chckbox_scoping",id))
                 projectname_adds_adhoc           <<- c(projectname_adds_adhoc,paste0("projectname_taskadd_adhoc",id))
                 details_adds_adhoc <<- c(details_adds_adhoc,paste0("details_taskadd_adhoc",id))
                 category_adds_adhoc        <<- c(category_adds_adhoc,paste0("category_taskadd_adhoc",id))
                 priority_adds_adhoc             <<- c(priority_adds_adhoc,paste0("priority_taskadd_adhoc",id))
                 fte_adds_adhoc      <<- c(fte_adds_adhoc,paste0("fte_taskadd_adhoc",id))
                 status_adds_adhoc       <<- c(status_adds_adhoc,paste0("status_taskadd_adhoc",id))
                 divList_s_adhoc     <<- c(divList_s_adhoc,paste0(id))
                 
                 
               })
  
  
  observeEvent(input$delRow_scoping_adhoc, 
               {
                 
                 
                 if (length(divList_s_adhoc) != 0)
                 {x <- divList_s_adhoc[length(divList_s_adhoc)]
                 divList_s_adhoc<<-divList_s_adhoc[-length(divList_s_adhoc)]
                 
                 removeUI(
                   selector = paste0("div:has(> #projectname_taskadd_adhoc",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 removeUI(
                   selector = paste0("div:has(> #details_taskadd_adhoc",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 removeUI(
                   selector = paste0("div:has(> #category_taskadd_adhoc",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 removeUI(
                   selector = paste0("div:has(> #priority_taskadd_adhoc",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 removeUI(
                   selector = paste0("div:has(> #fte_taskadd_adhoc",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 removeUI(
                   selector = paste0("div:has(> #status_taskadd_adhoc",x,")"),
                   multiple = TRUE,
                   immediate = TRUE,
                   session
                 )
                 
                 
                 
                 
                 
                 projectname_adds_adhoc<<-projectname_adds_adhoc[-length(projectname_adds_adhoc)]
                 details_adds_adhoc<<-details_adds_adhoc[-length(details_adds_adhoc)]
                 category_adds_adhoc<<-category_adds_adhoc[-length(category_adds_adhoc)]
                 priority_adds_adhoc<<-priority_adds_adhoc[-length(priority_adds_adhoc)]
                 fte_adds_adhoc<<-fte_adds_adhoc[-length(fte_adds_adhoc)]
                 status_adds_adhoc<<-status_adds_adhoc[-length(status_adds_adhoc)]
                 
                 
                 
                 }
                 
                 
                 
                 
                 
                 
                 
                 
               })
  
  
  refreshvectorsadhoc<-function(){
    
    chckbox_s_adhoc               <<- c()
    projectname_adds_adhoc           <<- c()
    details_adds_adhoc <<- c()
    category_adds_adhoc        <<- c()
    priority_adds_adhoc             <<- c()
    fte_adds_adhoc      <<- c()
    status_adds_adhoc       <<- c()
    divList_s_adhoc    <<- c()
    
    
    
  }  
  
  
  
  
  
  
  observeEvent(input$Addadhoc,{
    
    
  
    refresh_check$scoping<-6+input$Save_scoping
    
    #
    mainteamcheck<-input$rscmainTeam
    subteamcheck<-input$rscSubteam
    
    
    
    tasksMain <-read.csv("./data/Tasks.csv",header = TRUE, stringsAsFactors = FALSE)
    projectsMain <-read.csv("./data/Projects.csv",header = TRUE, stringsAsFactors = FALSE)
    taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
    
    
    #
    
    #CHECK IF USER HAS ENTERED VALUES FOR ALL FIELDS
    
    
    projectname_taskaddlog_adhoc<-c()
    details_taskaddlog_adhoc<-c()
    category_taskaddlog_adhoc<-c()
    priority_taskaddlog_adhoc<-c()
    fte_taskaddlog_adhoc<-c()
    status_taskaddlog_adhoc<-c()
    
    projectname_taskaddlog_adhoc<-c(projectname_taskaddlog_adhoc,input$projectname_taskadd_adhoc)
    details_taskaddlog_adhoc<-c(details_taskaddlog_adhoc,input$details_taskadd_adhoc)
    category_taskaddlog_adhoc<-c(category_taskaddlog_adhoc,input$category_taskadd_adhoc)
    priority_taskaddlog_adhoc<-c(priority_taskaddlog_adhoc,input$priority_taskadd_adhoc)
    fte_taskaddlog_adhoc<-c(fte_taskaddlog_adhoc,input$fte_taskadd_adhoc)
    status_taskaddlog_adhoc<-c(status_taskaddlog_adhoc,input$status_taskadd_adhoc)
    
    # 
    #id<-as.integer(input$AddRow_scoping)
    
    if (length(divList_s_adhoc) != 0)
    {
      for(i in 1:length(divList_s_adhoc))
      {
        
        projectname_taskaddlog_adhoc<-c(projectname_taskaddlog_adhoc,(eval(parse(text = paste0("input$",projectname_adds_adhoc[i],sep="")))))
        details_taskaddlog_adhoc<-c(details_taskaddlog_adhoc,(eval(parse(text = paste0("input$",details_adds_adhoc[i],sep="")))))
        category_taskaddlog_adhoc<-c(category_taskaddlog_adhoc,(eval(parse(text = paste0("input$",category_adds_adhoc[i],sep="")))))
        priority_taskaddlog_adhoc<-c(priority_taskaddlog_adhoc,(eval(parse(text = paste0("input$",priority_adds_adhoc[i],sep="")))))
        fte_taskaddlog_adhoc<-c(fte_taskaddlog_adhoc,(eval(parse(text = paste0("input$",fte_adds_adhoc[i],sep="")))))
        status_taskaddlog_adhoc<-c(status_taskaddlog_adhoc,(eval(parse(text = paste0("input$",status_adds_adhoc[i],sep="")))))
        
      }}
    
    
    #
    a<-c()
    
    for( i in 1:length(details_taskaddlog_adhoc)){
      
      
      a<-c(a,sum((projectname_taskaddlog_adhoc[i])!=""),sum((details_taskaddlog_adhoc[i])!=""),sum((category_taskaddlog_adhoc[i])!=""),sum((priority_taskaddlog_adhoc[i])!=""),
           sum(!is.na(fte_taskaddlog_adhoc[i])),sum((status_taskaddlog_adhoc[i])!=""))
    }
    
    m<-input$rscWDmonth2
    
   
    
    dat<-RVrsctemp2$data
    col<-colnames(dat)
    if(var(a)==0)
    {
      
      
      
      for( i in 1:length(details_taskaddlog_adhoc)){
        
        tu<-tasksMain
        h<-tu[tu[,1]==mainteamcheck,]
        h<-h[h[,2]==subteamcheck,]
        h<-h[h[,3]=="Adhoc",] 
        h<-h[!is.na(h$Project.Name),]
        n<-nrow(h)
        pkadhoc<-paste(mainteamcheck,subteamcheck,"Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc",paste0("T",n+1),m,sep="_",collapse=NULL)
        b<-c(mainteamcheck,subteamcheck,projectname_taskaddlog_adhoc[i],details_taskaddlog_adhoc[i],category_taskaddlog_adhoc[i],priority_taskaddlog_adhoc[i],fte_taskaddlog_adhoc[i],status_taskaddlog_adhoc[i],pkadhoc)
        globaladhoc<<-c(globaladhoc,pkadhoc)
        tasksMain[nrow(tasksMain)+1,] <- b
        
        for(j in 2:ncol(dat)){
          
          
          pad<-paste0(pkadhoc,"_",col[j],"_",m)
          taskresource[nrow(taskresource)+1,]<-c(pad,0)
          
          
        }
        
        
        
      }
      #
      write.table(x = tasksMain,file = "./data/Tasks.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      #
      #Update Projects
      pkc<-paste(mainteamcheck,subteamcheck,"Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc",sep="_",collapse=NULL)
      bproj<-c(mainteamcheck,subteamcheck,"Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc","Adhoc",pkc)
      
      
      if(! pkc%in% projectsMain$Primary.Key ){
        
        projectsMain[nrow(projectsMain)+1,] <-c(bproj,as.numeric(projectsMain[nrow(projectsMain),13])+1,"In Progress")
        
        write.table(x = projectsMain,file = "./data/Projects.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      }
      
      
      
      
      #
      
      updateTextInput(session,paste0("projectname_taskadd_adhoc"),label="Project Name",value = "Adhoc")
      updateTextInput(session,paste0("details_taskadd_adhoc"),label="Task Details",value = "")
      updateSelectInput(session,paste0("category_taskadd_adhoc"), label="Task Category",selected=NULL,choices=c("",unique(Targetaudience)))
      updateSelectInput(session,paste0("priority_taskadd_adhoc"), label="Priority",selected=NULL,choices = c("",priority))
      updateNumericInput(session,paste0("fte_taskadd_adhoc"),label="Total FTE days",value="")
      updateSelectInput(session,paste0("status_taskadd_adhoc"),label="Status", selected=NULL,choices = c("",status))
      
      
      #id<-as.integer(input$AddRow_scoping)
      
      #
      if (length(divList_s_adhoc) != 0)
      {
        for(i in length(divList_s_adhoc):1)
        {
          
          removeUI(
            selector = paste0("div:has(> #projectname_taskadd_adhoc",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          removeUI(
            selector = paste0("div:has(> #details_taskadd_adhoc",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          removeUI(
            selector = paste0("div:has(> #category_taskadd_adhoc",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          removeUI(
            selector = paste0("div:has(> #priority_taskadd_adhoc",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          removeUI(
            selector = paste0("div:has(> #fte_taskadd_adhoc",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          removeUI(
            selector = paste0("div:has(> #status_taskadd_adhoc",i,")"),
            multiple = TRUE,
            immediate = TRUE,
            session
          )
          
          
          
          
        }}
      
      # 
      g<-RVrsctemp2$data
      a<-as.data.frame(matrix(0, ncol = ncol(g), nrow = length(details_taskaddlog_adhoc)))
      
      a[,1]<-details_taskaddlog_adhoc
      colnames(a)<-colnames(g)
      RVrsctemp2$data<-rbind(RVrsctemp2$data,a)
      RVrsctemp2edited$data<-rbind(RVrsctemp2edited$data,a)
      refreshvectorsadhoc()
    }
    
    
    
    else{
      showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
      
    }
    
    
  })
  
  
  
  
  
  
  
  observeEvent(list(input$mainTeamreport), {
    filtered <- team_information[team_information$Team == input$mainTeamreport,]
    updateSelectInput(session,"Subteamreport", choices = c(as.vector(unique(filtered[,4]))), selected = '')
  })
  
  observeEvent(list(input$mainTeamreport2), {
    filtered <- team_information[team_information$Team == input$mainTeamreport2,]
    updateSelectInput(session,"Subteamreport2", choices = c(as.vector(unique(filtered[,4]))), selected = '')
  })
  
  
  
  
  
  
  
  
  
  output$Utilization_by_task_type <- renderPlot({
    
    
    
    req(input$mainTeamreport)
    req(input$Subteamreport)
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    
  
    data<-data[!is.na(data$`Project Name`),]
   
    tempdat<-data[data$`Project Name`=="Adhoc", ]
    data<-data[!(data$`Project Name`=="Adhoc"), ]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    
    
    a1<-input$mainTeamreport
      a2<-input$Subteamreport
    
    
    
    data<-data[data$Category=="Actuals",]
    data<-data[data$Team==input$mainTeamreport,]
    data<-data[data$`Sub Team`==input$Subteamreport,]
    
    
    t<-tasksMain
    
    datatask<-t %>% separate(Primary.Key, 
                             c("Team", "Sub Team","Project Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber"),
                             extra='drop',fill = "right",sep ='_')
    
    datatask<-datatask[!is.na(datatask$Project.Name),]
    datuse<-as.data.frame(cbind( datatask$Team, datatask$Subteam ,datatask$`Project Name`,datatask$Task.category,datatask$Tasknumber))
    datuse[] <- lapply(datuse, as.character)
    g<- left_join(data, datuse, by = c("Team" = "V1" ,"Sub Team" = "V2", "Project Name" = "V3", "Task" = "V5" ))
    
    m1<-input$startreport
    m2<-input$endreport
   
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      g<-g[g$Month %in% months,]
      
    }
    
    
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
       
        
      }
    
      g<-g[g$Month %in% months,]
    
    }
    
  else{
    
   
    months<-unique( data$Month )
    
    
    
    
  }
    
    
    ######Leaves
    
  
    
    leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
    
    leavelog<-leaves %>% separate(Primary.Key, 
                                  c("Team", "Sub Team","Month","Employee","Category"),
                                  extra='drop',fill = "right",sep ='_') 
    
    
    leavelog<-leavelog[leavelog$Team==a1 & leavelog$`Sub Team`==a2 & leavelog$Month%in%months & leavelog$Category=="Actuals",]
    
    leave1<-sqldf("Select  `Employee`, sum(Leaves) as PL
                        from leavelog
                  group by 1 ", stringsAsFactors = FALSE)
    
    
    leave1$"Project_Name"<-rep("Leaves",nrow(leave1))
    leave1<-leave1[,c(3,1,2)]
    
   
    
     summary_data<-sqldf("Select V4, `Employee Name`, sum(Planned_Days) as PL
                        from g
                        group by 1,2 ", stringsAsFactors = FALSE)
    
    colnames(leave1)<-colnames(summary_data)
    
    summary_data<-rbind(leave1,summary_data)
    
    
    teamall<-sqldf("Select V4,  sum(PL) as PL2
                        from summary_data
                        group by 1 ", stringsAsFactors = FALSE)
    
    teamlist<-rep("Team",nrow(teamall))
    c1<-teamall$V4
    c2<-as.integer(teamall$PL2)
    # 
    
    teamall<-cbind(teamlist,c2)
    teamall<-as.data.frame(teamall)
    teamall<-cbind(c1,teamall)
    colnames(teamall)<-colnames(summary_data)
    
    teamall$PL<-as.numeric(as.character(teamall$PL))
    
    
    
    summary_data<-rbind(teamall,summary_data)
    summary_data$PL<-as.numeric(summary_data$PL)
    summary_data<-group_by(summary_data, `Employee Name`) %>% mutate(percent_value = PL/sum(PL))
    
    
   summary_data<- summary_data[!is.na(summary_data$V4),]
   
   
   summary_data$percent_value[is.na(summary_data$percent_value)]<-0
    
    #
    
    if(nrow(summary_data)!=0){
      
      ggplot()+
        geom_bar(data = summary_data, aes(x = `Employee Name`, y = percent_value, fill = V4), stat="identity",position='stack') + 
        geom_text(data = summary_data, aes(x = `Employee Name`, y = percent_value,fill = V4,
                                           label = paste0(sprintf("%1.0f", percent_value*100),"%")), color = "white", size=5, face="bold", position = position_stack(vjust = 0.5)) +
        #coord_flip() +
        #facet_grid( ~ Team) +
        ggtitle("Utilization by Task Type") +
        theme(legend.position="top", 
              plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5, vjust=0.5),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(face = "bold", size = 14),
              axis.text.y = element_blank(),
              legend.text = element_text(color="black", size=12, face="bold",vjust=0.5),
              #panel.background = element_rect(fill = "#ecf0f5", color="#ecf0f5"),
              plot.background=element_rect(fill = "#ecf0f5"),
              legend.background = element_rect(fill = "#ecf0f5"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.5,"cm"))
    }
    
  })
  
  
  
  
  output$Utilization_by_proj_class <- renderPlot({
    
    
    
    
    
    req(input$mainTeamreport2)
   a1<-input$mainTeamreport2
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
  
    
    
    tempdat<-data[data$'Project Name'=="Adhoc", ]
    data<-data[!(data$'Project Name'=="Adhoc"), ]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    
    
    
  
    data<-data[!is.na(data$`Project Name`),]
    
   # ##
    data<-data[data$Category=="Actuals",]
    data<-data[data$Team==input$mainTeamreport2,]
    m1<-input$startreport3
    m2<-input$endreport3
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      
      data<-data[data$Month %in% months,]
      
      
      t<-team_information
      t<-t[t[,3]==input$mainTeamreport2,]
     
      
      empname<- unique(t$Employee)
      
      empname<-empname[empname!=""]
      
      
      fte<<-length(empname)
      
      data1<-taskresource %>% separate(Primary.Key, 
                                      c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month","M"),
                                      extra='drop',fill = "right",sep ='_') #%>% data
      
      
      
      
      
      
      
      
      data1<-data1[data1$Team==input$mainTeamreport2,]
      
      adhocc<-data1[data1$`Project Name`=="Adhoc",]
      data2<-data1[!data1$`Project Name`=="Adhoc",]
      
      adhocc<-adhocc[adhocc$M %in% months,]
      
      data2<-data2[data2$Month %in% months,]
      
      
     
      ####LEAVES
      if(a1!=""){
      leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
      
      leavelog<-leaves %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Month","Employee","Category"),
                                    extra='drop',fill = "right",sep ='_') 
      
      
      leavelog1<-leavelog[leavelog$Team==a1 & leavelog$Month%in%months & leavelog$Category=="Actuals",]
      
      leavelogactuals<-sqldf("Select  Team, sum(Leaves) as PL
              from leavelog1
              group by 1 ", stringsAsFactors = FALSE)
      
      leavelog2<-leavelog[leavelog$Team==a1 & leavelog$Month%in%months & leavelog$Category=="Planned",]
     
        
        leavelogplanned<-sqldf("Select  Team, sum(Leaves) as PL
              from leavelog2
                               group by 1 ", stringsAsFactors = FALSE)
        
        
        
       
        
        leavelog<-sqldf("Select `Sub Team` , sum(Leaves) as PL
              from leavelog1
              group by 1 ", stringsAsFactors = FALSE)
        
        
        
        
        
        }
      else{
        
        
        leavelogactuals<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
        leavelogplanned<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
        leavelog<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
        
      }
          
      #
      
      Actuals<-rbind(data2[data2$Category=="Actuals",],adhocc[adhocc$Month=="Actuals",])
      Planned<-rbind(data2[data2$Category=="Planned",],adhocc[adhocc$Month=="Planned",])
      
      
      actsum<-sum(Actuals$Planned_Days,leavelogactuals[,2])
      plansum<-sum(Planned$Planned_Days,leavelogplanned[,2])
      
      
    
      
      
      
      
      
      
      
      wd<-0
      
      for(x in 1:length(months)){
        
        
        b<-months[x]
        
        
        a<-as.Date(paste(as.character(b),"01"), format = "%B %Y %d")
        # a<-as.POSIXct(paste(as.character(b),"01"), format = "%B %Y %d")
        start_date <- as.Date(a, "%m/%d/%Y")
        num<-lubridate::days_in_month(start_date)
        end_date<-ceiling_date(start_date,"month")-1
        sat_sun_cnt<-Nweekends(start_date, end_date)
        holiday_cnt_MSD<-as.numeric(Nholidays(start_date, end_date, "MSD"))
        holiday_cnt_AR<-as.numeric(Nholidays(start_date, end_date, "AR"))
        
        total_WD1<-as.numeric(num-sat_sun_cnt-max(holiday_cnt_MSD,holiday_cnt_AR))
        
        wd<-sum(wd,total_WD1)

        
      }
      
      # 
      fteactual<<-actsum/wd
      fteplanned<<-plansum/wd
      fte<<-round(fte,digits=1)
      fteplanned<<-round(fteplanned,digits=1)
      fteactual<<-round(fteactual,digits=1)
      
      
      
      
      
      
      
      output$boxesfte <- renderUI({
        
        
        box(id="boxfte",
            title = fte, width = 1, background = "light-blue",
            "FTE Count:"
        )
        
      })
      output$boxesfte2 <- renderUI({
        
        
        box(id="boxfte2",
            title = fteplanned, width = 1, background = "light-blue",
            "Planned FTE:"
        )
        
      })
      output$boxesfte3 <- renderUI({
        
        
        box(id="boxfte3",
            title = fteactual, width = 1, background = "light-blue",
            "Actual FTE:"
        )
        
        
        
      })
      
      
      
      
    }
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        
        
      }
      
      data<-data[data$Month %in% months,]
      
      t<-team_information
      t<-t[t[,3]==input$mainTeamreport2,]
      
      
      empname<- unique(t$Employee)
      
      empname<-empname[empname!=""]
      
      
      fte<<-length(empname)
      
      data1<-taskresource %>% separate(Primary.Key, 
                                       c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month","M"),
                                       extra='drop',fill = "right",sep ='_') #%>% data
      
      
      data1<-data1[data1$Team==input$mainTeamreport2,]
      
      adhocc<-data1[data1$`Project Name`=="Adhoc",]
      data2<-data1[!data1$`Project Name`=="Adhoc",]
      
      adhocc<-adhocc[adhocc$M %in% months,]
      
      data2<-data2[data2$Month %in% months,]
      
      ####LEAVES
      
      
      if(a1!=""){
      
      
      leaves <-
        read.csv("./data/leaves.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)
      
      leavelog <- leaves %>% separate(
        Primary.Key,
        c("Team", "Sub Team", "Month", "Employee", "Category"),
        extra = 'drop',
        fill = "right",
        sep = '_'
      )
      
      
      leavelog1 <-
        leavelog[leavelog$Team == a1 &
                   leavelog$Month %in% months & leavelog$Category == "Actuals", ]
      
      leavelogactuals <- sqldf("Select  Team, sum(Leaves) as PL
                               from leavelog1
                               group by 1 ",
                               stringsAsFactors = FALSE)
      
      leavelog2 <-
        leavelog[leavelog$Team == a1 &
                   leavelog$Month %in% months & leavelog$Category == "Planned", ]
      
      
      leavelogplanned <- sqldf("Select  Team, sum(Leaves) as PL
                               from leavelog2
                               group by 1 ",
                               stringsAsFactors = FALSE)
      
      
      
      
      
      leavelog <- sqldf("Select `Sub Team` , sum(Leaves) as PL
                        from leavelog1
                        group by 1 ",
                        stringsAsFactors = FALSE)
      
      
    }
   
   else{
     leavelogactuals <- as.data.frame(matrix(0, ncol = 2, nrow = 1))
     leavelogplanned <-
       as.data.frame(matrix(0, ncol = 2, nrow = 1))
     leavelog <- as.data.frame(matrix(0, ncol = 2, nrow = 1))
     
   }
      
      
      Actuals<-rbind(data2[data2$Category=="Actuals",],adhocc[adhocc$Month=="Actuals",] )
      Planned<-rbind(data2[data2$Category=="Planned",],adhocc[adhocc$Month=="Planned",] )
      
      
      actsum<-sum(Actuals$Planned_Days,leavelogactuals[,2] )
      plansum<-sum(Planned$Planned_Days,leavelogplanned[,2] )
      
      
      
      
      
      
      wd<- 0
      
      for(x in 1:length(months)){
        
        
        b<-months[x]
        
        
        a<-as.Date(paste(as.character(b),"01"), format = "%B %Y %d")
        # a<-as.POSIXct(paste(as.character(b),"01"), format = "%B %Y %d")
        start_date <- as.Date(a, "%m/%d/%Y")
        num<-lubridate::days_in_month(start_date)
        end_date<-ceiling_date(start_date,"month")-1
        sat_sun_cnt<-Nweekends(start_date, end_date)
        holiday_cnt_MSD<-as.numeric(Nholidays(start_date, end_date, "MSD"))
        holiday_cnt_AR<-as.numeric(Nholidays(start_date, end_date, "AR"))
        
        total_WD1<-as.numeric(num-sat_sun_cnt-max(holiday_cnt_MSD,holiday_cnt_AR))
        
        wd<-sum(wd,total_WD1)
        
        
      }
      
      
      fteactual<<-actsum/wd
      fteplanned<<-plansum/wd
      fte<<-round(fte,digits=1)
      fteplanned<<-round(fteplanned,digits=1)
      fteactual<<-round(fteactual,digits=1)
      
      
      
      
      
      
      
      output$boxesfte <- renderUI({
        
        
        box(id="boxfte",
            title = fte, width = 1, background = "light-blue",
            "FTE Count:"
        )
        
      })
      output$boxesfte2 <- renderUI({
        
        
        box(id="boxfte2",
            title = fteplanned, width = 1, background = "light-blue",
            "Planned FTE:"
        )
        
      })
      output$boxesfte3 <- renderUI({
        
        
        box(id="boxfte3",
            title = fteactual, width = 1, background = "light-blue",
            "Actual FTE:"
        )
        
        
        
      })
      
      
      
      
      
      
      
      
    }
    
    
    
    else{
      
      
      months<-month_list_scope
     
      ####LEAVES
      if(a1!=""){
      leaves <-
        read.csv("./data/leaves.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)
      
      leavelog <- leaves %>% separate(
        Primary.Key,
        c("Team", "Sub Team", "Month", "Employee", "Category"),
        extra = 'drop',
        fill = "right",
        sep = '_'
      )
      
      
      leavelog1 <-
        leavelog[leavelog$Team == a1 &
                   leavelog$Month %in% months & leavelog$Category == "Actuals", ]
      
      leavelogactuals <- sqldf("Select  Team, sum(Leaves) as PL
                               from leavelog1
                               group by 1 ",
                               stringsAsFactors = FALSE)
      
      leavelog2 <-
        leavelog[leavelog$Team == a1 &
                   leavelog$Month %in% months & leavelog$Category == "Planned", ]
      
      
      leavelogplanned <- sqldf("Select  Team, sum(Leaves) as PL
                               from leavelog2
                               group by 1 ",
                               stringsAsFactors = FALSE)
      
      
      
      
      
      leavelog <- sqldf("Select `Sub Team` , sum(Leaves) as PL
                        from leavelog1
                        group by 1 ",
                        stringsAsFactors = FALSE)
        
        
        
        
        
        
        
      }
      else{
        
        leavelogactuals<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
        leavelogplanned<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
        leavelog<-as.data.frame(matrix(0, ncol = 2, nrow = 1))
      }
      
      datatemp<-taskresource %>% separate(Primary.Key, 
                                          c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                          extra='drop',fill = "right",sep ='_') #%>% data
      
      
      
      
      tempdat<-datatemp[datatemp$'Project Name'=="Adhoc", ]
      datatemp<-datatemp[!(datatemp$'Project Name'=="Adhoc"), ]
      tempdat<-tempdat[,c(1:13,15,16,14,17)]
      
      colnames(tempdat)<-colnames(datatemp)
      datatemp<-rbind(datatemp,tempdat)
      
      
      
      
      datatemp<-datatemp[!is.na(datatemp$`Project Name`),]
      
      Actuals<-datatemp[datatemp$Category=="Actuals",]
      Planned<-datatemp[datatemp$Category=="Planned",]
      
      
      actsum<-sum(Actuals$Planned_Days,leavelogactuals[,2])
      plansum<-sum(Planned$Planned_Days,leavelogplanned[,2])
      
      employees<- unique(team_information$Employee[team_information$Team==a1 & team_information$Employee!="" ])
      
      fte<-length(employees)
      
      
      wd<-0
      
      for(x in 1:length(months)){
        
        
        b<-months[x]
        
        
        a<-as.Date(paste(as.character(b),"01"), format = "%B %Y %d")
        # a<-as.POSIXct(paste(as.character(b),"01"), format = "%B %Y %d")
        start_date <- as.Date(a, "%m/%d/%Y")
        num<-lubridate::days_in_month(start_date)
        end_date<-ceiling_date(start_date,"month")-1
        sat_sun_cnt<-Nweekends(start_date, end_date)
        holiday_cnt_MSD<-as.numeric(Nholidays(start_date, end_date, "MSD"))
        holiday_cnt_AR<-as.numeric(Nholidays(start_date, end_date, "AR"))
        
        total_WD1<-as.numeric(num-sat_sun_cnt-max(holiday_cnt_MSD,holiday_cnt_AR))
        
        wd<-sum(wd,total_WD1)
        
        
      }
      
      
      fteactual<<-actsum/wd
      fteplanned<<-plansum/wd
      fte<<-round(fte,digits=1)
      fteplanned<<-round(fteplanned,digits=1)
      fteactual<<-round(fteactual,digits=1)
      
      
      
      
      
      
      
      output$boxesfte <- renderUI({
        
        
        box(id="boxfte",
            title = fte, width = 1, background = "light-blue",
            "FTE Count:"
        )
        
      })
      output$boxesfte2 <- renderUI({
        
        
        box(id="boxfte2",
            title = fteplanned, width = 1, background = "light-blue",
            "Planned FTE:"
        )
        
      })
      output$boxesfte3 <- renderUI({
        
        
        box(id="boxfte3",
            title = fteactual, width = 1, background = "light-blue",
            "Actual FTE:"
        )
        
        
        
      })
      
      
      
      
      
    }
    
    
    
    
    
    
    
    
    
    leavelog$leave<- rep("Leaves",nrow(leavelog))
    leavelog<-leavelog[,c(3,1,2)]
    
    leavelog<-leavelog[(leavelog[,2]!=0 ) & (!is.na(leavelog[,2])) & (leavelog[,2]!="Adhoc" ), ]
    
    
    
    
    #browse
    
    summary_data<-sqldf("Select Classification, `Sub Team`, sum(Planned_Days) as PL
                        from data
                        group by 1,2 ", stringsAsFactors = FALSE)
    
    
    
    summary_data<-summary_data[!is.na(summary_data$`PL`),]
    colnames(leavelog)<-colnames(summary_data)
    
    summary_data<-rbind(leavelog,summary_data)
   

                 
    teamall<-sqldf("Select Classification,  sum(PL) as PL2
                        from summary_data
                   group by 1 ", stringsAsFactors = FALSE)
    
    teamlist<-rep("All Subteams",nrow(teamall))
    
    teamall<-cbind(teamall$Classification,teamlist,teamall$PL2)
    teamall<-as.data.frame(teamall)
    colnames(teamall)<-colnames(summary_data)
    summary_data<-rbind(summary_data,teamall)
    summary_data$PL<-as.numeric(summary_data$PL)
    
    
    
    
    
    
    
    
    
    summary_data<-group_by(summary_data, `Sub Team`) %>% mutate(percent_value = PL/sum(PL))
    summary_data<- summary_data[!is.na(summary_data$Classification),]
    
    summary_data$percent_value[is.na(summary_data$percent_value)]<-0
    
    ## 
    if(nrow(summary_data)!=0){
      
      ggplot()+
        geom_bar(data = summary_data, aes(x = `Sub Team`, y = percent_value, fill = Classification), stat="identity",position='stack') + 
        geom_text(data = summary_data, aes(x = `Sub Team`, y = percent_value,fill = Classification,
                                           label = paste0(sprintf("%1.0f", percent_value*100),"%")), color = "white", size=5, face="bold", position = position_stack(vjust = 0.5)) +
        #coord_flip() +
        #facet_grid( ~ Team) +
        ggtitle("Utilization by Project Classification") +
        theme(legend.position="top", 
              plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5, vjust=0.5),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(face = "bold", size = 14 ), #, angle = 50, vjust = 0.5, hjust=1
              axis.text.y = element_blank(),
              legend.text = element_text(color="black", size=12, face="bold",vjust=0.5),
              #panel.background = element_rect(fill = "#ecf0f5", color="#ecf0f5"),
              plot.background=element_rect(fill = "#ecf0f5"),
              legend.background = element_rect(fill = "#ecf0f5"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.5,"cm"))
    }
    
    
  })
  
  
  
  
  
  
  
  
  output$Utilization_by_task_typeGCF <- renderPlot({
    
    
    
    
    
    req(input$mainTeamreport2)
    a1<-input$mainTeamreport2
    
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    data<-data[!is.na(data$`Project_Name`),]
    
  
  tempdat<-data[data$Project_Name=="Adhoc",]
  data<-data[!(data$Project_Name=="Adhoc"),]
  tempdat<-tempdat[,c(1:13,15,16,14,17)]
  
  colnames(tempdat)<-colnames(data)
  data<-rbind(data,tempdat)
  
  
    
  data<-data[data$Team==input$mainTeamreport2,]
  
  
   
    
    t<-tasksMain
    
    datatask<-t %>% separate(Primary.Key, 
                             c("Team", "Sub Team","Project_Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber"),
                             extra='drop',fill = "right",sep ='_')
    
    datatask<-datatask[!is.na(datatask$Project_Name),]
    
    datuse<-as.data.frame(cbind( datatask$Team, datatask$Subteam ,datatask$`Project_Name`,datatask$Task.category,datatask$Tasknumber))
    datuse[] <- lapply(datuse, as.character)
    g<- left_join(data, datuse, by = c("Team" = "V1" ,"Sub Team" = "V2", "Project_Name" = "V3", "Task" = "V5" ))
    
    data<-g
    
    
     data<-data[data$Category=="Actuals",]
    
    
    # months<-month_list_scope
    
    
    m1<-input$startreport3
    m2<-input$endreport3
    
    
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      data<-data[data$Month %in% months,]
      
    }
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        
        
      }
      
      data<-data[data$Month %in% months,]
      
    }
    
    
    else{
      
      
      months<-unique( data$Month )
      
      
      
      
    }
    
    
 
    
 leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
 
 leavelog<-leaves %>% separate(Primary.Key, 
                               c("Team", "Sub Team","Month","Employee","Category"),
                               extra='drop',fill = "right",sep ='_') 
 
 
 leavelog<-leavelog[leavelog$Team==a1 &  leavelog$Month%in%months & leavelog$Category=="Actuals",]
 
 leave1<-sqldf("Select  `Sub Team`, sum(Leaves) as PL
               from leavelog
               group by 1 ", stringsAsFactors = FALSE)
 
 
 leave1$"Project_Name"<- rep("Leaves",nrow(leave1))
 leave1<-leave1[,c(3,1,2)]
 
    
    
   # 
    summary_data<-sqldf("Select V4, `Sub Team`, sum(Planned_Days) as PL
                        from data
                        group by 1,2 ", stringsAsFactors = FALSE)
    
    
    colnames(leave1)<-colnames(summary_data)
    
    summary_data<-rbind(leave1,summary_data)
    teamall<-sqldf("Select V4,  sum(PL) as PL2
                        from summary_data
                   group by 1 ", stringsAsFactors = FALSE)
    
    teamlist<-as.data.frame(rep("All Subteams",nrow(teamall)))
    
    teamall<-cbind(teamall$V4,teamlist,teamall$PL2)
    
    teamall<-as.data.frame(teamall)
    colnames(teamall)<-colnames(summary_data)
    summary_data<-rbind(summary_data,teamall)
    
    
    
    summary_data$PL<-as.numeric(as.character(summary_data$PL))
    
    
    
    
    
    
     
    
    
    
    
    
    
    summary_data<-group_by(summary_data, `Sub Team`) %>% mutate(percent_value = PL/sum(PL))
    
    summary_data<- summary_data[!is.na(summary_data$V4),]
    
    
    summary_data$percent_value[is.na(summary_data$percent_value)]<-0
    ## 
    if(nrow(summary_data)!=0){
      
      ggplot()+
        geom_bar(data = summary_data, aes(x = `Sub Team`, y = percent_value, fill = V4), stat="identity",position='stack') + 
        geom_text(data = summary_data, aes(x = `Sub Team`, y = percent_value,fill = V4,
                                           label = paste0(sprintf("%1.0f", percent_value*100),"%")), color = "white", size=5, face="bold", position = position_stack(vjust = 0.5)) +
        #coord_flip() +
        #facet_grid( ~ Team) +
        ggtitle("Utilization by Task Type") +
        theme(legend.position="top", 
              plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5, vjust=0.5),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(face = "bold", size = 14), #, angle = 50, vjust = 0.5, hjust=1 
              axis.text.y = element_blank(),
              legend.text = element_text(color="black", size=12, face="bold",vjust=0.5),
              #panel.background = element_rect(fill = "#ecf0f5", color="#ecf0f5"),
              plot.background=element_rect(fill = "#ecf0f5"),
              legend.background = element_rect(fill = "#ecf0f5"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.5,"cm"))
    }
    
    
  })
  
  
  output$UTPCGCF<-DT :: renderDataTable({
    
    
    
    #  # #
    
    m1<-input$startreport2
    m2<-input$endreport2
    
    cbind.fill <- function(...){
      nm <- list(...)
      nm <- lapply(nm, as.matrix)
      n <- max(sapply(nm, nrow))
      do.call(cbind, lapply(nm, function (x)
        rbind(x, matrix(, n-nrow(x), ncol(x)))))
    }
    
    
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    
    
    data<-data[data$Category=="Actuals",]
    data<-data[data$Team==input$mainTeamreport2,]
    
    
    
    
    summary_data<-sqldf("select  `Sub Team` , `Project Name`
                         from data 
                        where `Project Name`!= 'Adhoc'
                        ", stringsAsFactors=FALSE)
    
    #  # #
    
    
    
    a<-unique(summary_data$`Sub Team`)
    
    final<-data.frame()
    if(nrow(summary_data)!=0){
      for(i in 1:length(a)){
        
        e<-c()
        for(j in 1:nrow(summary_data)){
          
          ## 
          if(summary_data$`Sub Team`[j]==a[i]){
            
            e<-c(e,summary_data$`Project Name`[j])
          }
          
          
        }
        
        e<-unique(e)
        e<-data.frame(e)
        final<-as.data.frame(cbind.fill(final,e))
        
      }
      
      
      
      
      out <- final
      
      datatable(out, options = list(autoWidth=TRUE),colnames = a)}
    
    
  })
  
  
  
  
  
  observeEvent(input$Leadership_data_reports_tasks_rows_selected, {
    
    
    #
    
    
    a<-input$Leadership_data_reports_tasks_rows_selected
    
    dat<-RVxyz$data
    
    
    
    RVname$data<-dat[a,1]
    
    
    
    
    
    
    
    
  }) 
    
    
    
    
    
    
   
  output$Leadership_data_reports<-DT :: renderDataTable({
    
    
    
    
    
    
    #
    if(!is.null(RVname$data)){
     
    req(input$mainTeamreport)
    req(input$Subteamreport)
    
    a1<-input$mainTeamreport
    a2<-input$Subteamreport
     p1<-RVname$data
     
     t<-tasksMain
     t<-t[!is.na(t$Project.Name),]
     t<-t[t$Team==a1,]
     t<-t[t$Subteam==a2,]
     t<-t[t$Project.Name==p1,]
     datatask<-t %>% separate(Primary.Key,
                                     c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand",
                                       "Project End Month","Status","Task","Employee_Name","Category","Month"),
                                     extra='drop',fill = "right",sep ='_') #%>% data
     
    data<-taskresource %>% separate(Primary.Key,
                                    c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand",
                                      "Project End Month","Status","Task","Employee_Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    
    
    tempdat<-data[data$Project_Name=="Adhoc",]
    data<-data[!(data$Project_Name=="Adhoc"),]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    data<-data[!is.na(data$Project_Name),]
    
    data<-data[data$Team==a1,]
    data<-data[data$`Sub Team`==a2,]
    data<-data[data$`Project_Name`==p1,]
    
    
    
    
    datatask<-datatask[,c(3,19)]
    colnames(datatask)<-c("TaskName","Task")
    
    data$TaskName <- datatask$TaskName[match(data$Task, datatask$Task)]
    
    
    m1<-input$startreport
    m2<-input$endreport
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      data1<-data[data$Month %in% months,]
      
   
      months2<-month_list_scope[n1:length(month_list_scope)]
      data2<-data[!(data$Month %in% months2),]
      data2<-data2[(data2$TaskName%in%data1$TaskName),]
      test33<-sqldf("select [Employee_Name], TaskName, Category, sum(Planned_Days) as [Actual_Days]
                        from data2
                  group by [Employee_Name], TaskName, Month,Category" )
  
      
      
      test22<-sqldf("select [Employee_Name], TaskName, Category, sum(Planned_Days) as [Actual_Days]
                        from data1
                        group by [Employee_Name], TaskName, Month,Category" )
      
      if(nrow(test33)==0){
        
        testfinal<-test22
        
        
      }
      
      else{
        
        
        
      test33$Employee_Name<-"Previous"
      
      testfinal<-rbind(test33,test22)
      
      
      }
      
      
      
    }
   
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        months2<-month_list_scope[n1:length(month_list_scope)]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        #months2<-month_list_scope[n1:length(month_list_scope)]
        
        months2<-month_list_scope
        
        
        
      }
      
      
      
      
      
      
      
      data1<-data[data$Month %in% months,]
      
      
      
      data2<-data[!(data$Month %in% months2),]
      
      
      
      data2<-data2[(data2$TaskName%in%data1$TaskName),]
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      test33<-sqldf("select [Employee_Name], TaskName, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                    from data2
                    group by [Employee_Name], TaskName, Month,Category" )
      
      test22<-sqldf("select [Employee_Name], TaskName, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                    from data1
                    group by [Employee_Name], TaskName, Month,Category" )
      
      if(nrow(test33)!=0){
        test33$Employee_Name<-"Previous"
        testfinal<-rbind(test33,test22)
        
        
        
      }
      else{
        
        testfinal<-test22
        
      }
      
      
    }
    
    
    else{
      
     
     
      testfinal<-sqldf("select Employee_Name, TaskName, Category, sum(Planned_Days) as [Actual_Days]
                        from data
                        group by Employee_Name, TaskName, Month,Category" )
      
      
      
    }
    
    
    
   
    
    
    if(nrow(testfinal)!=0){
    finaltest22<-reshape2::dcast(testfinal, TaskName+Category ~ Employee_Name, value.var = c('Actual_Days'),sum)
   
    col<-colnames(finaltest22)
    
    
   
    if("Previous"%in%col){
    
    
    n<-as.numeric(which(col=="Previous"))
    
    if(n!=3){
      
      if(n!=length(col)){
        finaltest22<-finaltest22[,c(1,2,n,3:(n-1),(n+1):length(col))]
        
      }
    else{
      
      finaltest22<-finaltest22[,c(1,2,n,3:(n-1))]
      
    }
    }
    
    
   
    }
    
    x<-ncol(finaltest22)
    #
    if(x!=3){
    finaltest22[,c(3:x)]<-lapply(finaltest22[,c(3:x)], as.numeric)
    
    finaltest22$'Project Total' <- apply(finaltest22[,c(3:x)], 1, sum)
    
    }
   
    }
    
    else{
      
      finaltest22<-NULL
    }
    
    
    
    
    if(!is.null(finaltest22)){
    datatable(finaltest22,
              rownames = FALSE,selection = 'single',options = list(autoWidth=TRUE, columnDefs = list(list(className = 'dt-center', targets = "_all")))
    )%>%
      
      formatStyle(
        'Category',target = 'row',
        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        backgroundColor = styleEqual(c('Planned','Actuals'), c('lightgray', 'yellow')),
        fontWeight = "bold"
      )%>%
      
      formatStyle(
        'TaskName',
        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        backgroundColor = 'white',
        fontWeight = "bold"
      )%>%
      
      formatStyle(
        colnames(finaltest22),
        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
       
        fontWeight = "bold"
      )
    }
    
    else{
      
      datatable(finaltest22)
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    }
    
    
    
    
  })   
  
  output$Leadership_data_reports_tasks<-DT :: renderDataTable({
    
    
    
    
    
  
    req(input$mainTeamreport)
    req(input$Subteamreport)
    
    a1<-input$mainTeamreport
    a2<-input$Subteamreport
    
    data<-taskresource %>% separate(Primary.Key,
                                    c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand",
                                      "Project End Month","Status","Task","Employee_Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    ###
    
    data<-data[!is.na(data$Project_Name),]
    
    ##
    data<-data[data$Team==a1,]
    data<-data[data$`Sub Team`==a2,]
    #
    m1<-input$startreport
    m2<-input$endreport
    
    tempdat<-data[data$Project_Name=="Adhoc",]
    data<-data[!(data$Project_Name=="Adhoc"),]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    
    #
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      data1<-data[data$Month %in% months,]
      
      
      
      if(nrow(data1)!=0){
      
      months2<-month_list_scope[n1:length(month_list_scope)]
      }
      else{
        
        months2<-month_list_scope
      }
      
      
      
      
      
      data2<-data[!(data$Month %in% months2),]
      
      data2<-data2[(data2$Project_Name%in%data1$Project_Name),]
      
      test33<-sqldf("select [Employee_Name], Project_Name, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                    from data2
                    group by [Employee_Name], Project_Name, Month,Category" )
      
      test22<-sqldf("select [Employee_Name], Project_Name, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                    from data1
                    group by [Employee_Name], Project_Name, Month,Category" )
      
      
      
      
   
      if(nrow(test33)!=0){
      test33$Employee_Name<-"Previous"
      }
      else if(nrow(test22)!=0){
        
        test33<- test22
        test33$Employee_Name<-"Previous"
        test33$Actual_Days<-0

      }
      testfinal<-rbind(test33,test22)

    }
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        months2<-month_list_scope[n1:length(month_list_scope)]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        #months2<-month_list_scope[n1:length(month_list_scope)]
        
        months2<-month_list_scope
        
        
        
      }
      
      
      
      
      #  
      
      
      
      data1<-data[data$Month %in% months,]
      
      if(nrow(data1)==0){
        
       months2<- month_list_scope
        
      }
      
      data2<-data[!(data$Month %in% months2),]
      data2<-data2[(data2$Project_Name%in%data1$Project_Name),]
     test33<-sqldf("select [Employee_Name], Project_Name, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                    from data2
                    group by [Employee_Name], Project_Name, Month,Category" )
      
      test22<-sqldf("select [Employee_Name], Project_Name, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                    from data1
                    group by [Employee_Name], Project_Name, Month,Category" )
      
      if(nrow(test33)!=0){
        test33$Employee_Name<-"Previous"
        testfinal<-rbind(test33,test22)
        
        
        
      }
      else{
        
        testfinal<-test22
        
      }
      
      
    }
      

    
    
      
    else{
      
      
      months<-month_list_scope
      testfinal<-sqldf("select [Employee_Name], Project_Name, Category,`Project End Month`, sum(Planned_Days) as [Actual_Days]
                       from data
                       group by [Employee_Name], Project_Name, Month,Category" )
      
      
      
    }
    
    
    
    
 

    leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
    
    leavelog<-leaves %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Month","Employee","Category"),
                                    extra='drop',fill = "right",sep ='_') 
    
    
    
    leavelog<-leavelog[leavelog$Team==a1 & leavelog$`Sub Team`==a2 & leavelog$Month%in%months,]
    
   
    leave1<-sqldf("Select  `Employee`, Category,  sum(Leaves) as PL
              from leavelog
                  group by 1,2 ", stringsAsFactors = FALSE)
    
  
    leave1$x<-rep(" ",nrow(leave1))
    leave1$"Project_Name"<-rep("Leaves",nrow(leave1))
    
    leave1<-leave1[,c(1,5,2,4,3)]
    colnames(leave1)<-colnames(testfinal)
   
     testfinal<-rbind(leave1,testfinal)
  
      if(nrow(testfinal)!=0){
     finaltest22<-reshape2::dcast(testfinal, Project_Name+Category+`Project End Month` ~ Employee_Name, value.var = c('Actual_Days'),sum)
    
    col<-colnames(finaltest22)
    
    finaltest22<-finaltest22[,c(1,2,4:length(col),3)]
    
    if("Previous"%in%col){
      
      
      n<-as.numeric(which(col=="Previous"))-1
      
      if(n!=3){
        finaltest22<-finaltest22[,c(1,2,n,3:(n-1),(n+1):length(col))]
      }
      
      
      
    }
    
    x<-ncol(finaltest22)-1
    #
    if(x!=3){
      finaltest22[,c(3:x)]<-lapply(finaltest22[,c(3:x)], as.numeric)
      
      finaltest22$'Project Total' <- apply(finaltest22[,c(3:x)], 1, sum)
      
    }
    
    
    
    #
    
    finaltest22<-finaltest22[,c(1:x,ncol(finaltest22),(x+1))]
    finaltest22$`Project End Month`[finaltest22$`Project End Month`=="Adhoc"]<-""
    
    
    
    
    
    p<-projectsMain
    p<-p[p$Team==a1 & p$Sub_Team==a2,]
    finaltest22$status<-""
    for(i in 1:nrow(finaltest22)){
      
      
      for(j in 1:nrow(p)){
        
        if(p[j,3]==finaltest22[i,1]){
          finaltest22$status[i]<-p[j,14]
        }
       
        
        
      }
      
    }
    
   
    
   
      }
      
      
      
      else{
        
        finaltest22<-NULL
        
      }
    
    RVxyz$data<-finaltest22
    
    if(!is.null(finaltest22)){
    datatable(finaltest22,
              rownames = FALSE,selection = 'single',options = list(autoWidth=TRUE, columnDefs = list(list(className = 'dt-center', targets = "_all"))
                                                                  )
    )%>%
      
      formatStyle(
        'Category',target = 'row',
        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        backgroundColor = styleEqual(c('Planned','Actuals'), c('lightgray', 'yellow'))
      )%>%
      
      formatStyle(
        c('Project_Name','Project End Month','status'),
        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        backgroundColor = 'white',
        fontWeight = "bold"
      )%>%
      
      formatStyle(
        colnames(finaltest22),
        #color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
        
        fontWeight = "bold"
      )
    }
    
    else{
      datatable((finaltest22))
    }
    
    
    
  })   
  
  
  
#####BACKEND  
  
  observeEvent(list(input$backparameter), {
    
    index<-which(colnames(dropdowns)==input$backparameter)
    filtered <- dropdowns[,index]
    updateSelectInput(session,"backlist", choices = c(as.vector(unique(filtered))), selected = '')
    
    
    
  }) 
  
  
  
  observeEvent(list(input$Save_back), {
    
    dropdowns<-read.csv("./data/dropdowns.csv", header = TRUE, stringsAsFactors = FALSE)
    if(input$Save_back>0){
      
      if(is.null(input$backentry)){
        
        showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
         
      }
      
      else
      {
        
        
        if(!grepl("_",input$backentry,  fixed=TRUE)){
        
        colnames<-colnames(dropdowns)
        b<-input$backentry
        index<-which(colnames(dropdowns)==input$backparameter)
        filtered <- dropdowns[,index]
        filtered<-c(filtered,b)
        dropdowns<-dropdowns[,-c(index)]
       
        new<-colnames[index]
        colnames<-colnames[-index]
        dropdowns<-cbind.fill(dropdowns,filtered)
      
        dropdowns<-as.data.frame(dropdowns)
        colnames(dropdowns)<-c(colnames,new)
        write.table(x = dropdowns,file = "./data/dropdowns.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
        
        geography_list<-unique(dropdowns$Geography)
        
        brand <- unique(dropdowns$Brand)
        TA <- unique(dropdowns$TA)
        
        stakeholdernameList<- unique(dropdowns$Stakeholders)
       
        
        updateSelectInput(session,paste0("geography_scope"), label="Geography",selected=NULL,choices = c("",geography_list))
        # updateSelectInput(paste0("stakeholdernames_scope"),label="Stakeholders"))))
        updateSelectInput(session,paste0("TA_scope"),label="TA", selected=NULL,choices=c("",TA))
        updateSelectInput(session,paste0("Brand_scope"),label="Brand", selected=NULL,choices =c("",brand))
      
        
        updateSelectInput(session,paste0("geography_modifying2"), label="Geography",selected=NULL,choices = c("",geography_list))
        # updateSelectInput(paste0("stakeholdernames_scope"),label="Stakeholders"))))
        updateSelectInput(session,paste0("TA_modifying2"),label="TA", selected=NULL,choices=c("",TA))
        updateSelectInput(session,paste0("Brand_modifying2"),label="Brand", selected=NULL,choices =c("",brand))
        
        
        session$reload()
        
      }
        else{
          
          showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error") 
          
        }
      
      }
      
     
   
    }
   
    
  }) 
  
  
  observeEvent(list(input$backteam), {
    
    
    
    filtered <- team_information[team_information$Team == input$backteam,]
    updateSelectInput(session,"backsubteam", choices = c(as.vector(unique(filtered[,4]))), selected = '')
    updateSelectInput(session,"backsubteam1", choices = c(as.vector(unique(filtered[,4]))), selected = '')
    
    
    
    
    
  }) 
  
  
  observeEvent(list(input$Save_back2), {
    
    
    
    if(input$Save_back2>0){ 
    team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)
    a<-input$backentrysub
    b<-input$backteam
    
     if(!grepl("_",input$backentrysub,  fixed=TRUE)){
    d <- as.character(as.POSIXlt(as.Date(Sys.Date())))
    list<-as.data.frame(matrix(0, ncol = 5, nrow = 1))
    list[1,]<-c(d,"",b,a,"")
    colnames(list)<-colnames(team_information)
    team_information<-rbind(team_information,list)
    write.table(x = team_information,file = "./data/Employee_list.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    session$reload()
    
     }
    else{
      
      
      showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error")
    }
    
   }
    
  }) 
  
  
  observeEvent(list(input$backsubteam1), {
    
    team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)
    t<-team_information 
t<-t[t$Team==input$backteam & t$Sub_Team==input$backsubteam1,]
    emp<-unique(t$Employee)
    
    updateSelectInput(session,"backemp", choices = c("",emp), selected = '')
    updateSelectInput(session,"backempdel", choices = c("",emp), selected = '')
    
  })
  
  
  
  observeEvent(list(input$Save_back3), {
    
   
    if(input$Save_back3>0){ 
      if(!is.null(input$backentryemp)){
        
        if(!grepl("_",input$backentryemp,  fixed=TRUE)){
      
      team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)
      a<-input$backsubteam1
      b<-input$backteam
      c<-input$backentryemp
      
      d <- as.character(as.POSIXlt(as.Date(Sys.Date())))
      list<-as.data.frame(matrix(0, ncol = 5, nrow = 1))
      list[1,]<-c(d,c,b,a,"")
      colnames(list)<-colnames(team_information)
      team_information<-rbind(team_information,list)
      write.table(x = team_information,file = "./data/Employee_list.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
      session$reload()
        }
        
        else{
        showNotification("Text fields cannot contain special character '_' ",duration = 10, closeButton = TRUE,type ="error") 
          
        }
      
      }
      else{
        
      showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
        
      }
    }
    
  }) 
  
  
  
  
  
  observeEvent(input$Leadership_data_reports_tasks_rows_selected,{
    
    
    output$editstatus <- renderUI({
      
      
      fluidRow(
        column(1,actionButton("changestatus",tags$b("Change Project Status"), style="color: #0a0a0a; background-color:  #aee0e8; height:60px",width=200))
      
      )
      
    })
  })  
   
  
  observeEvent(input$changestatus,{
    
    
    output$changestatustable <- renderUI({
      
      #
      t<-RVxyz$data
      i<-input$Leadership_data_reports_tasks_rows_selected
      p<-projectsMain
      t<-t[i,]
      a1<-input$mainTeamreport
      a2<-input$Subteamreport
      p<-p[p$Team==a1 & p$Sub_Team==a2 & p$Project_Name==t[1,1] & p$status==t$status & p$End_Month==t$`Project End Month` ,]
     
      ind<-which(p$Primary.Key==list )
      
      
      
    
      
      
      
      
      box(solidHeader = TRUE, collapsible = TRUE, width=600, closable = FALSE, background = 'light-blue',
      fluidRow(
        
        #column(1,offset=1,(div(style="padding: 0px 0px;margin-top:-1em; margin-right:-10em",h5(tags$b(t[1,1]))))),
        column(2,offset=1,div(style="padding: 0px 0px; width: 125px; margin-top:-1em; margin-left:-3em ",(selectInput(paste0( "selectedstatusnew"), label = "Status", choices=c("",status), selected = NULL )))),
        actionButton("updatestatus9",tags$b("Update"), style="color: #0a0a0a; background-color: #aee0e8; height:60px",width=120)
        
        
        
        
      ))
      
    })
  })
  
  observeEvent(input$updatestatus9,{
    
    
    
   
    
    
    t<-RVxyz$data
    i<-input$Leadership_data_reports_tasks_rows_selected
    p<-projectsMain
    t<-t[i,]
    a1<-input$mainTeamreport
    a2<-input$Subteamreport
    p<-p[p$Team==a1 & p$Sub_Team==a2 & p$Project_Name==t[1,1] & p$status==t$status & p$End_Month==t$`Project End Month` ,]
    s<-input$selectedstatusnew
    
    
    list<-projectsMain$Primary.Key
    ind<-which(p$Primary.Key==list )
    
    
    newp<-c(a1,a2,p[1,3],p[1,4],p[1,5],p[1,6],
            p[1,7],p[1,8],p[1,9],p[1,10],p[1,11],p[1,13],s) 
    
    pknew<-as.character("")
    
    
    
    
    
    for( i in 1:(length(newp)-2)){
      
      
      pknew<-paste0(pknew,newp[i],sep = "_")
      
    } 
    
    pknew<-paste0(pknew,newp[13],"_",newp[12])
    
  
    newp<-c(newp[c(1:11)],pknew,newp[c(12,13)])
    
    oldpk<-p[1,12]
    ##
    projectsMain[ind,]<-newp
    
    
    ##update tasks
    h<-tasksMain
    

    
   
  
    h[,ncol(h)+1]<-substr(h$Primary.Key,1,nchar(h$Primary.Key)-2)
    g<-filter(h, (h$V10 %in% oldpk))
    h <- filter(h, !(V10 %in% oldpk))
    
    
    datatask<-g %>% separate(Primary.Key, 
                             c("Team", "Sub Team","Project Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber"),
                             extra='drop',fill = "right",sep ='_')
    
    
    
    da2<-datatask %>% separate(Tasknumber, c("Ser","Projectnum"),
                          extra='drop',sep ='T')
    
    
    
    
    
    
  
   
    
   g$newpk<- paste0( pknew,"T",da2$Projectnum )
    g<-g[,-c(9,10)]
    
    
    h<-h[,-10]
    
    colnames(g)<-colnames(h)
    
    h<-rbind(h,g)
    
    h<-h[!is.na(h$Project.Name),]
    
    tasksMain<-h
    
    
    
    #####resouce
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    da<-data %>% separate(Task, c("Ser","Project Name"),
                          extra='drop',sep ='T') #%>% data
    indexx<-c()
    
    for(k in 1:nrow(data))
    {
      pkneww<-""
      for(j in 1:12 ){
        
        pkneww<-paste0(pkneww,data[k,j],"_")
        
        
      }
      pkneww<-paste0(pkneww,da$Ser[k])
     
      if(pkneww==oldpk){
        indexx<-c(indexx,k)
        
      }
      
      
    }
    
   
    
    
    
    
    for(i in indexx){
      
      
      newpk<-paste0(pknew,"T",da$`Project Name`[i],"_",data$`Employee Name`[i],"_",data$Category[i],"_",data$Month[i])
      taskresource[i,1]<-newpk
      
      
    }
    
    
    
    
   
    
    write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    write.table(x = projectsMain,file = "./data/Projects.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    write.table(x = tasksMain,file = "./data/Tasks.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
    
    
    session$reload()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
   
  
  
  output$Utilization_by_proj_class_GCFall <- renderPlot({
    
    
    
    
       
    taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
    
    
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
   
    data<-data[!is.na(data$`Project_Name`),]
    tempdat<-data[data$Project_Name=="Adhoc",]
    data<-data[!(data$Project_Name=="Adhoc"),]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    
    ##
    data<-data[data$Category=="Actuals",]
    #data<-data[data$Team==input$mainTeamreport2,]
    m1<-input$startreport2
    m2<-input$endreport2
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      
      data<-data[data$Month %in% months,]
      
      
    }
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        
        
      }
      
      data<-data[data$Month %in% months,]
      
    }
    
    
    else{
      
      
      months<-unique( data$Month )
      
      
      
      
    }
     
    ######Leaves
 

 leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
 
 leavelog<-leaves %>% separate(Primary.Key, 
                               c("Team", "Sub Team","Month","Employee","Category"),
                               extra='drop',fill = "right",sep ='_') 
 
 
 leavelog<-leavelog[ leavelog$Month%in%months & leavelog$Category=="Actuals",]
 
 leave1<-sqldf("Select  Team, sum(Leaves) as PL
               from leavelog
               group by 1 ", stringsAsFactors = FALSE)
 
    leave1$"Project_Name"<-rep("Leaves",nrow(leave1))
    leave1<-leave1[,c(3,1,2)]
     
    summary_data<-sqldf("Select Classification, `Team`, sum(Planned_Days) as PL
                        from data
                        group by 1,2 ", stringsAsFactors = FALSE)
    
    summary_data<-summary_data[!is.na(summary_data$`PL`),]
    
    colnames(leave1)<-colnames(summary_data)
    
    summary_data<-rbind(leave1,summary_data)
    teamall<-sqldf("Select Classification,  sum(PL) as PL2
                   from summary_data
                   group by 1 ", stringsAsFactors = FALSE)
    
    
    teamlist<-rep("All Teams",nrow(teamall))
    
    teamall<-cbind(teamall$Classification,teamlist,teamall$PL2)
    teamall<-as.data.frame(teamall)
    colnames(teamall)<-colnames(summary_data)
    summary_data<-rbind(summary_data,teamall)
    summary_data$PL<-as.numeric(summary_data$PL)
     
    
    summary_data<-group_by(summary_data, `Team`) %>% mutate(percent_value = PL/sum(PL))
    
     
    summary_data<- summary_data[!is.na(summary_data$Classification),]
    
    
    summary_data$percent_value[is.na(summary_data$percent_value)]<-0
   #
    if(nrow(summary_data)!=0){
      
      ggplot()+
        geom_bar(data = summary_data, aes(x = `Team`, y = percent_value, fill = Classification), stat="identity",position='stack') + 
        geom_text(data = summary_data, aes(x = `Team`, y = percent_value,fill = Classification,
                                           label = paste0(sprintf("%1.0f", percent_value*100),"%")), color = "white", size=5, face="bold", position = position_stack(vjust = 0.5)) +
        #coord_flip() +
        #facet_grid( ~ Team) +
        ggtitle("Utilization by Project Classification") +
        theme(legend.position="top", 
              plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5, vjust=0.5),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(face = "bold", size = 14),
              axis.text.y = element_blank(),
              legend.text = element_text(color="black", size=12, face="bold",vjust=0.5),
              #panel.background = element_rect(fill = "#ecf0f5", color="#ecf0f5"),
              plot.background=element_rect(fill = "#ecf0f5"),
              legend.background = element_rect(fill = "#ecf0f5"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.5,"cm"))
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$Utilization_by_task_typeGCF_GCFall <- renderPlot({
    
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    data<-data[!is.na(data$`Project_Name`),]
    
    
    tempdat<-data[data$Project_Name=="Adhoc",]
    data<-data[!(data$Project_Name=="Adhoc"),]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    
  
    
    t<-tasksMain
    
    datatask<-t %>% separate(Primary.Key, 
                             c("Team", "Sub Team","Project_Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber"),
                             extra='drop',fill = "right",sep ='_')
    
    datatask<-datatask[!is.na(datatask$Project_Name),]
    
    datuse<-as.data.frame(cbind( datatask$Team, datatask$Subteam ,datatask$`Project_Name`,datatask$Task.category,datatask$Tasknumber))
    datuse[] <- lapply(datuse, as.character)
    g<- left_join(data, datuse, by = c("Team" = "V1" ,"Sub Team" = "V2", "Project_Name" = "V3", "Task" = "V5" ))
    
    data<-g
    
    
    data<-data[data$Category=="Actuals",]
    
    
    # months<-month_list_scope
    
    
    m1<-input$startreport2
    m2<-input$endreport2
    
    
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      data<-data[data$Month %in% months,]
      
    }
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        
        
      }
      
      data<-data[data$Month %in% months,]
      
    }
    
    
    else{
      
      
      months<-unique( data$Month )
      
      
      
      
    }
 
    
    ######Leaves

    
    leaves<-read.csv("./data/leaves.csv", header = TRUE, stringsAsFactors = FALSE)
    
    leavelog<-leaves %>% separate(Primary.Key, 
                                  c("Team", "Sub Team","Month","Employee","Category"),
                                  extra='drop',fill = "right",sep ='_') 
    
    
    leavelog<-leavelog[leavelog$Month%in%months & leavelog$Category=="Actuals",]
    
    leave1<-sqldf("Select  Team, sum(Leaves) as PL
                  from leavelog
                  group by 1 ", stringsAsFactors = FALSE)
    
    
    leave1$"Project_Name"<-rep("Leaves",nrow(leave1))
    leave1<-leave1[,c(3,1,2)]
    
    
    summary_data<-sqldf("Select V4, `Team`, sum(Planned_Days) as PL
                        from data
                        group by 1,2 ", stringsAsFactors = FALSE)
    
    colnames(leave1)<-colnames(summary_data)
    
    summary_data<-rbind(leave1,summary_data)
    
    teamall<-sqldf("Select V4,  sum(PL) as PL2
                   from summary_data
                   group by 1 ", stringsAsFactors = FALSE)
    
    teamlist<-as.data.frame(rep("All Teams",nrow(teamall)))
    
    teamall<-cbind(teamall$V4,teamlist,teamall$PL2)
    
    teamall<-as.data.frame(teamall)
    colnames(teamall)<-colnames(summary_data)
    summary_data<-rbind(summary_data,teamall)
    summary_data$PL<-as.numeric(summary_data$PL)
    
    
    
    
    
    
    
    
    
    
    
    
    summary_data<-group_by(summary_data, `Team`) %>% mutate(percent_value = PL/sum(PL))
    
    
    
    summary_data<- summary_data[!is.na(summary_data$V4),]
    
    
    
    summary_data$percent_value[is.na(summary_data$percent_value)]<-0
    
    
    
    ## 
    if(nrow(summary_data)!=0){
      
      ggplot()+
        geom_bar(data = summary_data, aes(x = `Team`, y = percent_value, fill = V4), stat="identity",position='stack') + 
        geom_text(data = summary_data, aes(x = `Team`, y = percent_value,fill = V4,
                                           label = paste0(sprintf("%1.0f", percent_value*100),"%")), color = "white", size=5, face="bold", position = position_stack(vjust = 0.5)) +
        #coord_flip() +
        #facet_grid( ~ Team) +
        ggtitle("Utilization by Task Type") +
        theme(legend.position="top", 
              plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5, vjust=0.5),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(face = "bold", size = 14),
              axis.text.y = element_blank(),
              legend.text = element_text(color="black", size=12, face="bold",vjust=0.5),
              #panel.background = element_rect(fill = "#ecf0f5", color="#ecf0f5"),
              plot.background=element_rect(fill = "#ecf0f5"),
              legend.background = element_rect(fill = "#ecf0f5"),
              panel.border = element_rect(colour = "black", fill=NA, size=1),
              plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
              legend.title = element_blank(),
              legend.spacing.x = unit(0.5,"cm"))
    }
    
    
  })
  
  
  output$UTPCGCF_GCFall<-DT :: renderDataTable({
    
    
    
    
    
    m1<-input$startreport2
    m2<-input$endreport2
    
    cbind.fill <- function(...){
      nm <- list(...)
      nm <- lapply(nm, as.matrix)
      n <- max(sapply(nm, nrow))
      do.call(cbind, lapply(nm, function (x)
        rbind(x, matrix(, n-nrow(x), ncol(x)))))
    }
    
    
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    
    
    data<-data[data$Category=="Actuals",]
    
    #data<-data[data$Team==input$mainTeamreport2,]
    
    
    
    
    summary_data<-sqldf("select   `Project Name`,Classification
                        from data 
                        where `Project Name`!= 'Adhoc'
                        ", stringsAsFactors=FALSE)
    
 
    
    
    a<-unique(summary_data$Classification)
    
    final<-data.frame()
    if(nrow(summary_data)!=0){
      for(i in 1:length(a)){
        
        e<-c()
        for(j in 1:nrow(summary_data)){
          
          
          if(summary_data$Classification[j]==a[i]){
            
            e<-c(e,summary_data$`Project Name`[j])
          }
          
          
        }
        
        e<-unique(e)
        e<-data.frame(e)
        final<-as.data.frame(cbind.fill(final,e))
        
      }
      
      
      
      
      out <- final
      
      datatable(out,options = list(autoWidth=TRUE),colnames = a)}
    
    
  })
  
  
  output$UTPCGCFtask_GCFall<-DT :: renderDataTable({
    
    
    data<-taskresource %>% separate(Primary.Key, 
                                    c("Team", "Sub Team","Project_Name","Classification","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Task","Employee Name","Category","Month"),
                                    extra='drop',fill = "right",sep ='_') #%>% data
    
    
    data<-data[!is.na(data$`Project_Name`),]
    
    
    tempdat<-data[data$Project_Name=="Adhoc",]
    data<-data[!(data$Project_Name=="Adhoc"),]
    tempdat<-tempdat[,c(1:13,15,16,14,17)]
    
    colnames(tempdat)<-colnames(data)
    data<-rbind(data,tempdat)
    
    
    
    t<-tasksMain
    
    datatask<-t %>% separate(Primary.Key, 
                             c("Team", "Sub Team","Project_Name","TaskType","Target Audience","Geography","Functional Group","Stakeholders","TA","Brand","Project End Month","Status","Tasknumber"),
                             extra='drop',fill = "right",sep ='_')
    
    datatask<-datatask[!is.na(datatask$Project_Name),]
    
    datuse<-as.data.frame(cbind( datatask$Team, datatask$Subteam ,datatask$`Project_Name`,datatask$Task.category,datatask$Tasknumber,datatask$Task.details))
    
    datuse[] <- lapply(datuse, as.character)
    
    g<- left_join(data, datuse, by = c("Team" = "V1" ,"Sub Team" = "V2", "Project_Name" = "V3", "Task" = "V5" ))
    
    data<-g
    
    
    
    
    
    # months<-month_list_scope
    
    
    m1<-input$startreport2
    m2<-input$endreport2
    
    
    
    if(!m2=="" && !m1=="" ){
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      months<-month_list_scope[n1:n2]
      data<-data[data$Month %in% months,]
      
    }
    
    
    else if((m2=="" & m1!="") | (m1=="" & m2!="") ){
      
      
      
      n1<-which(m1 == month_list_scope)  
      n2<-which(m2 == month_list_scope)
      
      
      if(m2==""){
        
        months<-month_list_scope[ n1:length(month_list_scope) ]
        
        
      }
      else {
        
        
        months<-month_list_scope[ 1: n2]
        
        
        
      }
      
      data<-data[data$Month %in% months,]
      
    }
    
    
    

    
   
    
    #data<-data[data$Team==input$mainTeamreport2,]
    
    
    
    
    summary_data<-sqldf("select   `V6`,V4
                        from data 
                        
                        ", stringsAsFactors=FALSE)
    
    #  # #
    summary_data<-summary_data[!is.na(summary_data$V6),]
    summary_data<-summary_data[!is.na(summary_data$V4),]
    
    
    a<-unique(summary_data$V4)
    
    final<-data.frame()
    if(nrow(summary_data)!=0){
      for(i in 1:length(a)){
        
        e<-c()
        for(j in 1:nrow(summary_data)){
          
          
          if(summary_data$V4[j]==a[i]){
            
            e<-c(e,summary_data$`V6`[j])
          }
          
          
        }
        
        e<-unique(e)
        e<-data.frame(e)
        final<-as.data.frame(cbind.fill(final,e))
        
      }
      
      
      
      
      out <- final
      
      datatable(out,rownames= FALSE ,selection = 'multiple', options = list(autoWidth=TRUE),colnames = a)}
    
    
  })
  
  
  observeEvent(list(input$abc), {
  
  
    geography_list<-unique(dropdowns$Geography)
    
    brand <- unique(dropdowns$Brand)
    TA <- unique(dropdowns$TA)
    
    stakeholdernameList<- unique(dropdowns$Stakeholders)
    
    
    
    updateSelectInput(session,paste0("geography_scope"), label="Geography",selected=NULL,choices = c("",geography_list))
    # updateSelectInput(paste0("stakeholdernames_scope"),label="Stakeholders"))))
    updateSelectInput(session,paste0("TA_scope"),label="TA", selected=NULL,choices=c("",TA))
    updateSelectInput(session,paste0("Brand_scope"),label="Brand", selected=NULL,choices =c("",brand))
    
    updateSelectInput(session,paste0("geography_modifying2"), label="Geography",selected=NULL,choices = c("",geography_list))
    # updateSelectInput(paste0("stakeholdernames_scope"),label="Stakeholders"))))
    updateSelectInput(session,paste0("TA_modifying2"),label="TA", selected=NULL,choices=c("",TA))
    updateSelectInput(session,paste0("Brand_modifying2"),label="Brand", selected=NULL,choices =c("",brand))
  
  
  
  })
  
  observeEvent(list(input$xyz), {
    
    
    geography_list<-unique(dropdowns$Geography)
    
    brand <- unique(dropdowns$Brand)
    TA <- unique(dropdowns$TA)
    
    stakeholdernameList<- unique(dropdowns$Stakeholders)
    
    updateSelectInput(session,paste0("geography_scope"), label="Geography",selected=NULL,choices = c("",geography_list))
    # updateSelectInput(paste0("stakeholdernames_scope"),label="Stakeholders"))))
    updateSelectInput(session,paste0("TA_scope"),label="TA", selected=NULL,choices=c("",TA))
    updateSelectInput(session,paste0("Brand_scope"),label="Brand", selected=NULL,choices =c("",brand))
    
   
    
    
    updateSelectInput(session,paste0("geography_modifying2"), label="Geography",selected=NULL,choices = c("",geography_list))
    # updateSelectInput(paste0("stakeholdernames_scope"),label="Stakeholders"))))
    updateSelectInput(session,paste0("TA_modifying2"),label="TA", selected=NULL,choices=c("",TA))
    updateSelectInput(session,paste0("Brand_modifying2"),label="Brand", selected=NULL,choices =c("",brand))
    
    
    
  })
  

   output$boxesfte <- renderUI({
  
  
     box(id="boxfte",
         title = fte, width = 1, background = "light-blue",
         "FTE Count:"
     )
     
   })
     output$boxesfte2 <- renderUI({
       
       
       box(id="boxfte2",
           title = fteplanned, width = 1, background = "light-blue",
           "Planned FTE:"
       )
       
     })
       output$boxesfte3 <- renderUI({
         
         
         box(id="boxfte3",
             title = fteactual, width = 1, background = "light-blue",
             "Actual FTE:"
         )
  
  
  
})
  
  

       # observeEvent(RVprojectnameid$data2,{
       # 
       # 
       #   for(ii in 1:length(RVprojectnameid$data2)){
       #     local({
       #       i <- ii
       #       observeEvent(RVprojectnameid$data2[i],{
       # 
       #         
       # 
       # 
       # 
       # 
       # 
       #         })
       #     })
       #   }
       # 
       # 
       # })

  
       observeEvent(input$startreport,{
        
         
         m<-input$startreport
         if(m!=""){
         n<-as.numeric(which(m==month_list_scope))
         
         mlist<-month_list_scope[n:length(month_list_scope)]
        
         updateSelectInput(session,"endreport", label = "End Month",choices=c("",mlist),selected = NULL)
       }
       
       })
       
      
       
       observeEvent(input$startreport2,{
         
         
         m<-input$startreport2
         if(m!=""){
           n<-as.numeric(which(m==month_list_scope))
           
           mlist<-month_list_scope[n:length(month_list_scope)]
           
           updateSelectInput(session,"endreport2", label = "End Month",choices=c("",mlist),selected = NULL)
         }
         
       })
       
       
       observeEvent(input$startreport3,{
         
         
         m<-input$startreport3
         if(m!=""){
           n<-as.numeric(which(m==month_list_scope))
           
           mlist<-month_list_scope[n:length(month_list_scope)]
           
           updateSelectInput(session,"endreport3", label = "End Month",choices=c("",mlist),selected = NULL)
         }
         
       })
       
       
       observeEvent(list(input$Save_back4), {
         
         
         if(input$Save_back4>0){ 
           if(!is.null(input$backempdel)){
             
            
             
             team_information <- read.csv("./data/Employee_list.csv", header = TRUE, stringsAsFactors = FALSE)
             a<-input$backsubteam1
             b<-input$backteam
             c<-input$backempdel
             
            
             team_information<-team_information[!(team_information$Team==b & team_information$Sub_Team==a & team_information$Employee==c),]
             write.table(x = team_information,file = "./data/Employee_list.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
             session$reload()
             
             
           }
           else{
             
             showNotification("Please enter all fields correctly",duration = 10, closeButton = TRUE,type ="error")
             
           }
         }
         
       })
       
       observeEvent(input$Delltaskrsc, { 
         
         
         
         
         
         e<-input$temp2Rsc_rows_selected-1
         pkdel<-globalpk[e]
         globalpk<<-globalpk[-e]
         
         RVrsctemp2edited$data<-RVrsctemp2edited$data[-(e+1),]
         
         RVrsctemp2$data<-RVrsctemp2$data[-(e+1),]
         
         t<-RVrsctemp2edited$data
         m<-input$rscWDmonth2
         
         g<-length(pkdel)+1
         
         tasksMain <-read.csv("./data/Tasks.csv",header = TRUE, stringsAsFactors = FALSE)
         
         projectsMain <-read.csv("./data/Projects.csv",header = TRUE, stringsAsFactors = FALSE)
         taskresource<-read.csv("./data/taskemployee.csv", header = TRUE, stringsAsFactors = FALSE)
        
         adpk<-globaladhoc
         col<-colnames(t)
         
         
         if(!is.null(pkdel)){
           for( i in 2:g){
            
             for(j in 1:(ncol(t)-1)){
               check<-0
               tempk<-paste(pkdel[i-1],col[j+1],m,sep="_")
              
               for(k in 1:nrow(taskresource)){
                 
                 if(taskresource[k,1] == tempk){
                   taskresource <- taskresource[-k,]
                   break
                 }
                 
                 
               }
               
              
               
               
               
             }
           }}
        
         write.table(x = taskresource,file = "./data/taskemployee.csv", append=FALSE, row.names = FALSE, col.names = TRUE, sep=',')
         
       })
       
       
       
    } 
