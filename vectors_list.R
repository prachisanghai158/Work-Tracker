
#for scoping
chckbox_s               <-c()
projectname_s           <- c()
projectclassification_s <- c()
targetaudience_s        <- c()
geography_s             <- c()
stakeholdergroup_s      <- c()
stakeholdername_s       <- c()
planningcycle_s         <- c()
TA_s                    <- c()
brand_s                 <- c()
divList_s               <- c()

#for planning
checkbox_p      <-c()
projectname_p   <-c()
empname_p       <-c()
taskName_p      <-c()
tasktype_p      <-c()
taskpriority_p  <-c()
taskDateRange_p <-c()
taskHRS_p       <-c()
divList_p       <-c()

#FOR ACTUALS LOGGING
projectname_l   <-c()
taskName_l      <-c()
tasktype_l      <-c()
taskpriority_l  <-c()
taskHRS_l       <-c()
divList_l       <-c()

#FOR ACTUALS TRACKING
projectname_t   <-c()
empname_t       <-c()
taskName_t      <-c()
tasktype_t      <-c()
taskpriority_t  <-c()
taskDateRange_t <-c()
taskHRS_t       <-c()
task_status_t   <-c()
divList_t       <-c()


#for planning - current month data
c_divList_p       <-c()
#for scoping - previous month and current month
c_divList_s <-c()
p_divList_s <-c()

nrow_prev_data<-c(0)

#DEFINE VECTORS TO HOLD TRACKING DATA FROM INPUT FIELDS
projectName_t      <- c()
TeamMember_t       <- c()
taskNameList_t     <- c()
taskTypeList_t     <- c()
taskPriorityList_t <- c()
taskStartTime_t    <- c()
taskEndTime_t      <- c()
hourEntered_t      <- c()
taskStatus_A       <- c()


#DEFINE VECTORS TO HOLD SCOPING DATA FROM INPUT FIELDS
projectNameInputData  <- c()
projectClassificationData<-c()
targetAudienceData<-c()
geographyData<-c()
stakeholderGroupData<-c()
stakeholderNameData<-c()
planningCycleData<-c()
TAData<-c()
brandData<-c()

#DEFINE VECTORS TO HOLD SCOPING DATA FROM INPUT FIELDS PULLED FOR CURRENT MONTH
c_checkbox_scoping<-c()
c_projectNameInputData  <- c()
c_projectClassificationData<-c()
c_targetAudienceData<-c()
c_geographyData<-c()
c_stakeholderGroupData<-c()
c_stakeholderNameData<-c()
c_planningCycleData<-c()
c_TAData<-c()
c_brandData<-c()

#DEFINE VECTORS TO HOLD SCOPING DATA FROM INPUT FIELDS PULLED FOR PREVIOUS MONTH
p_projectNameInputData  <- c()
p_projectClassificationData<-c()
p_targetAudienceData<-c()
p_geographyData<-c()
p_stakeholderGroupData<-c()
p_stakeholderNameData<-c()
p_planningCycleData<-c()
p_TAData<-c()
p_brandData<-c()

#DEFINE VECTORS TO HOLD PLANNING DATA FROM USER ENTRY FIELDS
projectNameInputData_P<- c()
taskNameInputData     <- c()
taskTypeInputData     <- c()
taskPriorityInputData <- c()
planningMemberData    <- c()
taskTotalHoursData    <- c()
taskStartTimeInputData<- c()
taskEndTimeInputData  <- c()

#DEFINE VECTORS TO HOLD PLANNING DATA FOR CURRENT MONTH
c_projectNameInputData_P<- c()
c_taskNameInputData     <- c()
c_taskTypeInputData     <- c()
c_taskPriorityInputData <- c()
c_planningMemberData    <- c()
c_taskTotalHoursData    <- c()
c_taskStartTimeInputData<- c()
c_taskEndTimeInputData  <- c()

#DEFINE VECTORS TO HOLD LOGGING DATA
projectNameInputData_l  <- c()
taskNameInputData_l     <- c()
taskTypeInputData_l     <- c()
taskPriorityInputData_l <- c()
taskActualHoursData_l   <- c()
taskPlannedHoursData_l  <- c()

#DEFINE VECTOR TO HOLD ACTUAL HOURS FROM DAILY LOGGING DATATABLE
taskActualsHoursData_l_DT <-c()


taskActualsHoursData_t_DT <- c()
taskStatus_t_DT           <- c()

#DEFINE VECTORS TO HOLD DAILY LOGGING HOURS INPUT NAMES
invec_Hours_l<-c()

#TRACKING
invec_Hours_t<-c()
invec_Status_t<-c()
