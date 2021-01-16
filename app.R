library(shiny)
library(RMySQL)
library(pool)
library(tidyverse)
library(shinyjs)
library(glue)
library(lubridate)
library(shinydashboard)
library(httr)
library(jsonlite)
library(DT)
library(shinyWidgets)


# Define the fields we want to save from the form
options(mysql = list(
    "user" = Sys.getenv('MUFASA_USER'),
    "password" = Sys.getenv('MUFASA_PASSWORD'),
    "host" = "mufasa.cxcfcs0uwqfn.us-east-2.rds.amazonaws.com",
    "port" = 3306
))


glue_sql <- function(..., .con) {
    connection <- pool::poolCheckout(.con)
    on.exit(pool::poolReturn(connection))  
    glue::glue_sql(..., .con = connection, .envir = parent.frame())  
}

# databaseName <- 'staffing-data'
# table <- 'ROR_CandidateRoot'

fields <- c('AIdent','lastName','firstName', 'phoneNumber','cellNumber', 
            'branchId','srident')

contactFields <- c('contactAIdent','contactSrident', 'contactNotes')
interviewFields <- c('interviewAIdent',
                     'interviewSrident', 'interviewDateTime',
                     'interviewStatusId')


mandatoryPersonFields <- c('AIdent','srident')
mandatoryContactFields <- c('contactSrident')
mandatoryInterviewFields <- c('interviewSrident','interviewDateTime','interviewStatusId')




# Load all previous responses
# ---- This is one of the two functions we will change for every storage type ----
loadData <- function(databaseName,table) {
    # Connect to the database
    db <- dbPool(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                 port = options()$mysql$port, user = options()$mysql$user, 
                 password = options()$mysql$password)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    poolClose(db)
    data
}



BranchData <- loadData('staffing-data','BranchRoot')
branchList <- setNames(as.list(BranchData$branch_id), BranchData$branch_name)

ServiceRepData <- loadData('staffing-data','ServiceRepRoot')
srList <- setNames(as.list(ServiceRepData$srident), ServiceRepData$rep_name)

InterviewStatusData <- loadData('staffing-data','ROR_InterviewStatus')
intStatusList <- setNames(as.list(InterviewStatusData$interview_status_id),
                          InterviewStatusData$interview_status)
filled <- function(reqiredFields)
{
    mandatoryFilled <-
        vapply(mandatoryPersonFields,
               function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
    mandatoryFilled <- all(mandatoryFilled)  
    return(mandatoryFilled)
}

dataModalUpdate <- function(data = NULL,failed = FALSE,aident = NULL) {
    modalDialog(
        numericInput('AIdent', 'Employee Number',
                     value = ifelse(is.numeric(aident),
                                    aident,
                                    NA_integer_)),
        actionLink("candidateInfo","Get Candidate Info"),
        tags$hr(),
        disabled(textInput('lastName','Last Name')),
        disabled(textInput('firstName', 'First Name')),
        disabled(textInput('phoneNumber','Phone')),
        disabled(textInput('cellNumber', 'Cell')),
        disabled(selectInput("branchId","BranchName",
                    choices = c('',branchList),
                    selected = '')),
        selectInput("srident","Recruiter",
                             choices = c('',srList),
                             selected = ''),
        easyClose = TRUE,
        footer = tagList(
            actionButton("update","Update"),
            modalButton("Cancel")
        )
    )
}


contactModal <- function(aident = 0, srident = 0, name = NULL) {
    modalDialog(
        numericInput('contactAIdent', 'Employee Number',
                     value = ifelse(aident == 0,
                                    NA_integer_,
                                    aident)),
        disabled(textInput('contactEmpName','Employee Name', value = name)),
        # numericInput('contactSrident', 'Recruiter Number',
        #              value = ifelse(srident == 0,
        #                             NA_integer_,
        #                             srident)),
        selectInput("contactSrident","Recruiter",
                    choices = c('',srList),
                    selected = ''),
        textAreaInput("contactNotes", "Contact Notes",
                      value = '',
                      width = '150%',
                      height = '150%'),
        easyClose = TRUE,
        footer = tagList(
            actionButton("contactUpdate","Update"),
            modalButton("Cancel")
        )
    )
}

interviewModal <- function(aident = 0, srident = 0, name = NULL) {
    modalDialog(
        numericInput('interviewAIdent', 'Employee Number',
                     value = ifelse(aident == 0,
                                    NA_integer_,
                                    aident)),
        disabled(textInput('interviewEmpName','Employee Name', value = name)),
        selectInput('interviewSrident', 'Recruiter',
                    choices = c('',srList),
                    selected = ''),
        airDatepickerInput("interviewDateTime","Interview Time", timepicker = TRUE,
                           todayButton = TRUE,
                           timepickerOpts = timepickerOptions(minutesStep = 10),
                           autoClose = TRUE),
        selectInput('interviewStatusId', 'Interview Status', 
                    choices = c('',intStatusList),
                    selected = 1),
        easyClose = TRUE,
        footer = tagList(
            actionButton("interviewUpdate","Update"),
            modalButton("Cancel")
        )
    )
}

candidateClickModal <- function(contactdata = NULL, interviewdata = NULL){
    modalDialog(
        h2("What would you like to do?"),
        h3("Previous Contacts"),
        renderDataTable(datatable(contactdata,
                                  rownames = FALSE,
                                  options = list(dom = 'ftpri',
                                                 order = list(5,'desc'),
                                                 pageLength = 3)) %>% 
                            formatDate('created_date', method = 'toLocaleDateString')),
        h3("Previous Interviews"),
        renderDataTable(datatable(interviewdata,
                                  rownames = FALSE,
                                  options = list(dom = 'ftpri',
                                                 order = list(9,'desc'),
                                                 pageLength = 2))%>% 
                            formatDate('created_date', method = 'toLocaleDateString')),
        easyClose = TRUE,
        footer = tagList(
            actionButton("candidateNewContact","New Contact"),
            actionButton("candidateNewInterview","New Interview"),
            actionButton("updateCandidate","Update Candidate Info"),
            modalButton("Cancel"),
        ),
        size = "l"
    )
}

contactClickModal <- function(){
    modalDialog(
        h3("What would you like to do?"),
        easyClose = TRUE,
        footer = tagList(
            actionButton("viewContactInfo","View Contact Info"),
            actionButton("updateContact","Update Rep Info"),
            modalButton("Cancel"),
        )
    )
}


interviewClickModal <- function(){
    intStatusLis <- within(intStatusList,rm(PENDING))
    modalDialog(
        h3("How did the interview go?"),
        selectInput('interviewStatusId', 'Interview Status', 
                    choices = c('',intStatusLis),
                    selected = ''),
        easyClose = TRUE,
        footer = tagList(
            actionButton("updateInterviewStatus","Update"),
            modalButton("Cancel")
        )
    )
}


createCandidateData <- function(data) {
    # Connect to the database
    
    db <- dbPool(MySQL(), dbname = 'staffing-data', host = options()$mysql$host,
                 port = options()$mysql$port, user = options()$mysql$user,
                 password = options()$mysql$password)
    var <- c('aident','last_name', 'first_name', 'phone_number',
             'cell_number', 'branch_id','ror_srident')
    values <- data
    tableroot <- 'ROR_CandidateRoot'


    query <- glue_sql(
        "INSERT INTO {`tableroot`}
        ({`var`*})
        VALUES ({values*})", .con=db
    )
   print(query)

    dbGetQuery(db, query)
    poolClose(db)
}

createContactData <- function(data) {
    # Connect to the database
    db <- dbPool(MySQL(), dbname = 'staffing-data', host = options()$mysql$host,
                 port = options()$mysql$port, user = options()$mysql$user,
                 password = options()$mysql$password)
    var <- c('aident','ror_srident', 'notes')
    values <- data
    tableroot <- 'ROR_ContactRoot'
    query <- glue_sql(
        "INSERT INTO {`tableroot`}
        ({`var`*})
        VALUES ({values*})", .con=db
    )
    # print(query)
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    poolClose(db)
}

createInterviewData <- function(data) {
    # Connect to the database
    db <- dbPool(MySQL(), dbname = 'staffing-data', host = options()$mysql$host,
                 port = options()$mysql$port, user = options()$mysql$user,
                 password = options()$mysql$password)
    interviewtime <- strftime(as_datetime(as.numeric(data[['interviewDateTime']])),
                    format="%Y-%m-%d %H:%M:%OS", tz = "GMT")
    data[['interviewDateTime']] <- interviewtime
    
    var <- c('aident','ror_srident', 'interview_datetime', 
             'interview_status_id')
    values <- data
    tableroot <- 'ROR_InterviewRoot'
    query <- glue_sql(
        "INSERT INTO {`tableroot`}
        ({`var`*})
        VALUES ({values*})", .con=db
    )
    
    # Submit the update query and disconnect
    #print(query)
    dbGetQuery(db, query)
    poolClose(db)
    
    date <- str_extract(values[['interviewDateTime']], "\\d+-\\d+-\\d+")
    time <- str_extract(values[['interviewDateTime']], "\\d+:\\d+")
    day <- wday(ymd(date), label = TRUE, abbr = TRUE)
    time <- format(strptime(time,format = '%H:%M'),'%I:%M %p')
    scheduledInterviewInfo <-  paste(day, date,"@",time)
    aident <- values[['interviewAIdent']]
    sr <- ServiceRepData %>% filter(srident == values[['interviewSrident']]) %>% 
        select(rep_name)
    token <-  Sys.getenv("ROR_TW_TOKEN")
    bodyData <- paste0('{ "actionId": 462, "message":"',scheduledInterviewInfo,
                       ' Scheduled by ', sr,
                       '", "linkedEntities": [ {"originTypeId":1, "originId": ',
                       aident,'} ]}')
    
    headers <-  c(
        `accept` = 'text/plain',
        `x-tw-token` = token,
        `Content-Type` = 'application/json'
    )

    httr::POST(url = paste0('https://api.ontempworks.com/Employees/',aident,
                            '/messages'), httr::add_headers("x-tw-token"= token,
                                                            "Content-Type" = 'application/json'),
                            body = bodyData)

    # print(bodyData)
    
}


updateCandidateData <- function(data) {
    # Connect to the database
    db <- dbPool(MySQL(), dbname = 'staffing-data', host = options()$mysql$host,
                 port = options()$mysql$port, user = options()$mysql$user,
                 password = options()$mysql$password)
    var <- c('aident','last_name', 'first_name', 'phone_number',
             'cell_number', 'branch_id','ror_srident')
    values <- data
    tableroot <- 'ROR_CandidateRoot'
    
    #print(glue("SET {var*} = {data*},",.na = '',.sep = ','))
    aident <-  as.numeric(data[1])
    
    query <- glue_sql(
        "UPDATE {`tableroot`}
        SET {`var`} = {values}
        WHERE aident = {aident};", .con=db
    ) %>% map(~ dbExecute(db,.))
    
    #dbGetQuery(db, query)
    poolClose(db)
}

updateInterviewData <- function(InterviewID, InterviewStatus){
    # Connect to the database
    db <- dbPool(MySQL(), dbname = 'staffing-data', host = options()$mysql$host,
                 port = options()$mysql$port, user = options()$mysql$user,
                 password = options()$mysql$password)
    var <- c('aident','emp_name','ror_srident', 'interview_datetime', 
             'interview_status_id')
    values <- data
    tableroot <- 'ROR_InterviewRoot'
    query <- glue_sql(
        "UPDATE {`tableroot`}
        SET interview_status_id = {InterviewStatus}
        WHERE interview_id = {InterviewID};", .con=db
    )#%>% map(~ dbExecute(db,.))
    
   # print(query)
    dbGetQuery(db, query)
    
    poolClose(db)
}

# updateTwEmployeeMessage <- function(DateTime,)


################################################################################################3


ui <- dashboardPage(
    dashboardHeader(title = 'ROR'),
    dashboardSidebar(
        sidebarMenu(id ="sidebarmenu",
            menuItem(
                "Main Dashboard",tabName = "Main", icon = icon("dashboard")
            ),
            menuItem(
                "Candidates",
                tabName = 'Candidates',
                icon = icon("users"),
                menuSubItem(
                    'Candidate',
                    tabName = 'Person',
                    icon = icon("user")
                ),
                menuSubItem(
                    'Contact',
                    tabName = 'Contact',
                    icon = icon("phone-alt")
                ),
                menuSubItem(
                    'Interviews',
                    tabName = 'Interviews',
                    icon = icon("calendar")
                )
            )
        )
        
    ),
    dashboardBody(
        HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
        
        tags$script('
        $(function() {
        var time_now = new Date()
        $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
        });
        '),
        
        useShinyjs(),
        tabItems(
            tabItem(
                'Main',
                #DT::dataTableOutput("responses")
                valueBoxOutput("candidatesBox"),
                valueBoxOutput("contactsBox"),
                valueBoxOutput("interviewsBox")
            ),
            tabItem(
                'Person',
                actionButton('NewPerson','New Person'),
                #verbatimTextOutput("test"),
                DT::dataTableOutput("personTable")
                # tableOutput("text")
            ),
            tabItem(
                'Contact',
                #actionButton('NewContact','New Contact'),
                DT::dataTableOutput("contactTable"),
                
                verbatimTextOutput("time_zone_offset")
                
            ),
            tabItem(
                'Interviews',
                #actionButton('NewInterview','New Interview'),
                DT::dataTableOutput("interviewTable"),
                # verbatimTextOutput("test"),
                tags$style(".datepicker { z-index: 9999 !important; }"),
                tags$style(
                    type = 'text/css',
                    '.modal-dialog{ width: fit-content !important; }'
                )
            )
        )
    )
)


server <- function(input, output, session) {
    
    observe({
        # check if all mandatory fields have a value
        mandatoryFilled <-
            vapply(mandatoryPersonFields,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        # enable/disable the submit button
        shinyjs::toggleState(id = "update", condition = mandatoryFilled)
        
        mandatoryContactFilled <-
            vapply(mandatoryContactFields,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryContactFilled <- all(mandatoryContactFilled)
        
        shinyjs::toggleState(id = "contactUpdate", condition = mandatoryContactFilled)
        
        mandatoryInterviewFilled <-
            vapply(mandatoryInterviewFields,
                   function(x) {
                       !is.null(input[[x]]) && input[['interviewSrident']] != ""
                   },
                   logical(1))
        mandatoryInterviewFilled <- all(mandatoryInterviewFilled)
        
        shinyjs::toggleState(id = "interviewUpdate", condition = mandatoryInterviewFilled)
    })
    
    formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        data
    })
    
    selectedAident <- reactiveVal(0)
    selectedRORsrident <- reactiveVal(0)
    selectedname <- reactiveValues(firstName = NULL, LastName = NULL)
    selectedInterviewID <- reactiveVal(0)
    time_zone_offset <- reactive({as.numeric(input$client_time_zone_offset)/60})

    # input$testTime <- reactive({strftime(input$interviewTime)})
    # output$test <- renderUI({strftime(input[['interviewTime']],'%T')})
    output$test <- renderPrint({interviewFormData()})
    
    contactFormData <- reactive({
        data <- sapply(contactFields, function(x) input[[x]])
        data
    })
    interviewFormData <- reactive({
        data <- sapply(interviewFields, function(x) input[[x]])
         #names(data)
        inputtime <- data["interviewDateTime"]
        data["interviewDateTime"] <- as_datetime(as.numeric(data["interviewDateTime"])) - hours(time_zone_offset())     
        # data["interviewDateTime"] <- strftime(as.numeric(as.character(data["interviewDateTime"])),
        #                                         origin = '1970-01-01',)
            
        # data['interviewDateTime'] <- strftime(.,format = "%Y-%m-%d %H:%M:%S")
         data
        # strftime(data$,'%T')
    })
    
    
    

    # Show the previous responses
    # (update with current response when Submit is clicked)
    
    CandidateTable <- reactive({
        input$submit
        input$delete
        input$update
        loadData('staffing-data','ROR_CandidateRoot') %>% 
            mutate(branch_id = as.character(branch_id)) %>% 
            left_join(BranchData,by = c("branch_id")) %>% 
            left_join(ServiceRepData, by = c("ror_srident" = "srident")) %>% 
            select(aident, last_name, first_name,
                   phone_number, cell_number, region,area,metro,
                   branch_name,rep_name,created_date)
    })
    
    ContactTable <- reactive({
        input$submit
        input$delete
        input$contactUpdate
        
        loadData('staffing-data','ROR_ContactRoot') %>% 
            mutate(created_date = as_datetime(created_date, tz = 'UTC')) %>% 
            left_join(ServiceRepData, by = c("ror_srident" = "srident")) %>%
            inner_join(CandidateTable(), by = c("aident")) %>% 
            mutate(emp_name = paste(last_name,first_name,sep = ', ')) %>% 
            select(contact_id,aident, emp_name,rep_name = rep_name.x,
                   notes,created_date = created_date.x)
    })
    
    InterviewTable <- reactive({
        input$submit
        input$delete
        input$interviewUpdate
        input$updateInterviewStatus
        
        loadData('staffing-data','ROR_InterviewRoot') %>% 
            mutate(created_date = as_datetime(created_date, tz = 'UTC'),
                   interview_status_id = as.integer(interview_status_id),
                   aident = as.integer(aident)) %>% 
            left_join(ServiceRepData, by = c("ror_srident" = "srident")) %>% 
            left_join(InterviewStatusData, by = c("interview_status_id")) %>% 
            inner_join(CandidateTable(),by = c("aident")) %>%
            mutate(emp_name = paste(last_name,first_name,sep = ', ')) %>% 
            select(interview_id,aident,emp_name,phone_number,cell_number,
                   rep_name = rep_name.x,
                   branch_name,
                   interview_datetime,
                   interview_status,
                   created_date = created_date.x)
    })
    
    output$personTable <- DT::renderDataTable({
        # input$submit
        # input$delete
        # input$update
        data <- CandidateTable() %>% 
            mutate(created_date = as_datetime(created_date, tz = 'UTC'))
        DT::datatable({data},
                      selection = list(mode = "single"),
                      filter = list(position = 'top', clear = FALSE),
                      rownames = FALSE,
                      options = list(dom = 'ftpri',
                                     order = list(10,'desc'),
                                     pageLength = 100)) %>% 
            formatDate('created_date', method = 'toLocaleDateString')
    })
    
    output$contactTable <- DT::renderDataTable({
        DT::datatable({ContactTable()},
                      selection = list(mode = "single"),
                      filter = list(position = 'top', clear = FALSE),
                      rownames = FALSE,
                      options = list(dom = 'ftpri',
                                     order = list(5,'desc'),
                                     pageLength = 100)) %>% 
            formatDate('created_date', method = 'toLocaleDateString')
    })
    

    output$interviewTable <-  DT::renderDataTable({
        DT::datatable({InterviewTable()},
                      selection = list(mode = "single"),
                      filter = list(position = 'top', clear = FALSE),
                      rownames = FALSE,
                      options = list(dom = 'ftpri',
                                     order = list(9,'desc'),
                                     pageLength = 100)) %>% 
            formatDate('created_date', method = 'toLocaleDateString') %>% 
            formatDate('interview_datetime', method = 'toLocaleString')
    })
    
    
    observeEvent(input$NewPerson,priority = 1,{
        showModal(dataModalUpdate())
        reset("form")
    })
    
    observeEvent(input$candidateInfo,{
        url <- "https://api.ontempworks.com"
        base <-  Sys.getenv("ROR_TW_TOKEN")
        aident <- input$AIdent
        employeeInfo <- GET(paste0(url,"/Search/Employees?skip=0&take=1000&employeeId=",
                                 aident),
                          httr::add_headers("x-tw-token" = base,
                                            "Content-Type" = "application/json")) %>% 
            content(.) %>% 
            toJSON() %>% 
            fromJSON(flatten = TRUE)
        
        employeeBranchInfo <- GET(paste0(url,"/Employees/",aident),
                          httr::add_headers("x-tw-token" = base,
                                            "Content-Type" = "application/json")) %>% 
            content(.)
        
        updateTextInput(session,"lastName", value = if_else(is.null(employeeInfo$data$lastName[[1]]),
                                                            NA_character_,
                                                            employeeInfo$data$lastName[[1]]))
        updateTextInput(session,"firstName", value = if_else(is.null(employeeInfo$data$firstName[[1]]),
                                                             NA_character_,
                                                             employeeInfo$data$firstName[[1]]))
        updateTextInput(session,"phoneNumber", value = if_else(is.null(employeeInfo$data$phoneNumber[[1]]),
                                                               NA_character_,
                                                               employeeInfo$data$phoneNumber[[1]]))
        updateTextInput(session,"cellNumber", value = if_else(is.null(employeeInfo$data$cellPhoneNumber[[1]]),
                                                              NA_character_,
                                                              employeeInfo$data$cellPhoneNumber[[1]]))
        updateSelectInput(session,"branchId", selected = as.character(employeeBranchInfo$branchId))
    })
    
    observeEvent(input$update,priority = 1,{
        aident <- as.numeric(formData()[1])
        if(aident != selectedAident())
        {
            createCandidateData(formData())   
        }else{
            updateCandidateData(formData())
        }
        removeModal()
    })
    
    observeEvent(input$personTable_rows_selected, {
        if (length(input$personTable_rows_selected) > 0) {
            data <- CandidateTable()[input$personTable_rows_selected,]
            selectedAident(data$aident)
            selectedRORsrident(data$ror_srident)
            selectedname$fullName <- paste(data$last_name,data$first_name,sep= ', ')
            contact <- ContactTable() %>% filter(aident == data$aident)
            interview <- InterviewTable() %>% filter(aident == data$aident)
            showModal(candidateClickModal(contactdata = contact,
                                          interviewdata = interview))
            #showModal(dataModalUpdate(data = data))
        }
        
    })
    
    # observeEvent(input$contactTable_rows_selected, {
    #     if (length(input$contactTable_rows_selected) > 0) {
    #         data <- ContactTable()[input$contactTable_rows_selected,]
    #         # output$text <- renderTable({data})
    #         selectedAident(data$aident)
    #         selectedRORsrident(data$ror_srident)
    #         #selectedname$fullName <- paste(data$last_name,data$first_name,sep= ', ')
    #         showModal(contactClickModal())
    #         #showModal(dataModalUpdate(data = data))
    #     }
    #     
    # })
    
    observeEvent(input$interviewTable_rows_selected, {
        if (length(input$interviewTable_rows_selected) > 0) {
            data <- InterviewTable()[input$interviewTable_rows_selected,]
            if(data$interview_status == "PENDING")
            {
                selectedAident(data$aident)
                selectedRORsrident(data$ror_srident)
                selectedInterviewID(data$interview_id)
                selectedname$fullName <- paste(data$last_name,data$first_name,sep= ', ')
                showModal(interviewClickModal())
            }
        }
        
    })
    
    observeEvent(input$candidateNewContact,{
        showModal(contactModal(aident = selectedAident(),
                               srident = selectedRORsrident(),
                               name = selectedname$fullName))
    })
    
    observeEvent(input$candidateNewInterview,{
        showModal(interviewModal(aident = selectedAident(),
                               srident = selectedRORsrident(),
                               name = selectedname$fullName))
    })
    
    observeEvent(input$NewContact,priority = 1,{
        showModal(contactModal(aident = selectedAident(),
                               srident = selectedRORsrident(),
                               name = selectedname$fullName))
    })
    
    observeEvent(input$updateCandidate,{
        showModal(dataModalUpdate(aident = selectedAident()))
    })
    
    observeEvent(input$contactUpdate,priority = 1,{
        createContactData(contactFormData())
        removeModal()
        
    })
    
    observeEvent(input$interviewUpdate,priority = 1,{
        createInterviewData(interviewFormData())
        removeModal()
        
    })
    
    observeEvent(input$NewInterview,priority = 1,{
        showModal(interviewModal(aident = selectedAident(),
                               srident = selectedRORsrident(),
                               name = selectedname$fullName))
    })
    
    observeEvent(input$updateInterviewStatus, priority = 1, {
        if(input[['interviewStatusId']] == 2)
        {
            updateInterviewData(InterviewID = selectedInterviewID(),
                                InterviewStatus = as.numeric(input[['interviewStatusId']]))
            showModal(interviewModal(aident = selectedAident(),
                                     srident = selectedRORsrident(),
                                     name = selectedname$fullName))
        }else{
            updateInterviewData(InterviewID = selectedInterviewID(),
                                InterviewStatus = as.numeric(input[['interviewStatusId']]))
            removeModal()
        }
    })
    
    
    output$candidatesBox <- renderValueBox({
        totalCand <- sum(CandidateTable()$created_date >= floor_date(Sys.Date(),
                                                     unit = 'week',
                                                     getOption("lubridate.week.start", 1)))
        valueBox(
            totalCand, "New Candidates", icon = icon("users"),
            color = "yellow"
        )
    })
    output$contactsBox <- renderValueBox({
        totalCand <- sum(ContactTable()$created_date >= floor_date(Sys.Date(),
                                                                     unit = 'week',
                                                                     getOption("lubridate.week.start", 1)))
        valueBox(
            totalCand, "New Contacts", icon = icon("phone-alt"),
            color = "blue"
        )
    })
    output$interviewsBox <- renderValueBox({
        totalCand <- sum(InterviewTable()$created_date >= floor_date(Sys.Date(),
                                                                   unit = 'week',
                                                                   getOption("lubridate.week.start", 1)))
        valueBox(
            totalCand, "New Interviews", icon = icon("calendar"),
            color = "green"
        )
    })

    
}
    

shinyApp(ui = ui, server = server)
