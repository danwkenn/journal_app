#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)
library(shinyTime)
library(shinyFiles)
library(tidyverse)
library(readxl)
# Define UI for application that draws a histogram
modes <- getAceModes()
themes <- getAceThemes()

source("resources/functions.R")

default_location <- paste0(fs::path_home(),"/Dropbox/workware/lab-notes-taker")

init <- "Write your journal entry in \\LaTeX here."

library(tidyverse)

row <- function(...) {
    tags$div(class="row", ...)
}

col <- function(width, ...) {
    tags$div(class=paste0("span", width), ...)
}

source("compile_all_notes.R")

ui <- shinyUI(bootstrapPage(
    
    headerPanel("New Note"),
    
    mainPanel(
        
        tags$div(
            class = "container",
            row(
                column(6,
                       dateInput("date", label = h3("Date input"), value = Sys.Date(),format = "dd/mm/yyyy"),
                       timeInput(inputId = "time",label = "Time",value = Sys.time(),seconds = FALSE),
                       textInput("title", label = "Title",value = "Enter short title here..."),
                       aceEditor(
                           outputId = "ace",
                           # to access content of `selectionId` in server.R use `ace_selection`
                           # i.e., the outputId is prepended to the selectionId for use
                           # with Shiny modules
                           mode = "latex",
                           selectionId = "selection",
                           value = init,
                           placeholder = "Show a placeholder when the editor is empty ...",wordWrap = TRUE),
                       actionButton("do", "Render PDF"),
                       actionButton("save_note", "Save note to file"),
                       actionButton("compile_full", "Render journal"),
                       shinyDirButton("dir", "Input directory", "Upload"),
                       uiOutput("DownloadButton"),
                       verbatimTextOutput("dir", placeholder = TRUE)),
                column(width = 6, htmlOutput('pdfviewer',inline = FALSE))
            )
        )
    )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
    
    #Create save filename:
    save_file_name = paste0("notes/",text_combination_generator(),".tex")
    
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    shinyDirChoose(input, "dir", roots = volumes, session = session)
    
    global <- reactiveValues(
        datapath = getwd(),
        journal_files_location = default_location,
        contact_details = {
            contact_details <- read_xlsx(paste0(default_location,.Platform$file.sep,"contacts.xlsx"))
            contact_details$short_form[is.na(contact_details$short_form)] <-
                contact_details$`First Name`[is.na(contact_details$short_form)]
            contact_details
        })
    
    dir <- reactive(input$dir)
    # paste0(fs::path_home(),paste0(unlist(input$dir),collapse = .Platform$file.sep),sep = .Platform$file.sep)
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- fs::path_home()
                     global$journal_files_location <-
                         file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                     global$contact_details <- {
                         contact_details <- read_xlsx(
                             paste0(journal_files_location,.Platform$file.sep,"contacts.xlsx"))
                         contact_details$short_form[is.na(contact_details$short_form)] <-
                             contact_details$`First Name`[is.na(contact_details$short_form)]
                         contact_details
                     }
                 })
    
    output$dir <- renderText({
        global$journal_files_location
    })
    
    # Read in the template:
    template_tex <- readChar("template.tex", file.info("template.tex")$size)
    quick_complete <- readChar("quick_complete.tex", file.info("quick_complete.tex")$size)
    # Add in date and time:
    date = reactive({input$date})
    hour = reactive({as.character(as.numeric(format(input$time, "%I")))})
    min = reactive({format(input$time, "%M")})
    time = reactive({paste0(hour(),":",min(),tolower(format(input$time, "%p")))})
    file_name = paste0("notes/",text_combination_generator(),".tex")
    
    #
    content_tex <- reactive({
        content  <- gsub(pattern = "FILL_TIME", replace = time(), x = template_tex)
        content  <- gsub(pattern = "FILL_DATE", replace = date(), x = content)
        content <- gsub(pattern = "SHORT_TITLE",replace = input$title, x = content)
        content <- str_replace(content,
                    pattern = "%%CONTENT%%",
                    replacement = str_replace_all(
                        input$ace,
                        pattern = "\\\\","\\\\\\\\"))
        content
    })
    
    processed_content_tex <- reactive({
        other_text <- str_split(content_tex(),"<>([[:alnum:]]{1,10})<>")[[1]]
        contacts_raw <- str_match_all(content_tex(),"<>([[:alnum:]]{1,10})<>")[[1]][,2]
        
        if(length(contacts_raw) > 0){
            contact_ids <- match(contacts_raw,contact_details$Abbreviation)
            contacts_first <- (!duplicated(contact_ids) & !is.na(contact_ids)) 
            other_contacts <- (duplicated(contact_ids) & !is.na(contact_ids)) 
            contacts <- character(length = length(contact_ids))
            contacts[contacts_first] <- paste0(
                "\\href{mailto:",
                contact_details$email[contact_ids[contacts_first]],
                "}{\\textbf{",
                contact_details$short_form[contact_ids[contacts_first]]," ",
                contact_details$Surname[contact_ids[contacts_first]],"}}")
            
            contacts[other_contacts] <- paste0(
                "\\href{mailto:",
                contact_details$email[contact_ids[other_contacts]],
                "}{\\textbf{",
                substr(contact_details$short_form[contact_ids[other_contacts]],1,1),"",
                substr(contact_details$Surname[contact_ids[other_contacts]],1,1),"}}")
            contacts[is.na(contact_ids)] <- contacts_raw[is.na(contact_ids)]
            
            result <- 
                paste0(
                    paste0(
                        paste0(
                            other_text[1:(length(other_text)-1)],
                            contacts
                        ),collapse = ""),
                    other_text[length(other_text)]
                )
            result
        }else{
            content_tex()
        }
        }
    )
    
    # Replace placeholder with content.
    document_tex <- reactive({
        tex_file_content <- str_replace(quick_complete,
                            pattern = "%%CONTENT%%",
                            replacement = str_replace_all(
                                processed_content_tex(),
                                pattern = "\\\\","\\\\\\\\"))
        return(tex_file_content)
    })
    
    # When button clicked re-render the .tex file into a PDF.
    observeEvent(input$do, {
        # Write:
        cat(document_tex())
        sink(file = paste0("www/temp",".tex"))
        cat(document_tex())
        sink(file = NULL)
        # Render:
        tools::texi2pdf(paste0("www/temp",".tex"),clean = TRUE)
        file.copy(from = paste0("temp",".pdf"),to = paste0("www/","temp",".pdf"),overwrite = TRUE)
        file.remove("temp.pdf")
    })
    
    # Creates the HTML tag for rendering the PDF.
    html_render <- eventReactive(input$do,{
        return(
            {tags$iframe(src=paste0("","temp",".pdf"), height=600, width = "100%")}
        )
    })

    output$pdfviewer <- renderUI({
        
        input$do
        
        print("re-rendering UI")
        refresh_render <- isolate(document_tex())
        addResourcePath("resources",getwd())
        return(isolate(html_render()))
    })
    
    observeEvent(input$save_note,{
        
        sink(file = paste0(global$journal_files_location,.Platform$file.sep,save_file_name))
        cat(content_tex())
        sink(file = NULL)
        
    })
    message(getwd())
    observeEvent(input$compile_full,{
        compile_all_notes(wd = global$journal_files_location)
    })
    
    output$journal_file_download <- downloadHandler(
        filename ="temp.pdf",
        content = function(file){file.copy("temp.pdf", file)}
    )
    
    observeEvent(input$compile_full,{
    output$DownloadButton <<- renderUI({
        downloadButton(outputId = "journal_file_download", label = "Download")
    })
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
