library(shiny)
library(pdftools)
library(tidyverse)
library(writexl)
library(readxl)
library(shinyjs)
library(marker)

library(shinybusy)
ui <- fluidPage(style="padding:60px;",
tags$head(
    useShinyjs()
     ,
    useMarker(), # include dependencies

    add_busy_bar(color = "#8fb8ff"),
    tags$style(HTML("
      .source_id{
        background-color: black;
        color: white;
      }
      .page::before{
        content: 'Page: ';
      }
      .page{
        background-color: #edeeed;

      }
      .source_id::before{
        content: 'Source: ';
      }
      #results{
       width: 800px;
      }
      "))

),
h1("Search multiple pdf files"),
p("Select more than one file (if required) by pressing shift, cmd or control when selecting. Then wait until upload is finished."),
p("Caveat: this tool produces quick but not beautiful search results!"),
p("It may not find all text within the PDF, and sometimes it may mangle the text if it is laid out in columns."),
# p("You will then be able to download an Excel-format spreadsheet with all the text data in a single column, with one row for each of the original text areas in the documents, together with columns for absolute (not necessarily printed) page number and a column for document name."),
    fileInput("upload_upload",icon("upload") %>% span(" Upload"),
              multiple=T,
              width="400px",
              placeholder="Drag files here or press Browse",
              accept = c("application/pdf")),

div(id="down",p("Upload finished"),
h3("Search"),
p("You can use | to separate different search terms."),
textInput("search_text",NULL,placeholder = "Search text"),
h3("Results"),
div(id="results"),
) %>% hidden
# ,
# div("Free service by ",a("Causal Map",href="https://causalmap.app"),style="position:absolute;bottom:60px;left:60px")
# ,rpdfOutput("pdf", width = "100%", height = "400px")

)

server <- function(input, output) {
  marker <- marker$new("#results")

options(shiny.maxRequestSize = 300*1024^2)
    control <- reactiveValues()
    get_text <- function(inFile,x){

        pdf_text(inFile$datapath[x])%>%
            map(~str_replace_all(.," {2,}"," ")) %>%
            # map(~str_replace_all(.,"\r\n"," ")) %>%
            unlist %>%
            tibble(text=.,page=seq_along(.),source_id=inFile$name[[x]])#,author=pdf_info(inFile$datapath[[x]])$author,modified=pdf_info(inFile$datapath[[x]])$modified)
    }
    observeEvent(input$search_text,{
      if(input$search_text=="")return()
# browser()
    res <-
      control$content$statements %>%
        filter(
          str_detect(tolower(text),tolower(input$search_text))
          )
    if(nrow(res)==0){  html("results","No results");return()}
    res %>%
        mutate(text=map(text,~div(.,class="text") %>% as.character )%>% unlist) %>%
        mutate(page=map(page,~div(.,class="page") %>% as.character) %>% unlist) %>%
        mutate(source_id=map(source_id, ~div(.,class="source_id") %>% as.character) %>% unlist)  %>%
      unite(result,source_id,page,text,sep="") %>%
        pull(result) %>%
        paste0(collapse="") %>%
        # map(div) %>%
      html("results",.)

      marker$
        unmark()$ # unmark all before we mark
        mark(input$search_text) # highlight text
    })
    observeEvent(input$upload_upload,{

            inFile <- isolate(input$upload_upload)
        if (is.null(inFile))
            return(NULL)
# browser()
            control$content <- seq_along(1:nrow(inFile)) %>%
                map(~get_text(inFile,.)) %>%
                bind_rows %>%
                list(statements=.)

        shinyjs::show("down")
    })




}

shinyApp(ui = ui, server = server)
