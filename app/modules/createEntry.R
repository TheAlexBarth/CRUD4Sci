createEntryUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("create"), "Create entry", class = "pull-right btn-info"),
    selectInput(ns("tableName"), "Choose a table", character(0)),
    uiOutput(ns("fields")),
    fileInput(ns("fileInput"), "Upload dataframe", accept = c(".csv", ".xlsx")),
    actionButton(ns("upload"), "Upload and create", class = "btn-success"),
    DTOutput(ns("manualEntryTable"))
  )
}
createEntry <- function(input, output, session, pool, reqTable, goHome) {
  observeEvent(tbls(), {
    updateSelectInput(session, "tableName", choices = tbls())
  })
  
  fields <- reactive({
    req(input$tableName)
    pool %>% tbl(input$tableName) %>% head %>% collect %>% 
      lapply(type_sum) %>% unlist
  })
  
  output$fields <- renderUI({
    fieldNames <- names(fields())
    fieldTypes <- unname(fields())
    selections <- vector("list", length(fieldNames))
    for (i in seq_len(length(fieldNames))) {
      nm <- fieldNames[i]
      id <- paste0("field", nm)
      selections[[i]] <- box(width = 3,
                             switch(fieldTypes[i],
                                    int = numericInput(session$ns(id), nm, NULL),
                                    chr = textInput(session$ns(id), nm, NULL)
                             )
      )
    }
    selections
  })
  
  observeEvent(input$create, {
    entryValues <- data.frame(stringsAsFactors = FALSE,
                              lapply(fields(), type.convert)
    )
    
    for (name in names(entryValues)) {
      id <- paste0("field", name)
      
      if (!isTruthy(input[[id]])) {
        showModal(modalDialog(
          title = "NULL value",
          "NULL values are not allowed for new entries",
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
      
      entryValues[name] <- input[[id]]
    }
    
    dbAppendTable(pool, input$tableName, entryValues)
    goHome()
  })
  
  # Handle file upload and create entries
  observeEvent(input$upload, {
    req(input$fileInput)
    file <- input$fileInput$datapath
    
    # Determine file type and read data accordingly
    if (grepl("\\.csv$", file)) {
      df <- read.csv(file)
    } else if (grepl("\\.xlsx$", file)) {
      library(readxl)
      df <- read_excel(file)
    } else {
      showModal(modalDialog(
        title = "Invalid file format",
        "Please upload a CSV or Excel file.",
        easyClose = TRUE, footer = NULL
      ))
      return()
    }
    
    req(input$tableName)
    dbAppendTable(pool, input$tableName, df)
    goHome()
  })
  
  # Render the manual entry table
  output$manualEntryTable <- renderDT({
    req(input$tableName)
    fieldNames <- names(fields())
    data.frame(matrix(ncol = length(fieldNames), nrow = 1, dimnames = list(NULL, fieldNames)))
  }, editable = TRUE)
  
  # Handle manual entry creation
  observeEvent(input$manualEntryTable_cell_edit, {
    info <- input$manualEntryTable_cell_edit
    row <- info$row
    col <- info$col + 1  # Adjust for 1-based indexing
    value <- info$value
    
    # Update the table data
    updatedDF <- isolate({
      df <- input$manualEntryTable
      df[row, col] <- value
      df
    })
    
    # Insert the updated data into the database
    req(input$tableName)
    dbAppendTable(pool, input$tableName, updatedDF)
    goHome()
  })
}
