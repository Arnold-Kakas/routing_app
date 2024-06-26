# https://github.com/jbryer/DTedit/blob/master/R/dtedit.R

box::use(
  DT[dataTableProxy, replaceData, renderDataTable, dataTableOutput],
  shiny[reactiveValues, selectInput, numericInput, textAreaInput, textInput, passwordInput, renderText,
        showModal, removeModal, renderUI, column, modalButton, div, modalDialog, actionButton, observeEvent, textOutput, p, br]
)

#' @export
dtedit <- function(input, output, name, thedata, id,
                   view.cols = names(thedata),
                   edit.cols = names(thedata),
                   edit.label.cols = edit.cols,
                   input.types,
                   input.choices = NULL,
                   selectize = TRUE,
                   modal.size = 'm',
                   text.width = '100%',
                   textarea.width = '300px',
                   textarea.height = '50px',
                   date.width = '100px',
                   numeric.width = '100px',
                   select.width = '100%',
                   defaultPageLength = 10,
                   title.delete = 'Zmazať',
                   title.edit = 'Upraviť',
                   title.add = 'Pridať',
                   label.delete = 'Zmazať',
                   label.edit = 'Upraviť',
                   label.add = 'Pridať',
                   show.delete = TRUE,
                   show.update = TRUE,
                   show.insert = TRUE,
                   callback.delete = function(data, row) { },
                   callback.update = function(data, olddata, row) { },
                   callback.insert = function(data, row) { },
                   click.time.threshold = 2, # in seconds
                   datatable.options = list(pageLength = 10)
) {
  # Some basic parameter checking
  if(!is.data.frame(thedata) | ncol(thedata) < 1) {
    stop('Must provide a data frame with at least one column.')
  } else if(length(edit.cols) != length(edit.label.cols)) {
    stop('edit.cols and edit.label.cols must be the same length.')
  } else if(!all(view.cols %in% names(thedata))) {
    stop('Not all view.cols are in the data.')
  } else if(!all(edit.cols %in% names(thedata))) {
    stop('Not all edit.cols are in the data.')
  }
  
  if(missing(id)) {
    id <- ''
  } else {
    id <- paste0(id, '-')
  }
  
  DataTableName <- name
  
  result <- reactiveValues()
  result$thedata <- thedata
  result$view.cols <- view.cols
  result$edit.cols <- edit.cols
  
  dt.proxy <- dataTableProxy(DataTableName)
  
  selectInputMultiple <- function(...) {
    selectInput(multiple = TRUE, selectize = selectize, ...)
  }
  
  valid.input.types <- c('numericInput',
                         'textInput')
  inputTypes <- sapply(thedata[,edit.cols], FUN = function(x) {
    switch(class(x),
           character = 'textInput',
           integer = 'numericInput',
           numeric = 'numericInput')
  })
  if (!missing(input.types)) {
    if (!all(names(input.types) %in% edit.cols)) {
      stop('input.types column not a valid editting column: ',
           paste0(names(input.types)[!names(input.types) %in% edit.cols]))
    }
    if (!all(input.types %in% valid.input.types)) {
      stop(paste0('input.types must only contain values of: ',
                  paste0(valid.input.types, collapse = ', ')))
    }
    inputTypes[names(input.types)] <- input.types
  }

  output[[DataTableName]] <- renderDataTable({
    thedata
  }, options = datatable.options, 
  server = TRUE, 
  selection = 'single', 
  rownames = FALSE)
  
  getFields <- function(typeName, values) {
    fields <- list()
    for (i in seq_along(edit.cols)) {
      if (inputTypes[i] == 'numericInput') {
        value <- ifelse(missing(values), 0, values[,edit.cols[i]])
        fields[[i]] <- numericInput(
          inputId = paste0(id, name, typeName, edit.cols[i]),
          label = edit.label.cols[i],
          value = value,
          width = numeric.width)
      } else if (inputTypes[i] == 'textInput') {
        value <- ifelse(missing(values), '', values[,edit.cols[i]])
        fields[[i]] <- textInput(
          inputId = paste0(id, name, typeName, edit.cols[i]),
          label = edit.label.cols[i],
          value = value,
          width = text.width)
      } else {
        stop('Nesprávny typ vstupu!')
      }
    }
    return(fields)
  }
  
  output[[paste0(name, '_message')]] <- renderText('')
  
  updateData <- function(proxy, data, ...) {
    # Convert any list columns to characters before displaying
    for (i in 1:ncol(data)) {
      if (is.list(data[,i])) {
        data[,i] <- sapply(data[,i], FUN = function(x) { paste0(x, collapse = ', ') })
      }
    }
    replaceData(proxy, data, ...)
  }
  
  ##### Insert functions #####################################################
  
  observeEvent(input[[paste0(name, '_add')]], {
    if (!is.null(row)) {
      showModal(addModal())
    }
  })
  
  insert.click <- NA
  
  observeEvent(input[[paste0(name, '_insert')]], {
    if (!is.na(insert.click)) {
      lastclick <- as.numeric(Sys.time() - insert.click, units = 'secs')
      if (lastclick < click.time.threshold) {
        warning(paste0('Dvojité kliknutie. Ignorujem výzvy na vloženie do tabulky.'))
        return()
      }
    }
    insert.click <<- Sys.time()
    
    newdata <- result$thedata
    row <- nrow(newdata) + 1
    newdata[row,] <- NA
    
    for (i in edit.cols) {
      if (inputTypes[i] %in% c('selectInputMultiple')) {
        newdata[[i]][row] <- list(input[[paste0(name, '_add_', i)]])
      } else {
        newdata[row,i] <- input[[paste0(name, '_add_', i)]]
      }
    }
    tryCatch({
      callback.data <- callback.insert(data = newdata, row = row)
      if (!is.null(callback.data) & is.data.frame(callback.data)) {
        result$thedata <- callback.data
      } else {
        result$thedata <- newdata
      }
      updateData(dt.proxy,
                 result$thedata[,view.cols],
                 rownames = FALSE)
      removeModal()
      return(TRUE)
    }, error = function(e) {
      output[[paste0(name, '_message')]] <<- renderText(geterrmessage())
      return(FALSE)
    })
  })
  
  addModal <- function(row, values) {
    output[[paste0(name, '_message')]] <- renderText('')
    fields <- getFields('_add_', values)
    modalDialog(title = title.add,
                       div(textOutput(paste0(name, '_message')), style = 'color:red'),
                       fields,
                       footer = column(modalButton('Cancel'),
                                              actionButton(paste0(id, name, '_insert'), 'Save'),
                                              width = 12),
                       size = modal.size
    )
  }
  
  
  ##### Update functions #####################################################
  
  observeEvent(input[[paste0(name, '_edit')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if (!is.null(row)) {
      if (row > 0) {
        showModal(editModal(row))
      }
    }
  })
  
  update.click <- NA
  
  observeEvent(input[[paste0(name, '_update')]], {
    if (!is.na(update.click)) {
      lastclick <- as.numeric(Sys.time() - update.click, units = 'secs')
      if (lastclick < click.time.threshold) {
        warning(paste0('Dvojité kliknutie. Ignorujem výzvy na aktualizáciu tabulky.'))
        return()
      }
    }
    update.click <- Sys.time()
    
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if (!is.null(row)) {
      if (row > 0) {
        newdata <- result$thedata
        for (i in edit.cols) {
          if (inputTypes[i] %in% c('selectInputMultiple')) {
            newdata[[i]][row] <- list(input[[paste0(name, '_edit_', i)]])
          } else {
            newdata[row,i] <- input[[paste0(name, '_edit_', i)]]
          }
        }
        tryCatch({
          callback.data <- callback.update(data = newdata,
                                           olddata = result$thedata,
                                           row = row)
          if (!is.null(callback.data) & is.data.frame(callback.data)) {
            result$thedata <- callback.data
          } else {
            result$thedata <- newdata
          }
          updateData(dt.proxy,
                     result$thedata[,view.cols],
                     rownames = FALSE)
          removeModal()
          return(TRUE)
        }, error = function(e) {
          output[[paste0(name, '_message')]] <<- renderText(geterrmessage())
          return(FALSE)
        })
      }
    }
    return(FALSE)
  })
  
  editModal <- function(row) {
    output[[paste0(name, '_message')]] <- renderText('')
    fields <- getFields('_edit_', values = result$thedata[row,])
    modalDialog(title = title.edit,
                       div(textOutput(paste0(name, '_message')), style = 'color:red'),
                       fields,
                       footer = column(modalButton('Cancel'),
                                       actionButton(paste0(id, name, '_update'), 'Save'),
                                       width = 12),
                       size = modal.size
    )
  }
  
  ##### Delete functions #####################################################
  
  observeEvent(input[[paste0(name, '_remove')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if (!is.null(row)) {
      if (row > 0) {
        showModal(deleteModal(row))
      }
    }
  })
  
  observeEvent(input[[paste0(name, '_delete')]], {
    row <- input[[paste0(name, 'dt_rows_selected')]]
    if (!is.null(row)) {
      if (row > 0) {
        newdata <- callback.delete(data = result$thedata, row = row)
        if (!is.null(newdata) & is.data.frame(newdata)) {
          result$thedata <- newdata
        } else {
          result$thedata <- result$thedata[-row,]
        }
        updateData(dt.proxy,
                   result$thedata[,view.cols],
                   rownames = FALSE)
        removeModal()
        return(TRUE)
      }
    }
    return(FALSE)
  })
  
  deleteModal <- function(row) {
    fields <- list()
    for (i in view.cols) {
      fields[[i]] <- div(paste0(i, ' = ', result$thedata[row,i]))
    }
    modalDialog(title = title.delete,
                       p('Naozaj chcete odstrániť tento záznam?'),
                       fields,
                       footer = column(modalButton('Zrušiť'),
                                              actionButton(paste0(id, name, '_delete'), 'Zmazať'),
                                              width = 12),
                       size = modal.size
    )
  }
  
  ##### Build the UI for the DataTable and buttons ###########################
  
  output[[name]] <- renderUI({
    div(
      if (show.insert) { actionButton(paste0(id, name, '_add'), label.add) },
      if (show.update) { actionButton(paste0(id, name, '_edit'), label.edit) },
      if (show.delete) { actionButton(paste0(id, name, '_remove'), label.delete) },
      br(), br(), dataTableOutput(paste0(id, DataTableName))
    )
  })
  
  return(result)
}
