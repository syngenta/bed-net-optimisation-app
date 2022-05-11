
server <- function(input, output, session){
  
  dir.create('tmp', showWarnings=FALSE)
  rv <- reactiveValues(sims=list())
  
  
  # SIMULATION ####
  
  busy <- reactiveVal()
  
  observeEvent(input$restore_defaults, {
    for (parameter in names(DEFAULTS)){
      tryCatch(
        expr={
          f <- get(UPDATE_FUNCTIONS[[parameter]])
          f(session, parameter, value=DEFAULTS[[parameter]])
        },
        error=function(e) print(paste('Failed to update:', parameter))
      )
    }
  })
  
  observeEvent(input$run_sim, ignoreInit=TRUE, {
    if (isTRUE(busy())) return()
    
    if (input$sim_name %in% names(rv$sims)){
      showNotification(
        'Simulation name already used. Set a different name in the box on the left.',
        type='error'
      )
      return()
    }
    
    if (length(rv$sims) >= 50) {
      showNotification('Simulation limit reached', type='error')
      return()
    }
    
    v <- validate_settings(input)
    if (!v) return()
    
    rv$progress <- Progress$new(getDefaultReactiveDomain(), min=0, max=1)
    rv$progress$set(value=0, message='Running simulation', detail='Please wait ...')
    
    e <- new.env(parent=emptyenv())
    e$l <- reactiveValuesToList(input)
    e$token <- session$token
    saveRDS(.1, file.path('tmp', e$token))
    
    busy(TRUE)
    future(envir=e, globals=c('l', 'token'), expr={
      
      source('r/helpers.r')
      source('r/run_simulation.r')
      
      try(run_sim(l, token), silent=TRUE)

    }) %...>%
      busy()
    
    NULL
  })
  
  observe({
    if (!inherits(rv$progress, 'Progress')) return()
    invalidateLater(500)
    path <- file.path('tmp', session$token)
    
    if (!file.exists(path)) {
      rv$progress$close()
      rv$progress <- NULL
    } else {
      try(
        expr={
          x <- readRDS(path)
          rv$progress$set(x)
        },
        silent=TRUE
      )
    }
  })
  
  observeEvent(busy(), {
    if (isTRUE(busy())) return()
    r <- busy()
    
    if (inherits(r, 'try-error')){
      showNotification('Simulation Failed', type='error')
      if (inherits(rv$progress, 'Progress')){
        rv$progress$close()
        rv$progress <- NULL
        file.remove(file.path('tmp', session$token))
      }
    } else {
      showNotification('Simulation succeeded', type='message')
      
      simname <- if (input$sim_name != '') input$sim_name else paste0('sim_', length(rv$sims) + 1)
      rv$sims[[simname]] <- r
      lapply(1:N_SIM_ROWS, function(x){
        updateSelectInput(
          session,
          paste0('sim_selected', x),
          choices=names(rv$sims),
          selected=if (x == 1) simname else input[[paste0('sim_selected', x)]]
        )
      })
    }
  })
  
  observeEvent(input$plot_test1, {
    rv$test_AI <- 1
    show_test_modal()
  })
  
  observeEvent(input$plot_test2, {
    rv$test_AI <- 2
    show_test_modal()
  })
  
  output$test_chemical <- renderPlot({
    plot_test(input, rv$test_AI, 'chemical')
  })
  
  output$test_mortality <- renderPlot({
    plot_test(input, rv$test_AI, 'mortality')
  })

  # RESULTS ####
  
  # ** tables ####
  
  lapply(1:N_SIM_ROWS, function(x){
    
    output[[paste0('summary', x)]] <- DT::renderDataTable({
      
      req(rv[[paste0('sim_results', x)]])
      d <- optima_table(rv[[paste0('sim_results', x)]])
      
      percent.cols <- grep('Coverage|Simulation|Bioefficacy', colnames(d), value=TRUE)
      dollar.cols <- grep('Cost', colnames(d), value=TRUE)
      relative.cols <- grep('Relative', colnames(d), value=TRUE)
      lifespan.cols <- grep('Lifespan', colnames(d), value=TRUE)
      
      for (col in setdiff(colnames(d), 'Optimum Type')) {
        d[[col]] <- suppressWarnings(as.numeric(as.character(d[[col]])))
      }
      for (col in percent.cols) d[[col]] <- d[[col]] / 100
      
      d %>% 
        datatable(
          rownames=FALSE,
          options=list(
            dom='t',
            pageLength=nrow(d)
          )
        ) %>% 
        formatCurrency(dollar.cols) %>% 
        formatPercentage(percent.cols, digits=1) %>% 
        formatStyle(
          percent.cols,
          background = styleColorBar(c(0, 1), '#80deea'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>% 
        formatStyle(
          dollar.cols,
          background = styleColorBar(c(0, max(d[, dollar.cols], na.rm=TRUE)), '#66bb6a'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>% 
        formatStyle(
          relative.cols,
          background = styleColorBar(c(0, range(d[, relative.cols], na.rm=TRUE)), '#ffe082'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>% 
        formatStyle(
          lifespan.cols,
          background = styleColorBar(c(0, range(d[, lifespan.cols], na.rm=TRUE)), '#ef9a9a'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
        
    })
  })
  
  # ** graphs ####
  
  lapply(1:N_SIM_ROWS, function(x) make_outputs(output, rv, x))
 
  # ** sims ####
  
  observeEvent(input$show_export_modal, {
    x <- names(rv$sims)
    showModal(modalDialog(
      size='s',
      title='Select which simulations to export',
      checkboxGroupInput('export_selection', '', choices=x, width='100%'),
      footer=tagList(
        downloadButton('export_sims', 'Export Bookmarks', class='btn-primary'),
        modalButton('Dismiss')
      )
    ))
  })
  
  output$export_sims <- downloadHandler(
    'sims.rds',
    function(file){
      removeModal()
      saveRDS(rv$sims[input$export_selection], file)
    }
  )
  
  observeEvent(input$show_import_modal, {
    showModal(modalDialog(
      size='s',
      title='Upload sim file here',
      fileInput('file_upload', 'Upload file')
    ))
  })
  
  observeEvent(input$file_upload, {
    removeModal()
    path <- input$file_upload$datapath
    r <- try(
      expr={
        l <- readRDS(path)
        i <- names(l) %in% names(rv$sims)
        names(l)[i] <- paste0(names(l)[i], ' (2)')
        rv$sims <- c(rv$sims, l)
        
        lapply(1:N_SIM_ROWS, function(x){
          updateSelectInput(
            session,
            paste0('sim_selected', x),
            choices=names(rv$sims),
            selected=input[[paste0('sim_selected', x)]]
          )
        })
      },
      silent=TRUE
    )
    
    if (inherits(r, 'try-error')){
      showNotification('Import failed', type='error')
    } else {
      showNotification('Import successful', type='message')
    }
  })
  
  lapply(1:N_SIM_ROWS, function(x) {
    
    # sim names above tables
    output[[paste0('table_simname', x)]] <- renderText({
      val <- input[[paste0('sim_selected', x)]]
      if (val == 'NA') NULL else val
    })
    # sim names above graphs
    for (prefix in c('base', 'mvc', 'cheapest', 'who', 'universal')){
      for (simname in c('input_curves',  'output_dynamics',  'vector_control',  'coverage',  'bednet_cost')){
        output[[paste0(prefix, simname, x)]] <- renderText({
          val <- input[[paste0('sim_selected', x)]]
          if (val == 'NA') NULL else val
        })
      }
    }
    
    # sim selection in saved sims tab
    observeEvent(input[[paste0('sim_selected', x)]], ignoreInit=TRUE, {
      rv[[paste0('sim_results', x)]] <- rv$sims[[input[[paste0('sim_selected', x)]]]]
    })
    
    output[[paste0('sim_details', x)]] <- renderUI({
      req(rv[[paste0('sim_results', x)]])
      l <- rv[[paste0('sim_results', x)]]
      x <- intersect(names(DEFAULTS), names(l))
      
      diff_from_default <- sapply(x, function(i) l[[i]] != DEFAULTS[[i]])
      diff_from_default <- names(diff_from_default[diff_from_default])
      
      if (!length(diff_from_default)) return('Default parameter values used')
      
      l <- l[diff_from_default]
      names(l) %>% 
        sapply(function(x) paste0(PARAMETER_MAP[[x]], ': ', l[[x]])) %>% 
        paste(collapse='<br>') %>% 
        paste('Parameters different from default:', ., sep='<br>') %>% 
        HTML()
    })
    
    observeEvent(input[[paste0('del_modal', x)]], {
      sim <- input[[paste0('sim_selected', x)]]
      if (!sim %in% names(rv$sims)) return()
      showModal(modalDialog(
        size='s',
        title=paste0('Delete ', sim, '?'),
        p('All the hard work will forever be lost!'),
        footer=tagList(
          actionButton(paste0('del_sim', x), 'Delete', class='btn-danger'),
          modalButton('Cancel')
        )
      ))
    })
    
    observeEvent(input[[paste0('del_sim', x)]], {
      
      removeModal()
      
      sim <- input[[paste0('sim_selected', x)]]
      rv$sims[[sim]] <- NULL
      rv[[paste0('sim_results', x)]] <- NULL
      
      lapply( (1:N_SIM_ROWS)[-x], function(i){
        updateSelectInput(
          session,
          paste0('sim_selected', i),
          choices=names(rv$sims),
          selected=input[[paste0('sim_selected', i)]]
        )
      })
      updateSelectInput(
        session,
        paste0('sim_selected', x),
        choices=names(rv$sims),
        selected=NA
      )
    })
  })
}
