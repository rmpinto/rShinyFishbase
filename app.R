library(shiny)
library(DT)
library(rfishbase)

ui <- tagList(
   includeCSS("styles.css"),
   navbarPage(
      "rShinyFishbase",
      collapsible = T,
      inverse = T,
      tabPanel("Data",
               wellPanel(
                  fluidRow(
                     column(8,
                            textInput('speciesInput',
                                      label = 'Add species',
                                      width = '100%')
                     ),
                     column(2,
                            actionButton('addSpecies',
                                         label = '',
                                         icon = icon('plus'),
                                         class = 'btn-primary',
                                         width = '100%')
                     )
                  ),
                  fluidRow(
                     column(2,
                            checkboxInput("checkbox1", label = "Scientific name", value = T)
                     ),
                     column(2,
                            checkboxInput("checkbox2", label = "Common name", value = F)
                     ),
                     column(2,
                            checkboxInput("checkbox3", label = "Species code", value = F)
                     )
                  )
               ),
               hr(),
               wellPanel(
                  fluidRow(
                     column(2,
                            selectInput('class', 'Class', choices = unique(fishbase$Class), selectize = F, width='100%')
                     ),
                     column(2,
                            selectInput('order', 'Order', choices = NULL, selectize = F, selected = NULL, width='100%')
                     ),
                     column(2,
                            selectInput('family', 'Family', choices = NULL, selectize = F, selected = NULL, width='100%')
                     ),
                     column(2,
                            selectInput('genus', 'Genus', choices = NULL, selectize = F, selected = NULL, width='100%')
                     ),
                     column(2,
                            actionButton('addTaxo',
                                         label = '',
                                         icon = icon('plus'),
                                         class = 'btn-primary',
                                         width = '100%')
                     )
                  )
               ),
               hr(),
               fluidRow(
                  column(12,
                         selectizeInput('validatedList',
                                        label = 'Species List',
                                        choices = NULL, selected = NULL, multiple = T, options = NULL,
                                        width = '100%')
                  )
               ),
               hr(),
               fluidRow(
                  column(12,
                         selectizeInput('tablesList',
                                        label = 'Select tables',
                                        choices = "", multiple = T, options = NULL,
                                        width = '100%')
                  )
               ),
               fluidRow(
                  column(12,
                         actionButton('getData',
                                      'GET DATA',
                                      icon = icon('data'),
                                      class = 'btn-warning',
                                      width = '100%')
                  )
               ),
               hr(),
               # TODO: fix table width to 100%
               uiOutput("tables", inline=T)
      ),
      tabPanel("About"),
      hr(),
      footer = div(style='text-align: center;',
                   HTML('Based on <code>rfishbase 3.0</code>, an rOpenSci package available at <a href="https://github.com/ropensci/rfishbase">https://github.com/ropensci/rfishbase</a>'))
   )
)

server <- function(input, output, session) {
   
   # Global server variables
   taxoOpt <- fishbase[,c('Genus','Family','Order','Class')]
   
   # Select available wrappers for fishbase tables
   pckFun <- lsf.str('package:rfishbase')
   availTbls <- c()
   for(i in 1:(length(pckFun)) ) {
      if('species_list' %in% names(formals(pckFun[[i]]))){
         availTbls <- c(availTbls, pckFun[i])
      }
   }
   observe({
      updateSelectInput(session, "tablesList",
                        choices = availTbls,
                        selected = 'species'
      )})
   
   # Taxonomy selection - Class
   observeEvent(input$class,
                {
                   taxoClass <- input$class
                   taxoOrderOpt <- unique(taxoOpt$Order[which(taxoOpt$Class == taxoClass)])
                   updateSelectInput(session, "order",
                                     choices = c('ALL', taxoOrderOpt),
                                     selected = 'ALL')
                })
   # Taxonomy selection - Order
   observeEvent(input$order,
                {
                   taxoOrder <- input$order
                   taxoClass <- input$class
                   taxoFamilyOpt <- unique(taxoOpt$Family[which(taxoOpt$Class == taxoClass & taxoOpt$Order == taxoOrder)])
                   if (length(taxoOrder) > 0 &&
                       length(taxoClass) > 0){
                      updateSelectInput(session, "family",
                                        choices = c('ALL',taxoFamilyOpt),
                                        selected = 'ALL')
                   }
                })
   # Taxonomy selection - Family
   observeEvent(input$family,
                {
                   taxoOrder <- input$order
                   taxoClass <- input$class
                   taxoFamily <- input$family
                   taxoGenusOpt <- unique(taxoOpt$Genus[which(taxoOpt$Class == taxoClass & taxoOpt$Order == taxoOrder & taxoOpt$Family == taxoFamily)])
                   if (length(taxoOrder) > 0 &&
                       length(taxoClass) > 0 &&
                       length(taxoFamily) > 0){
                      updateSelectInput(session, "genus",
                                        choices = c('ALL',taxoGenusOpt),
                                        selected = 'ALL')
                   }
                })
   
   # addSpecies
   observeEvent(input$addSpecies, {
      
      if(length(input$speciesInput)>0 && input$speciesInput != "") {
         
         withProgress(message = 'Validating input...', value = 0, {
            
            incProgress(0.5)
            newSpecies <- unlist(strsplit(input$speciesInput,',|\t|\n|;'))
            newSpecies <- as.vector(sapply(newSpecies, function(x) trimws(x, which=c('both') )))
            newSpecies <- validate_names(newSpecies)
            validatedSpeciesList <- c(input$validatedList, newSpecies)
            
            incProgress(0.5)
            updateSelectizeInput(session, 'validatedList', choices = validatedSpeciesList, selected = validatedSpeciesList, server = F)
            updateTextInput(session, 'speciesInput', value='')
            
            
         })
      }
   })
   
   # addTaxo
   observeEvent(input$addTaxo, {
      selOrder<- NULL
      selFamily <- NULL
      selGenus <- NULL
      if(input$order != 'ALL'){selOrder <- input$order}
      if(input$family != 'ALL'){selFamily <- input$family}
      if(input$genus != 'ALL'){selGenus <- input$genus}
      validatedSpeciesList <- species_list(Class = input$class,
                                           Order = selOrder,
                                           Family = selFamily,
                                           Genus = selGenus)
      updateSelectizeInput(session, 'validatedList', choices = validatedSpeciesList, selected = validatedSpeciesList, server = F)
   })
   
   # getData
   observeEvent(input$getData, {
      
      # FIXME: species table not showing
      # Create HTML elements dynamically
      output$tables = renderUI({
         nTabs = input$tablesList
         myTables = lapply(paste0('tbl-', nTabs), function(x){
            fluidRow(column(12,
                            fluidRow(column(12,
                                            wellPanel(h4(toupper(unlist(strsplit(x,'-'))[2]))))
                            ),
                            fluidRow(column(12,
                                            DTOutput(outputId = x, height='100%', width='100%'))
                            ),
                            hr()
            )
            )
            
         })
         do.call(tabPanel, myTables)
      })
      
      # Get species list
      speciesList <- as.character(input$validatedList)
      
      # Call rfishbase tables and render them
      if(length(speciesList)>0 && speciesList != ""){
         
         # Selected tables
         tblsName <- input$tablesList
         tblsNumber <- length(input$tablesList)
         
         
         for(i in 1:tblsNumber) {
            
            # Limit scope
            local({
               
               # Call rsfishbase table wrapper
               thisId <- tblsName[i]
               tmpTbl <- eval(parse(text=paste0(thisId,'(species_list = speciesList)')))
               
               # Render DT output linked to HTML elements
               output[[paste0("tbl-", thisId)]] <-
                  renderDT(tmpTbl,
                           extensions = c('Scroller', 'Buttons', 'ColReorder','Responsive'),
                           options = list(autoWidth = T,
                                          dom = 'Bfrtip',
                                          keys = F,
                                          scrollX = T,
                                          deferRender = T,
                                          scrollY = 300,
                                          scroller = T,
                                          colReorder = T,
                                          buttons = c('copy', 'csv', 'excel'),
                                          fixedColumns = list(leftColumns = 3)
                           )
                  )
               
            })
         }
      }
   })
}

shinyApp(ui = ui, server = server)

