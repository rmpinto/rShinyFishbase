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
                                        choices = unique(list_fields()$table), selected = 'species', multiple = T, options = NULL,
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
               # TODO: create dynamic UI component, one for each selected table
               fluidRow(
                  column(12,DTOutput('tbl1', height='100%'))
               )
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
   
   # Taxonomy selection
   observeEvent(input$class,
                {
                   taxoClass <- input$class
                   taxoOrderOpt <- unique(taxoOpt$Order[which(taxoOpt$Class == taxoClass)])
                   updateSelectInput(session, "order",
                                     choices = c('ALL', taxoOrderOpt),
                                     selected = 'ALL')
                })
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
      speciesList <- as.character(input$validatedList)
      if(length(speciesList)>0 && speciesList != ""){
         
         withProgress(message = 'Getting data...', value = 0, {
            
               incProgress(0.5)
               nSpecies <- length(speciesList)
               dataset <- species(species_list = speciesList)
               
               incProgress(0.5)
               output$tbl1 <- renderDT(dataset,
                                       extensions = c('Scroller', 'Buttons', 'ColReorder','Responsive'),
                                       options = list(autoWidth = T,
                                                      dom = 'Bfrtip',
                                                      keys = F,
                                                      scrollX = T,
                                                      deferRender = T,
                                                      scrollY = 600,
                                                      scroller = T,
                                                      colReorder = T,
                                                      buttons = c('copy', 'csv', 'excel'),
                                                      fixedColumns = list(leftColumns = 3)
                                       )
               )
               
            
         })
         
      }
   })
   
}

shinyApp(ui = ui, server = server)

