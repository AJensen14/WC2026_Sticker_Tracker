# UI
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/styles.css")
  ),
  
  titlePanel("🏆 Panini World Cup Sticker Tracker"),
  h4("Designed by Anders Jensen"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      
      # Download section
      div(class = "section-card",
          h4("🚀 Start Tracking"),
          p("Click below for the starting sheet."),
          tags$a(
            "Download Starting CSV",
            href = "starting_data/blank_panini.csv",
            download = NA,
            class = "btn btn-save"
          ),
          hr(), 
          p("Upload this file to track your collection!")
      ),
      
      
      # ---- Upload Section ----
      div(class = "section-card",
          h4("📁 Load Collection"),
          fileInput("file_upload", "Upload CSV", accept = ".csv"),
          textOutput("upload_status")
      ),
      
      # ---- Save Section ----
      div(class = "section-card",
          h4("💾 Save Changes"),
          downloadButton("download_data", "Download Updated CSV", class = "btn-save")
      ),
      
      # ---- Stats Section ----
      div(class = "section-card",
          h4("📊 Collection Stats"),
          uiOutput("stats_display")
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        # ---- Add Cards Tab ----
        tabPanel(
          "Add Cards",
          div(class = "tab-content",
              
              fluidRow(
                # Add by Card ID
                column(6,
                       div(class = "action-card",
                           h4("Add by Card ID"),
                           textInput("add_card_id", "Card ID (e.g., MEX3)", placeholder = "ENG10"),
                           actionButton("btn_add_by_id", "Add Card", class = "btn-action btn-add")
                       )
                ),
                
                # Add by Player Name
                column(6,
                       div(class = "action-card",
                           h4("Add by Player Name"),
                           selectizeInput("add_player_name", "Search Player", choices = NULL, 
                                          options = list(placeholder = "Start typing...")),
                           actionButton("btn_add_by_name", "Add Card", class = "btn-action btn-add")
                       )
                )
              ),
              
              fluidRow(
                # Add Non-Player Card
                column(6,
                       div(class = "action-card",
                           h4("Add Non-Player Card"),
                           selectizeInput("add_nonplayer", "Select Card", choices = NULL),
                           actionButton("btn_add_nonplayer", "Add Card", class = "btn-action btn-add")
                       )
                ),
                
                # Quick Add Multiple
                column(6,
                       div(class = "action-card",
                           h4("Bulk Add (Comma-Separated)"),
                           textAreaInput("bulk_add_ids", "Card IDs", placeholder = "MEX1, MEX2, ENG5", rows = 2),
                           actionButton("btn_bulk_add", "Add All", class = "btn-action btn-add")
                       )
                )
              ),
          ),
          h3("Important note:"),
          p(strong("Cards from Morocco have the 'MOR' annotation instead of the # annotation. This is because both myself and Microsoft Excel are stupid."))
        ),
        
        # ---- Manage Cards Tab ----
        tabPanel(
          "Manage Cards",
          div(class = "tab-content",
              
              fluidRow(
                # Remove Card (Undo mistake)
                column(6,
                       div(class = "action-card",
                           h4("Remove Card (Undo Mistake)"),
                           selectizeInput("remove_card", "Select Collected Card", choices = NULL),
                           actionButton("btn_remove", "Remove from Collection", class = "btn-action btn-remove")
                       )
                ),
                
                # Trade Out Duplicate
                column(6,
                       div(class = "action-card",
                           h4("Trade Out Duplicate"),
                           selectizeInput("trade_card", "Select Card with Duplicates", choices = NULL),
                           actionButton("btn_trade", "Remove 1 Duplicate", class = "btn-action btn-trade")
                       )
                )
              ),
              
              # Feedback
              div(class = "feedback-area",
                  uiOutput("manage_feedback")
              )
          )
        ),
        
        # ---- Sticker Book Tab ----
        tabPanel(
          "Sticker Book",
          div(class = "tab-content",
              
              div(class = "book-controls",
                  selectInput("book_page", "Select Page", choices = NULL, width = "200px"),
                  div(class = "legend",
                      span(class = "legend-item collected", "✓ Collected"),
                      span(class = "legend-item missing", "✗ Missing"),
                      span(class = "legend-item duplicate", "Has Duplicates")
                  )
              ),
              
              uiOutput("sticker_book_page")
          )
        )
      )
    )
  )
)


