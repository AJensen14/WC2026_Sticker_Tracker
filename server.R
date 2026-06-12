# Server
server <- function(input, output, session) {
  
  
  # =========================================================================
  # REACTIVE DATA
  
  # =========================================================================
  
  # Main data store
  collection <- reactiveVal(NULL)
  
  # Session action history for potential undo
  
  action_log <- reactiveVal(list())
  
  # Feedback messages
  add_message <- reactiveVal(NULL)
  manage_message <- reactiveVal(NULL)
  
  
  # =========================================================================
  # FILE UPLOAD
  # =========================================================================
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      df <- read_csv(input$file_upload$datapath, show_col_types = FALSE)
      
      # Validate required columns
      required_cols <- c("card_id", "card_description", "association", "card_type", 
                         "nation", "collected", "duplicate", "main_set", "page_number")
      missing <- setdiff(required_cols, names(df))
      
      if (length(missing) > 0) {
        add_message(list(type = "error", 
                         text = paste("Missing columns:", paste(missing, collapse = ", "))))
        return()
      }
      
      # Normalise data
      df <- df %>%
        mutate(card_id = toupper(trimws(card_id)),
               collected = tolower(trimws(collected)),
               duplicate = as.integer(duplicate))
      
      collection(df)
      action_log(list())  
      
      # Update all the dropdowns
      update_dropdowns()
      
    }, error = function(e) {
      add_message(list(type = "error", text = paste("Upload failed:", e$message)))
    })
  })
  
  
  # =========================================================================
  # DROPDOWN UPDATES
  # =========================================================================
  
  update_dropdowns <- function() {
    df <- collection()
    req(df)
    
    # Player names (where card_description is not a generic type)
    non_player_keywords <- c("emblem", "logo", "stadium", "team photo", "city")
    players <- df %>%
      filter(!grepl(paste(non_player_keywords, collapse = "|"), 
                    card_description, ignore.case = TRUE)) %>%
      arrange(card_description)
    
    player_choices <- setNames(players$card_id, 
                               paste0(players$card_description, " (", players$card_id, ")"))
    updateSelectizeInput(session, "add_player_name", choices = player_choices, server = TRUE)
    
    # Non-player cards
    non_players <- df %>%
      filter(grepl(paste(non_player_keywords, collapse = "|"), 
                   card_description, ignore.case = TRUE)) %>%
      arrange(association, card_description)
    
    non_player_choices <- setNames(non_players$card_id,
                                   paste0(non_players$card_description, " - ", 
                                          non_players$association, " (", non_players$card_id, ")"))
    updateSelectizeInput(session, "add_nonplayer", choices = non_player_choices, server = TRUE)
    
    # Collected cards (for removal)
    collected <- df %>% filter(collected == "yes") %>% arrange(desc(card_id))
    
    if (nrow(collected) > 0) {
      collected_choices <- setNames(
        collected$card_id,
        paste0(collected$card_description, " (", collected$card_id, ")")
      )
    } else {
      collected_choices <- character(0)
    }
    
    updateSelectizeInput(
      session,
      "remove_card",
      choices = collected_choices,
      server = TRUE
    )
    
    
    # Cards with duplicates (for trading)
    with_dupes <- df %>% filter(duplicate > 0) %>% arrange(desc(duplicate))
    
    if (nrow(with_dupes) > 0) {
      dupe_choices <- setNames(
        with_dupes$card_id,
        paste0(
          with_dupes$card_description,
          " (", with_dupes$card_id, ") - ",
          with_dupes$duplicate,
          " dupes"
        )
      )
    } else {
      dupe_choices <- character(0)
    }
    
    updateSelectizeInput(
      session,
      "trade_card",
      choices = dupe_choices,
      server = TRUE
    )
    
    pages <- df %>%
      filter(main_set == "yes") %>%
      distinct(page_number) %>%
      arrange(page_number)
    
    updateSelectInput(
      session,
      "book_page",
      choices = page_labels,
      selected = as.character(pages$page_number[1])
    )
    
  }
  
  
  # =========================================================================
  # ADD CARD FUNCTIONS
  # =========================================================================
  
  add_card <- function(card_id) {
    df <- collection()
    card_id <- toupper(trimws(card_id))
    
    if (!card_id %in% df$card_id) {
      return(list(success = FALSE, msg = paste("Card not found:", card_id)))
    }
    
    row_idx <- which(df$card_id == card_id)
    current_collected <- df$collected[row_idx]
    current_dupes <- df$duplicate[row_idx]
    
    if (current_collected == "yes") {
      # Already have it - increment duplicate
      df$duplicate[row_idx] <- current_dupes + 1
      collection(df)
      return(list(success = TRUE, 
                  msg = paste0("✓ Added duplicate of ", card_id, 
                               " (now ", df$duplicate[row_idx], " dupes)")))
    } else {
      # First time collecting
      df$collected[row_idx] <- "yes"
      collection(df)
      return(list(success = TRUE, 
                  msg = paste0("✓ Collected ", card_id, " (", df$card_description[row_idx], ")")))
    }
  }
  
  # Add by Card ID
  observeEvent(input$btn_add_by_id, {
    req(collection(), input$add_card_id)
    result <- add_card(input$add_card_id)
    add_message(list(type = ifelse(result$success, "success", "error"), text = result$msg))
    if (result$success) {
      updateTextInput(session, "add_card_id", value = "")
      update_dropdowns()
    }
  })
  
  # Add by Player Name
  observeEvent(input$btn_add_by_name, {
    req(collection(), input$add_player_name)
    result <- add_card(input$add_player_name)
    add_message(list(type = ifelse(result$success, "success", "error"), text = result$msg))
    if (result$success) update_dropdowns()
  })
  
  # Add Non-Player
  observeEvent(input$btn_add_nonplayer, {
    req(collection(), input$add_nonplayer)
    result <- add_card(input$add_nonplayer)
    add_message(list(type = ifelse(result$success, "success", "error"), text = result$msg))
    if (result$success) update_dropdowns()
  })
  
  # Bulk Add
  observeEvent(input$btn_bulk_add, {
    req(collection(), input$bulk_add_ids)
    
    ids <- strsplit(input$bulk_add_ids, "[,\\s]+")[[1]]
    ids <- ids[ids != ""]
    
    results <- lapply(ids, add_card)
    successes <- sum(sapply(results, function(x) x$success))
    failures <- length(results) - successes
    
    msg <- paste0("Added ", successes, " cards")
    if (failures > 0) msg <- paste0(msg, " (", failures, " failed)")
    
    add_message(list(type = ifelse(failures == 0, "success", "warning"), text = msg))
    updateTextAreaInput(session, "bulk_add_ids", value = "")
    update_dropdowns()
  })
  
  
  # =========================================================================
  # MANAGE CARDS (REMOVE / TRADE)
  # =========================================================================
  
  # Remove card (undo mistake)
  observeEvent(input$btn_remove, {
    req(collection(), input$remove_card)
    df <- collection()
    card_id <- input$remove_card
    
    row_idx <- which(df$card_id == card_id)
    df$collected[row_idx] <- "no"
    df$duplicate[row_idx] <- 0
    
    collection(df)
    manage_message(list(type = "success", 
                        text = paste0("✗ Removed ", card_id, " from collection")))
    update_dropdowns()
  })
  
  # Trade out duplicate
  observeEvent(input$btn_trade, {
    req(collection(), input$trade_card)
    df <- collection()
    card_id <- input$trade_card
    
    row_idx <- which(df$card_id == card_id)
    df$duplicate[row_idx] <- max(0, df$duplicate[row_idx] - 1)
    
    collection(df)
    manage_message(list(type = "success", 
                        text = paste0("Traded 1x ", card_id, 
                                      " (", df$duplicate[row_idx], " dupes remaining)")))
    update_dropdowns()
  })
  
  
  # =========================================================================
  # FEEDBACK DISPLAYS
  # =========================================================================
  
  output$add_feedback <- renderUI({
    msg <- add_message()
    req(msg)
    div(class = paste("feedback", msg$type), msg$text)
  })
  
  output$manage_feedback <- renderUI({
    msg <- manage_message()
    req(msg)
    div(class = paste("feedback", msg$type), msg$text)
  })
  
  
  # =========================================================================
  # STATS DISPLAY
  # =========================================================================
  
  output$stats_display <- renderUI({
    df <- collection()
    
    if (is.null(df)) {
      return(p("Upload a file to see stats"))
    }
    
    total <- nrow(df)
    collected <- sum(df$collected == "yes")
    main_set_total <- sum(df$main_set == "yes", na.rm = TRUE)
    main_set_collected <- sum(df$collected == "yes" & df$main_set == "yes", na.rm = TRUE)
    total_dupes <- sum(df$duplicate, na.rm = TRUE)
    
    pct <- round(collected / total * 100, 1)
    main_pct <- if(main_set_total > 0) round(main_set_collected / main_set_total * 100, 1) else 0
    
    div(class = "stats-grid",
        div(class = "stat-item",
            span(class = "stat-value", paste0(collected, "/", total)),
            span(class = "stat-label", paste0("Total (", pct, "%)"))
        ),
        div(class = "stat-item",
            span(class = "stat-value", paste0(main_set_collected, "/", main_set_total)),
            span(class = "stat-label", paste0("Main Set (", main_pct, "%)"))
        ),
        div(class = "stat-item",
            span(class = "stat-value", total_dupes),
            span(class = "stat-label", "Duplicates")
        )
    )
  })
  
  
  # =========================================================================
  # STICKER BOOK VISUALISATION
  # =========================================================================
  
  output$sticker_book_page <- renderUI({
    df <- collection()
    req(df, input$book_page)
    
    page_cards <- df %>%
      filter(
        main_set == "yes",
        page_number == input$book_page
      ) %>%
      arrange(as.numeric(gsub("\\D+", "", card_id)))
    
    if (nrow(page_cards) == 0) {
      return(p("No cards on this page"))
    }
    
    sticker_boxes <- lapply(1:nrow(page_cards), function(i) {
      card <- page_cards[i, ]
      
      # Determine CSS class
      status_class <- if (card$collected == "yes") "collected" else "missing"
      if (card$duplicate > 0) status_class <- paste(status_class, "has-dupes")
      
      div(class = paste("sticker-box", status_class),
          div(class = "sticker-content",
              if (card$collected == "yes") span(class = "check-mark", "✓") else span(class = "x-mark", "✗"),
              if (card$duplicate > 0) span(class = "dupe-badge", paste0("+", card$duplicate))
          ),
          div(class = "sticker-id", card$card_id),
          div(class = "sticker-name", 
              substr(card$card_description, 1, 12),
              if (nchar(card$card_description) > 12) "..." else "")
      )
    })
    
    div(class = "sticker-grid", sticker_boxes)
  })
  
  
  # =========================================================================
  # FULL COLLECTION TABLE
  # =========================================================================
  
  output$collection_table <- renderTable({
    df <- collection()
    req(df)
    
    # Apply filters
    if (input$filter_collected != "all") {
      df <- df %>% filter(collected == input$filter_collected)
    }
    if (input$filter_nation != "all") {
      df <- df %>% filter(nation == input$filter_nation)
    }
    
    df %>%
      select(card_id, card_description, association, collected, duplicate) %>%
      rename(
        "Card ID" = card_id,
        "Description" = card_description,
        "Association" = association,
        "Collected" = collected,
        "Duplicates" = duplicate
      )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  
  # =========================================================================
  # DOWNLOAD
  # =========================================================================
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("panini_collection_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv(collection(), file)
    }
  )
  
  
  
}
