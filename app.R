library(shiny)
library(tuneR)
library(seewave)
library(bslib)
library(DT)

# Setup persistent directory
audio_dir <- file.path(tempdir(), "audio_app")
if (!dir.exists(audio_dir)) dir.create(audio_dir)
addResourcePath("audio_files", audio_dir)

ui <- page_navbar(
  id = "nav_id",
  title = "Vocal Type Assessor",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("Review",
            layout_sidebar(
              sidebar = sidebar(
                width = 350,
                fileInput("files", "1. Upload .wav files", multiple = TRUE, accept = ".wav"),
                # Information text that will only show BEFORE files are uploaded
                uiOutput("pre_upload_info"),
                uiOutput("progress_ui"),
                hr(),
                uiOutput("side_controls") 
              ),
              mainPanel(
                uiOutput("main_content")
              )
            )
  ),
  nav_panel("Data Table", 
            div(class = "d-flex justify-content-between align-items-center mb-3",
                p("Click a row in the table to jump to that file's review.", class = "mb-0 text-muted"),
                uiOutput("download_btn_ui")
            ),
            DTOutput("results_table")
  )
)

server <- function(input, output, session) {
  current_idx <- reactiveVal(1)
  last_autosave <- reactiveVal(NULL) # Tracker for autosave time
  
  # Initialize data frame structure
  results_df <- reactiveVal(data.frame(
    Name = character(), 
    Category = character(), 
    Mix_Detail = character(), 
    Comment = character(),
    Transcription = character(),
    Translation = character(),
    Date_Reviewed = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$files, {
    # --- AUTOSAVE RESTORE LOGIC ---
    snapshot_file <- "vocal_review_snapshot.rds"
    
    if (file.exists(snapshot_file)) {
      snapshot <- readRDS(snapshot_file)
      # If the uploaded file names exactly match the snapshot's saved file names, restore memory
      if (identical(sort(input$files$name), sort(snapshot$files))) {
        results_df(snapshot$results)
        current_idx(snapshot$idx)
        showNotification("Previous session memory restored successfully!", type = "message", duration = 5)
        return() # Exit here so we don't overwrite with a fresh table
      }
    }
    
    # Otherwise, start a fresh session (Preserving original file names with #)
    df <- data.frame(
      Name = input$files$name, 
      Category = "Pending", # Default for forced choice
      Mix_Detail = "", 
      Comment = "",
      Transcription = "",
      Translation = "",
      Date_Reviewed = "",
      stringsAsFactors = FALSE
    )
    results_df(df)
    current_idx(1)
  })
  
  # --- SAFE DATAPATH LOOKUP ---
  # Ensures we get the correct audio even if the OS re-uploads files in a different order
  get_current_datapath <- function() {
    req(input$files, current_idx() <= nrow(results_df()))
    target_filename <- results_df()$Name[current_idx()]
    match_idx <- match(target_filename, input$files$name)
    if (is.na(match_idx)) return(NULL)
    return(input$files$datapath[match_idx])
  }
  
  # --- DYNAMIC INSTRUCTIONS ---
  output$pre_upload_info <- renderUI({
    # Only show this text if no files have been uploaded yet
    if (is.null(input$files)) {
      helpText("To resume a previous session, simply re-upload the exact same .wav files. The app will automatically restore your saved progress.", 
               style = "font-size: 0.85em; color: #6c757d; margin-top: -10px; margin-bottom: 5px;")
    }
  })
  
  # --- PROGRESS BAR LOGIC ---
  output$progress_ui <- renderUI({
    req(input$files)
    total <- nrow(results_df())
    current <- min(current_idx(), total)
    perc <- round((current / total) * 100)
    
    tagList(
      p(paste("Progress:", current, "of", total, "files"), style = "margin-bottom: 5px; font-weight: bold;"),
      div(class = "progress", style = "height: 20px;",
          div(class = "progress-bar progress-bar-striped bg-info", 
              role = "progressbar", 
              style = paste0("width: ", perc, "%;"),
              paste0(perc, "%"))
      )
    )
  })
  
  # --- NAVIGATION LOGIC ---
  observeEvent(input$results_table_rows_selected, {
    req(input$results_table_rows_selected)
    idx <- input$results_table_rows_selected
    
    # Save current state before switching
    save_current_row()
    
    current_idx(idx)
    update_inputs_with_row_data(idx)
    nav_select("nav_id", "Review")
  })
  
  # --- DYNAMIC UI LOGIC ---
  output$main_content <- renderUI({
    if (is.null(input$files)) {
      div(class = "text-center mt-5", 
          h3("Welcome!"), 
          p("Please select your .wav files using the sidebar to begin the review process."))
    } else if (current_idx() > nrow(results_df())) {
      tagList(
        div(class = "text-center my-5",
            h2("Review Complete!"),
            p("All files have been processed. You can download your CSV report below."),
            p("You can upload new files to start a new review cycle."),
            uiOutput("complete_download_ui")
        )
      )
    } else {
      tagList(
        h4(textOutput("current_filename")),
        plotOutput("oscillo_plot", height = "250px"),
        plotOutput("spectro_plot", height = "450px")
      )
    }
  })
  
  output$side_controls <- renderUI({
    if (!is.null(input$files) && current_idx() <= nrow(results_df())) {
      cat_val <- results_df()$Category[current_idx()]
      # If Category is "Pending", show no pre-selected option
      selected_cat <- if (is.na(cat_val) || cat_val == "Pending" || cat_val == "") character(0) else cat_val
      
      btns <- tagList(
        uiOutput("audio_container"),
        hr(),
        radioButtons("category", "Select Type:", 
                     choices = c("Verbal", "Non-verbal", "Interjection", "Mix"),
                     selected = selected_cat),
        
        conditionalPanel(
          condition = "input.category == 'Mix'",
          textInput("mix_detail", "Explanation (Required for Mix):", value = results_df()$Mix_Detail[current_idx()])
        ),
        
        textInput("comment", "Comment:", value = results_df()$Comment[current_idx()]),
        textInput("transcription", "Transcription:", value = results_df()$Transcription[current_idx()]),
        textInput("translation", "Translation:", value = results_df()$Translation[current_idx()]),
        
        div(class = "d-flex gap-2 mb-2",
            actionButton("save_btn", "Save & Next", class = "btn-success flex-fill")
        )
      )
      
      if (current_idx() > 1) {
        btns <- tagAppendChild(btns, 
                               div(class = "d-flex gap-2",
                                   actionButton("back", "Previous", class = "btn-secondary flex-fill")
                               )
        )
      }
      
      # --- COMPACT AUTOSAVE INDICATOR MOVED TO THE BOTTOM ---
      if (!is.null(last_autosave())) {
        btns <- tagAppendChild(btns,
                               div(paste("✔ Autosaved at", format(last_autosave(), "%H:%M:%S")),
                                   style = "color: #198754; font-size: 0.85em; text-align: center; margin-top: 15px;")
        )
      }
      
      return(btns)
    }
  })
  
  output$audio_container <- renderUI({
    req(input$files)
    path <- get_current_datapath()
    req(path)
    
    # Adding a unique timestamp ID to the filename prevents the browser from caching old audio
    unique_id <- as.integer(Sys.time())
    filename <- paste0("temp_", current_idx(), "_", unique_id, ".wav")
    file.copy(path, file.path(audio_dir, filename), overwrite = TRUE)
    
    tags$audio(src = paste0("audio_files/", filename), type = "audio/wav", controls = TRUE, autoplay = TRUE)
  })
  
  # --- HELPER FUNCTIONS ---
  save_current_row <- function() {
    df <- results_df()
    idx <- current_idx()
    chosen_cat <- if (is.null(input$category) || length(input$category) == 0) "Pending" else input$category
    df$Category[idx] <- chosen_cat
    df$Mix_Detail[idx] <- if(!is.null(input$mix_detail)) input$mix_detail else ""
    df$Comment[idx] <- if(!is.null(input$comment)) input$comment else ""
    df$Transcription[idx] <- if(!is.null(input$transcription)) input$transcription else ""
    df$Translation[idx] <- if(!is.null(input$translation)) input$translation else ""
    df$Date_Reviewed[idx] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    results_df(df)
    
    # --- AUTOSAVE CAPTURE ---
    if (!is.null(input$files)) {
      snapshot <- list(
        results = df,
        idx = current_idx(),
        files = input$files$name
      )
      saveRDS(snapshot, "vocal_review_snapshot.rds")
      last_autosave(Sys.time())
    }
  }
  
  update_inputs_with_row_data <- function(idx) {
    row <- results_df()[idx, ]
    cat_sel <- if (is.na(row$Category) || row$Category == "Pending" || row$Category == "") character(0) else row$Category
    updateRadioButtons(session, "category", selected = cat_sel)
    updateTextInput(session, "mix_detail", value = row$Mix_Detail)
    updateTextInput(session, "comment", value = row$Comment)
    updateTextInput(session, "transcription", value = row$Transcription)
    updateTextInput(session, "translation", value = row$Translation)
  }
  
  # --- EVENT HANDLING ---
  observeEvent(input$save_btn, {
    # Forced choice validation: do not allow proceeding if no category is selected
    if (is.null(input$category) || length(input$category) == 0 || input$category == "" || input$category == "Pending") {
      showNotification("Please select a category before proceeding.", type = "warning")
      return()
    }
    
    # Mix explanation validation
    if (input$category == 'Mix' && (is.null(input$mix_detail) || trimws(input$mix_detail) == "")) {
      showNotification("Please provide an explanation for 'Mix' category.", type = "error")
      return()
    }
    
    save_current_row()
    
    if(current_idx() <= nrow(results_df())) {
      current_idx(current_idx() + 1)
      if(current_idx() <= nrow(results_df())) {
        update_inputs_with_row_data(current_idx())
      }
    }
  })
  
  observeEvent(input$back, {
    save_current_row()
    if(current_idx() > 1) {
      current_idx(current_idx() - 1)
      update_inputs_with_row_data(current_idx())
    }
  })
  
  output$oscillo_plot <- renderPlot({
    req(input$files, current_idx() <= nrow(results_df()))
    path <- get_current_datapath()
    req(path)
    
    wave <- readWave(path)
    if (wave@stereo) wave <- mono(wave, "left")
    plot(wave, main = "Oscillogram")
  })
  
  # --- SPECTROGRAM (0-8 kHz SAFE RENDER) ---
  output$spectro_plot <- renderPlot({
    req(input$files, current_idx() <= nrow(results_df()))
    path <- get_current_datapath()
    req(path)
    
    wave <- readWave(path)
    
    # 1. Convert stereo wave to mono (prevents indexing subscript errors)
    if (wave@stereo) wave <- mono(wave, "left")
    
    # 2. Downsample to 16 kHz so Nyquist limit is natively 8 kHz
    if (wave@samp.rate > 16000) {
      wave <- resamp(wave, f = wave@samp.rate, g = 16000, output = "Wave")
    }
    
    # 3. Dynamic window length calculation for short audio clips
    n_samples <- length(wave@left)
    wl_val <- min(1024, n_samples)
    if (wl_val %% 2 != 0) wl_val <- wl_val - 1
    if (wl_val < 16) wl_val <- 16
    
    spectro(wave, f = wave@samp.rate, wl = wl_val, ovlp = 50, scale = TRUE, 
            dB = "max0", palette = reverse.gray.colors.1, collevels = seq(-100, 0, 10),
            main = "High-Depth Spectrogram (0-8 kHz)")
  })
  
  output$results_table <- renderDT({ 
    datatable(results_df(), selection = 'single')
  })
  
  output$current_filename <- renderText({ 
    req(input$files)
    if(current_idx() <= nrow(results_df())) {
      results_df()$Name[current_idx()]
    }
  })
  
  # --- HASH-SAFE & UTF-8 MULTI-LANGUAGE CSV EXPORT HANDLERS ---
  get_csv_uri <- function() {
    df <- results_df()
    con <- textConnection(NULL, "w", encoding = "UTF-8")
    write.csv(df, con, row.names = FALSE, fileEncoding = "UTF-8")
    csv_text <- textConnectionValue(con)
    close(con)
    csv_string <- paste(csv_text, collapse = "\n")
    # Prepend UTF-8 BOM (\xEF\xBB\xBF) for international characters (French, Japanese, Mandarin, Turkish, etc.)
    utf8_csv <- paste0("\xEF\xBB\xBF", csv_string)
    # reserved = TRUE ensures # in filenames is encoded as %23 for browser data transfer, keeping original # intact
    paste0("data:text/csv;charset=utf-8,", URLencode(utf8_csv, reserved = TRUE))
  }
  
  output$download_btn_ui <- renderUI({
    uri <- get_csv_uri()
    filename <- paste0("Review_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    tags$a("Export Table to CSV", href = uri, download = filename, class = "btn btn-primary")
  })
  
  output$complete_download_ui <- renderUI({
    uri <- get_csv_uri()
    filename <- paste0("Review_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    tags$a("Download CSV Report", href = uri, download = filename, class = "btn btn-success btn-lg mt-3")
  })
}

shinyApp(ui, server)