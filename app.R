library(shiny)
library(shinyMobile)
library(uuid)
library(DBI)
library(RPostgres)
library(highcharter)
library(shinyjs)

db_conn <- dbConnect(RPostgres::Postgres(), 
                     dbname = "mcabiashara",
                     host = "dpg-d1qmg2odl3ps73935t10-a.oregon-postgres.render.com",
                     port = 5432,
                     user = "mcabiashara_user",
                     password = "HarqQXyeRZ81xkpJF0rf20f2JJ0SnFkZ")

dbExecute(db_conn, "
CREATE TABLE IF NOT EXISTS pigakura (
  id SERIAL PRIMARY KEY,
  uuid TEXT UNIQUE NOT NULL,
  title TEXT NOT NULL,
  contestants TEXT[] NOT NULL,
  votes TEXT[] DEFAULT '{}',
  expiry DATE,
  created_at TIMESTAMP DEFAULT NOW()
);
;")


ui <- f7Page(
  useShinyjs(),
  
  title = "PigaKura 🇰🇪",
  options = list(
    theme = "ios",
    dark = TRUE,
    skeletonsOnLoad = TRUE
  ),
  
  
  tags$head(
    tags$style(HTML("
      .navbar .left-align-title {
        text-align: left !important;
        margin-left: 16px;
        width: 100%;
        font-weight: bold;
      }
      .navbar .title {
        text-align: left !important;
      }
      .option-input-wrapper {
        display: flex;
        align-items: center;
        margin-bottom: 12px;
      }
      .option-input-wrapper input[type='text'] {
        flex-grow: 1;
        min-width: 250px;
        max-width: 100%;
        padding-right: 0 !important;
      }
      .delete-inside-btn {
        background: none;
        border: none;
        color: red;
        font-weight: bold;
        font-size: 20px;
        cursor: pointer;
        line-height: 1;
        padding: 0 6px;
        user-select: none;
        margin-left: 4px;
      }
      .delete-inside-btn:hover {
        color: darkred;
      }
      .search-input {
        font-size: 18px;
        padding-left: 30px;
        background-image: url('data:image/svg+xml;utf8,%3Csvg fill=\"%23666\" height=\"20\" viewBox=\"0 0 24 24\" width=\"20\" xmlns=\"http://www.w3.org/2000/svg\"%3E%3Cpath d=\"M15.5 14h-.79l-.28-.27A6.471 6.471 0 0016 9.5 6.5 6.5 0 109.5 16a6.471 6.471 0 004.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zM9.5 14C7 14 5 12 5 9.5S7 5 9.5 5 14 7 14 9.5 12 14 9.5 14z\"/%3E%3C/svg%3E');
        background-repeat: no-repeat;
        background-position: 6px center;
        border-radius: 8px;
      }
      
       #search_uuid {
      border: 2px solid #ccc;     /* visible border */
      border-radius: 12px;        /* curved edges */
      padding: 8px 12px;          /* some padding for nicer look */
      outline: none;              /* remove default outline */
    }
    #search_uuid:focus {
      border-color: #007aff;      /* blue border on focus (iOS blue) */
      box-shadow: 0 0 5px #007aff;
    }

    /* Contestant images container */
    .contestant-images-container {
      display: flex;
      align-items: center;
      padding-left: 16px;
      margin-bottom: 12px;
      overflow-x: auto;
      gap: 16px;
    }
    .contestant-block {
      width: 120px;
      text-align: left;
      flex-shrink: 0;
    }
    .contestant-block img {
      width: 80px;
      height: 80px;
      object-fit: cover;
      border-radius: 50%;
      border: 2px solid #ccc;
      margin-bottom: 6px;
      display: block;
    }
    .contestant-name {
      font-weight: bold;
      font-size: 14px;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .contestant-percent {
      font-size: 12px;
      color: #bbb;
    }
    "))
  ),
  tags$head(
    # Load Open Sans font and custom CSS
    HTML("
      <style>
        @font-face {
          font-family: 'Open Sans';
          font-style: normal;
          font-weight: 300;
          src: local('Open Sans Light'), local('OpenSans-Light'),
          url(https://fonts.gstatic.com/s/opensans/v17/mem5YaGs126MiZpBA-UN_r8OUuhs.ttf) format('truetype');
        }
        .ip {
          font-size: 16px;
          color: #F3544D;
          text-align: center;
        }
        .ip p {
          margin: 0;
          color: #1493A5;
          -webkit-user-select: none;
          -moz-user-select: none;
          -ms-user-select: none;
          user-select: none;
          cursor: default;
        }
        #ip-address {
          font-size: 25px;
          font-size: 4vw;
        }
      </style>
    "),
    
    tags$head(
      tags$script(HTML("
    $(document).ready(function() {
      $.getJSON('https://jsonip.com?callback=?', function (data) {
        $('#ip-address').text(data.ip);  // optional display
        Shiny.setInputValue('user_ip', data.ip, {priority: 'event'});
      });
    });
  "))
    )
    
    
  ),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('openSheet', function(data) {
    const sheetEl = document.querySelector('#details_sheet');
    if (!sheetEl) return;

    // Initialize the sheet if not already initialized
    if (!sheetEl.f7Sheet) {
      sheetEl.f7Sheet = app.sheet.create({
        el: '#details_sheet',
        swipeToClose: true,
        backdrop: true
      });
    }

    // Open it
    sheetEl.f7Sheet.open();
  });
")),
  tags$script(HTML("
  let countdownInterval;
  function startCountdown(targetDate) {
    clearInterval(countdownInterval);
    countdownInterval = setInterval(function() {
      const now = new Date().getTime();
      const distance = new Date(targetDate).getTime() - now;

      if (distance < 0) {
        document.getElementById('countdown_timer').innerText = 'EXPIRED';
        clearInterval(countdownInterval);
        return;
      }

      const days = Math.floor(distance / (1000 * 60 * 60 * 24));
      const hours = Math.floor((distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
      const minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
      const seconds = Math.floor((distance % (1000 * 60)) / 1000);

      document.getElementById('countdown_timer').innerText =
        `${days} Day(s): ${hours} Hr(s): ${minutes} Min(s): ${seconds} Sec(s)`;
    }, 1000);
  }
"))
  
  
  ,
  
  f7TabLayout(
    navbar = f7Navbar(
      title = tags$div(class = "left-align-title", "PigaKura 🇰🇪")
    ),
    
    f7Tabs(
      id = "main_tabs",
      animated = TRUE,
      swipeable = FALSE,
      
      f7Tab(
        tabName = "Vote",
        title = "Vote",
        icon = f7Icon("checkmark_circle"),
        active = TRUE,
        
        # Countdown To Poll Expiry
        tags$head(
          tags$link(
            href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&display=swap",
            rel = "stylesheet"
          ),
          tags$style(HTML("
  #countdown_title {
    color: #CCCCCC; /* Dark black-grey for visibility on dark mode */
    font-size: 20px;
    font-weight: bold;
    text-align: center;
    font-family: 'Orbitron', sans-serif;
    margin-top: 20px;
  }

  #countdown_timer {
    color: #00FF00;
    font-size: 17px;
    font-weight: bold;
    text-align: center;
    font-family: 'Orbitron', sans-serif;
    margin-top: 10px;
  }
"))
        ),
        
        div(id = "countdown_title", "Countdown To Poll Expiry"),
        div(id = "countdown_timer", textOutput("countdown", inline = TRUE)),
        
        tags$div(
          uiOutput("contestant_images_ui", style = "margin-top: 24px;"),
          
          f7Sheet(
            id = "details_sheet",
            label = "Contestant Details",
            swipeToClose = TRUE,
            swipeHandler = TRUE,
            backdrop = TRUE,
            uiOutput("contestant_detail_ui")
          )
          ,
          
          h3("VOTE", style = "text-align: center; font-style: italic; font-size: 28px; margin: 16px 0;"),
          
          f7Card(
            raised = TRUE,
            outline = TRUE,
            uiOutput("contestants_ui")
          ),
          
          uiOutput("results_ui"),
          
          # 👇 Footer image
          tags$div(
            style = "text-align: center; margin-top: 32px; margin-bottom: 16px;",
            tags$img(
              src = "https://fra.cloud.appwrite.io/v1/storage/buckets/6736d84d000911ff64ee/files/6869379f003dd31cc2fe/view?project=6736d8350038b9ef3202&mode=admin",
              style = "width: 80%; max-width: 320px; height: auto;"
            )
          )
          
        )
      )
      ,
      
      f7Tab(
        tabName = "Create_Poll",
        title = "Create Poll",
        icon = f7Icon("square_list"),
        
        tags$div(
          tags$h3("Create a Poll", style = "margin-top: 24px; margin-bottom: 0; margin-left: 16px;"),
          
          tags$div(
            style = "margin-top: 0; margin-bottom: 16px; margin-left: 16px;",
            "Complete the below fields to create your poll."
          )
        ),
        
        f7Card(
          raised = TRUE,
          outline = TRUE,
          tags$div(style = "font-weight: 600; margin-bottom: 8px;", "Title"),
          f7Text(inputId = "poll_title", label = NULL, placeholder = "Type your question here"),
          
          tags$br(),
          
          tags$div(style = "font-weight: 600; margin-top: 15px; margin-bottom: 8px;", "Answer Options"),
          uiOutput("answer_ui"),
          
          # New text input for user name below answer_ui
          tags$div(style = "font-weight: 600; margin-top: 20px; margin-bottom: 8px;", "Enter user name:"),
          f7Text(inputId = "user_name", label = NULL, placeholder = "Your name here"),
          
          tags$br(),
          
          f7Button(inputId = "add_option", label = "+ Add another option", color = "blue", rounded = TRUE),
          f7DatePicker(inputId = "expiry", label = "Select Expiry Date"),
          
          tags$br(),
          f7Button(inputId = "create_poll", label = "Create Poll", color = "green", rounded = TRUE)
        )
      )
      
    )
  )
)




server <- function(input, output, session) {
  
  output$countdown <- renderText({
    "Loading..."
  })
  
  observe({
    poll_data <- dbGetQuery(db_conn, "
    SELECT expiry FROM pigakura
    WHERE id = $1
    LIMIT 1
  ", params = list(1))
    
    req(nrow(poll_data) > 0)
    
    print(poll_data$expiry[1])  # For debugging
    
    expiry_datetime <- format(as.POSIXct(poll_data$expiry[1]), "%Y-%m-%dT%H:%M:%S")
    runjs(sprintf("startCountdown('%s')", expiry_datetime))
  })
  
  
  ## ---------- shared reactives / helpers ----------
  selected_contestant_index <- reactiveVal(1)
  contestant_info <- reactiveValues(names = NULL, votes = NULL, pct = NULL)
  
  fixed_poll_id <- 1          # which poll to show
  page_state    <- reactiveVal("vote")
  options_ids   <- reactiveVal(c(1, 2))
  
  # map names → image URLs (used in two places)
  img_urls <- c(
    "Yes"       = "https://fra.cloud.appwrite.io/v1/storage/buckets/674080b000393efd837f/files/68756b17003e7f52c0b5/view?project=6736d8350038b9ef3202&mode=admin",
    "No"  = "https://fra.cloud.appwrite.io/v1/storage/buckets/674080b000393efd837f/files/68756b17003e7f52c0b5/view?project=6736d8350038b9ef3202&mode=admin"
  )
  
  generate_short_id <- function(n = 8) paste0(sample(c(0:9, letters, LETTERS), n, TRUE), collapse = "")
  
  ## ---------- UI for adding poll options ----------
  observeEvent(input$add_option, {
    ids <- options_ids()
    options_ids(c(ids, max(ids) + 1))
  })
  
  observe({
    lapply(options_ids(), function(id) {
      observeEvent(input[[paste0("del_", id)]], {
        options_ids(setdiff(options_ids(), id))
      }, ignoreInit = TRUE)
    })
  })
  
  output$answer_ui <- renderUI({
    tagList(lapply(seq_along(options_ids()), function(i) {
      id <- options_ids()[i]
      div(class = "option-row",
          div(class = "option-input-wrapper",
              f7Text(inputId = paste0("option_", id), label = NULL, placeholder = paste("Option", i)),
              actionButton(inputId = paste0("del_", id), label = "×",
                           class = "delete-inside-btn", title = "Delete this option")))
    }))
  })
  
  ## ---------- helper used several times ----------
  current_poll <- reactive({
    poll <- dbGetQuery(db_conn, "SELECT title, contestants FROM pigakura WHERE id = $1 LIMIT 1",
                       params = list(fixed_poll_id))
    if (!nrow(poll)) return(NULL)
    list(title = poll$title[1],
         contestants = strsplit(gsub("[{}\"]", "", poll$contestants[1]), ",")[[1]])
  })
  
  output$poll_title <- renderText({
    req(current_poll())
    current_poll()$title
  })
  
  ## ---------- contestants radio list (voting) ----------
  output$contestants_ui <- renderUI({
    poll_data <- dbGetQuery(db_conn, "
      SELECT title, contestants, votes FROM pigakura WHERE id = $1 LIMIT 1",
                            params = list(fixed_poll_id))
    if (!nrow(poll_data)) return(tags$div("No polls available in the database."))
    
    contestants <- strsplit(gsub("[{}\"]", "", poll_data$contestants[1]), ",")[[1]]
    
    tagList(
      # Title block
      tags$div(
        style = "font-size:18px; margin-bottom:8px;",
        tagList(
          tags$div("I SUPPORT", style = "font-weight:bold; text-align:center;"),
          tags$div("Kimani Nduta. RE-ELECTION 2027", style = "font-weight:bold; text-align:left;")
        )
      ),
      
      
      # Subheading
      tags$div(style = "margin-bottom:12px;", HTML("<i>Make a choice:</i>")),
      
      # Radio buttons
      tags$div(
        style = "margin-bottom:16px;",
        lapply(seq_along(contestants), function(i) {
          name <- contestants[i]
          inputId <- paste0("vote_option_", i)
          tags$div(
            style = "display:flex; align-items:center; margin-bottom:14px;",
            tags$input(
              type = "radio", name = "vote_choice", value = name, id = inputId,
              style = "width:20px; height:20px;",
              onchange = sprintf("Shiny.setInputValue('vote_choice', '%s')", name)
            ),
            tags$label(
              `for` = inputId,
              style = "margin-left:12px; font-size:18px; font-weight:500;",
              name
            )
          )
        })
      ),
      
      # Submit button
      f7Button("submit_vote", "Vote", color = "blue")
    )
    
  })
  
  ## ---------- contestant thumbnails + “Show More” ----------
  output$contestant_images_ui <- renderUI({
    invalidateLater(5000, session)
    
    poll_data <- dbGetQuery(db_conn, "
    SELECT contestants, votes FROM pigakura WHERE id = $1 LIMIT 1",
                            params = list(fixed_poll_id)
    )
    req(nrow(poll_data))
    
    contestants <- strsplit(gsub("[{}\"]", "", poll_data$contestants[1]), ",")[[1]]
    votes_raw   <- strsplit(gsub("[{}\"]", "", poll_data$votes[1]), ",")[[1]]
    votes_clean <- votes_raw[votes_raw != "" & votes_raw != "0"]
    
    cand <- sapply(votes_clean, function(v) {
      p <- strsplit(v, " - ")[[1]]; if (length(p) == 2) trimws(p[2]) else NA
    })
    cand <- cand[!is.na(cand)]
    
    vote_counts <- sapply(contestants, function(nm) sum(cand == nm))
    if (!sum(vote_counts)) return(NULL)
    
    idx <- order(vote_counts, decreasing = TRUE)
    contestant_info$names <- contestants[idx]
    contestant_info$votes <- vote_counts[idx]
    contestant_info$pct   <- round(contestant_info$votes / sum(vote_counts) * 100)
    
    # Inject CSS only once
    insertUI(selector = "head", where = "beforeEnd", ui = tags$style(HTML("
    .contestant-images-container {
      display: flex;
      flex-wrap: wrap;
      justify-content: center;
      gap: 16px;
      margin-top: 16px;
      padding: 0 12px;
    }
    .contestant-block {
      flex: 0 1 40%;
      max-width: 150px;
      text-align: center;
      margin-bottom: 20px;
    }
    .contestant-block img {
      width: 100%;
      height: auto;
      border-radius: 12px;
      margin-bottom: 6px;
    }
    .contestant-name {
      font-weight: bold;
      font-size: 16px;
      margin-bottom: 2px;
    }
    .contestant-percent {
      font-size: 14px;
      font-weight: bold;
      font-style: italic;
      color: white;
      background-color: #444;
      padding: 4px 8px;
      border-radius: 6px;
      display: inline-block;
      margin-top: 4px;
    }
  ")))
    
    tags$div(
      class = "contestant-images-container",
      lapply(seq_along(contestant_info$names), function(i) {
        nm  <- contestant_info$names[i]
        pct <- contestant_info$pct[i]
        url <- img_urls[[nm]]
        tags$div(
          class = "contestant-block",
          if (!is.null(url))
            tags$img(src = url, alt = nm)
          else
            tags$div(style = "width:100%; height:100px; background:#555; margin-bottom:6px;"),
          tags$div(class = "contestant-name", nm),
          tags$div(class = "contestant-percent", paste0(pct, "%"))
        )
      }),
      tags$div(
        style = "text-align:center; margin-top:20px;",
        actionLink("show_more", "Show More »",
                   style = "font-size:18px; font-weight:bold; color:#007aff;")
      )
    )
  })
  
  
  
  ## ---------- sheet navigation ----------
  observeEvent(input$show_more, {
    selected_contestant_index(1)
    session$sendCustomMessage("openSheet", list(id = "details_sheet"))
  })
  
  observeEvent(input$prev_contestant, {
    cur <- selected_contestant_index()
    if (!is.null(contestant_info$names) && cur > 1)
      selected_contestant_index(cur - 1)
  })
  
  observeEvent(input$next_contestant, {
    cur <- selected_contestant_index()
    if (!is.null(contestant_info$names) && cur < length(contestant_info$names))
      selected_contestant_index(cur + 1)
  })
  
  ## ---------- sheet body ----------
  output$contestant_detail_ui <- renderUI({
    req(contestant_info$names)
    i <- selected_contestant_index()
    if (i > length(contestant_info$names)) return(NULL)
    
    nm   <- contestant_info$names[i]
    vts  <- contestant_info$votes[i]
    pct  <- contestant_info$pct[i]
    url  <- img_urls[[nm]]
    
    poll <- current_poll()
    poll_title <- if (!is.null(poll)) poll$title else "Poll"
    
    tagList(
      tags$div(style = "margin-top:10px; padding: 0 12px;",
               
               # Bold italic poll title above image, left-aligned
               tags$div(
                 style = "font-style: italic; font-weight: bold; text-align: left; font-size: 16px; margin-bottom: 10px;",
                 poll_title
               ),
               
               # Contestant image
               tags$img(
                 src = url,
                 style = "width: 100%; max-width: 220px; height: auto; display: block; margin: 0 auto 10px auto;"
               ),
               
               # Contestant name
               tags$div(style = "font-weight:bold; font-size:18px; text-align:center;", nm),
               
               # Quick Statistics section
               tags$h3(style = "font-style: italic; font-weight: bold; text-align: left; margin-top: 20px;", "Quick Statistics"),
               tags$ul(style = "font-size: 16px; padding-left: 20px;",
                       tags$li(paste("Votes:", vts)),
                       tags$li(paste("Percentage:", pct, "%")),
                       tags$li(paste("Rank:", i))
               ),
               
               # Navigation buttons
               tags$div(style = "margin-top:16px; text-align:center;",
                        f7Button("prev_contestant", "<<", fill = FALSE),
                        f7Button("next_contestant", ">>", fill = FALSE)
               )
      )
    )
  })
  
  
  
  
  
  
  selected_contestant_index <- reactiveVal(1)
  
  
  fixed_poll_id <- 1  # manually control which poll is active
  
  page_state <- reactiveVal("vote")
  
  observeEvent(input$show_results, {
    page_state("results")
  })
  
  options_ids <- reactiveVal(c(1, 2))
  
  # Generate short 8-character poll ID
  generate_short_id <- function(n = 8) {
    paste0(sample(c(0:9, letters, LETTERS), n, replace = TRUE), collapse = "")
  }
  
  observeEvent(input$add_option, {
    ids <- options_ids()
    new_id <- if (length(ids) == 0) 1 else max(ids) + 1
    options_ids(c(ids, new_id))
  })
  
  observe({
    ids <- options_ids()
    lapply(ids, function(id) {
      observeEvent(input[[paste0("del_", id)]], {
        ids_new <- setdiff(options_ids(), id)
        options_ids(ids_new)
      }, ignoreInit = TRUE)
    })
  })
  
  output$answer_ui <- renderUI({
    ids <- options_ids()
    tagList(
      lapply(seq_along(ids), function(i) {
        id <- ids[i]
        div(
          class = "option-row",
          div(
            class = "option-input-wrapper",
            f7Text(
              inputId = paste0("option_", id),
              label = NULL,
              placeholder = paste("Option", i)
            ),
            actionButton(
              inputId = paste0("del_", id),
              label = "×",
              class = "delete-inside-btn",
              title = "Delete this option"
            )
          )
        )
      })
    )
  })
  
  
  
  
  current_poll <- reactive({
    poll <- dbGetQuery(db_conn, "
    SELECT title, contestants FROM pigakura WHERE id = $1 LIMIT 1
  ", params = list(fixed_poll_id))
    
    if (nrow(poll) == 0) return(NULL)
    
    contestants <- strsplit(gsub("[{}\"]", "", poll$contestants[1]), ",")[[1]]
    list(
      title = poll$title[1],
      contestants = contestants
    )
  })
  
  
  output$poll_title <- renderText({
    poll <- current_poll()
    if (is.null(poll)) return("Poll not found")
    poll$title
  })
  
  
  
  
  
  observeEvent(input$create_poll, {
    
    # ✅ Step 1: Validate username
    if (is.null(input$user_name) || input$user_name != "kamaugenz@254") {
      f7Toast(text = "Error: Invalid user name.", position = "center", closeTimeout = 3000)
      return()
    }
    
    # ✅ Step 2: Generate poll data
    poll_id <- generate_short_id()
    title <- input$poll_title
    selected_date <- input$expiry  # From f7DatePicker
    
    # ✅ Step 3: Append fixed time (19:00:00) to the selected date
    expiry_str <- paste0(selected_date, "T19:00:00")
    
    # ✅ Step 4: Collect poll options
    opts <- sapply(options_ids(), function(id) input[[paste0("option_", id)]])
    opts <- opts[!is.na(opts) & opts != ""]
    
    if (length(opts) < 2 || is.null(title) || title == "") {
      f7Toast(text = "Please enter a title and at least 2 options.", position = "center", closeTimeout = 3000)
      return()
    }
    
    # ✅ Step 5: Escape options for PostgreSQL array
    escape_pg_array_element <- function(x) {
      x <- gsub('"', '\\"', x, fixed = TRUE)
      paste0('"', x, '"')
    }
    
    pg_array <- paste0("{", paste(escape_pg_array_element(opts), collapse = ","), "}")
    empty_votes <- paste0("{", paste(rep("0", length(opts)), collapse = ","), "}")
    
    # ✅ Step 6: Save to database
    dbExecute(db_conn, "
      INSERT INTO pigakura (uuid, title, contestants, votes, expiry)
      VALUES ($1, $2, $3, $4, $5)
    ", params = list(
      poll_id,
      title,
      pg_array,
      empty_votes,
      expiry_str
    ))
    
    # ✅ Step 7: Notify user
    f7Toast(text = paste("Poll created! ID:", poll_id), position = "center", closeTimeout = 3000)
  })
  
  
  
  
  observeEvent(input$submit_vote, {
    user_ip <- input$user_ip  # From JavaScript
    
    if (is.null(user_ip) || user_ip == "") {
      f7Toast("⚠️ Could not detect your IP. Please check your connection.", position = "center", closeTimeout = 4000)
      return()
    }
    
    poll <- dbGetQuery(db_conn, "
    SELECT uuid, contestants, votes FROM pigakura WHERE id = $1 LIMIT 1
  ", params = list(fixed_poll_id))
    
    if (nrow(poll) == 0) {
      f7Toast("Poll not found.", position = "center", closeTimeout = 3000)
      return()
    }
    
    uuid <- poll$uuid[1]
    choice <- input$vote_choice
    
    if (is.null(choice) || choice == "") {
      f7Toast("Please select an option before voting", position = "center", closeTimeout = 3000)
      return()
    }
    
    # Fetch and clean existing votes
    votes_raw <- poll$votes[1]
    votes_vec <- if (is.null(votes_raw) || votes_raw == "{}" || votes_raw == "") {
      character(0)
    } else {
      cleaned <- strsplit(gsub("[{}\"]", "", votes_raw), ",")[[1]]
      if (length(cleaned) == 1 && cleaned == "") character(0) else cleaned
    }
    
    # Check if this IP has already voted
    ip_already_voted <- any(grepl(paste0("^", user_ip, " - "), votes_vec))
    
    if (ip_already_voted) {
      f7Toast("⚠️ You have already voted! ❌", position = "center", closeTimeout = 4000)
      return()
    }
    
    # Add new vote in "IP - Candidate" format
    new_vote <- paste(user_ip, "-", choice)
    votes_vec <- c(votes_vec, new_vote)
    
    # Escape and format as Postgres array
    escape_pg_array_element <- function(x) {
      x_escaped <- gsub('"', '\\"', x, fixed = TRUE)
      paste0('"', x_escaped, '"')
    }
    
    pg_vote_array <- paste0("{", paste(sapply(votes_vec, escape_pg_array_element), collapse = ","), "}")
    
    # Update DB
    dbExecute(db_conn, "
    UPDATE pigakura SET votes = $1 WHERE uuid = $2
  ", params = list(pg_vote_array, uuid))
    
    f7Toast("Vote recorded! 🗳️", position = "center", closeTimeout = 3000)
  })
  
  
  output$results_ui <- renderUI({
    invalidateLater(5000, session)  # Refresh every 5 seconds
    
    poll_data <- dbGetQuery(db_conn, "
    SELECT contestants, votes FROM pigakura WHERE id = $1 LIMIT 1
  ", params = list(fixed_poll_id))
    
    if (nrow(poll_data) == 0) return(NULL)
    
    contestants <- strsplit(gsub("[{}\"]", "", poll_data$contestants[1]), ",")[[1]]
    votes_raw <- strsplit(gsub("[{}\"]", "", poll_data$votes[1]), ",")[[1]]
    
    # Filter and extract just the candidate name from "IP - Name"
    votes_clean <- votes_raw[votes_raw != "" & votes_raw != "0"]
    candidate_names <- sapply(votes_clean, function(v) {
      parts <- strsplit(v, " - ")[[1]]
      if (length(parts) == 2) trimws(parts[2]) else NA
    })
    candidate_names <- candidate_names[!is.na(candidate_names)]
    
    vote_counts <- sapply(contestants, function(name) sum(candidate_names == name))
    total_votes <- sum(vote_counts)
    
    if (total_votes == 0) {
      return(tagList(
        tags$h3("📊 Live Results", style = "margin-top: 24px; margin-bottom: 16px; margin-left: 16px;"),
        f7Card(
          style = "margin-top: 16px;",
          "No votes yet. Be the first to vote! 🎉"
        )
      ))
    }
    
    colors <- c("#2F4F4F", "#8B0000", "#556B2F", "#483D8B", "#8B4513")
    bar_colors <- rep(colors, length.out = length(contestants))
    
    pie_data <- data.frame(
      name = contestants,
      y = as.numeric(vote_counts)
    )
    
    tagList(
      tags$h3("📊 Live Results", style = "margin-top: 24px; margin-bottom: 16px; margin-left: 16px;"),
      f7Card(
        style = "margin-bottom: 16px;",
        lapply(seq_along(contestants), function(i) {
          name <- contestants[i]
          count <- vote_counts[i]
          percent <- round((count / total_votes) * 100)
          
          tags$div(
            style = "margin-bottom: 16px;",
            tags$div(
              style = "display: flex; justify-content: space-between; font-weight: bold; font-size: 14px;",
              tags$span(name),
              tags$span(sprintf("%d%% (%d vote%s)", percent, count, ifelse(count == 1, "", "s")))
            ),
            tags$div(
              style = "height: 16px; background-color: #ccc; border-radius: 8px; overflow: hidden;",
              tags$div(
                style = paste0(
                  "height: 100%; width: ", percent, "%;",
                  "background-color: ", bar_colors[i], "; border-radius: 8px;"
                )
              )
            )
          )
        }),
        tags$hr(style = "margin-top: 24px; margin-bottom: 8px;"),
        tags$div(
          style = "text-align: left; font-weight: bold; font-size: 14px; margin-left: 16px;",
          paste("Total votes:", total_votes)
        ),
        highchartOutput("results_pie", height = "300px", width = "100%"),
        tags$div(
          style = "margin-top: 24px; text-align: center;",
          actionButton(
            inputId = "live_results_refresh",
            label = HTML("<i class='fa fa-signal' style='margin-right: 6px;'></i>Live Results"),
            style = "background-color: #006400; color: white; border: 2px solid #90ee90; font-weight: bold; padding: 8px 16px; border-radius: 8px;"
          )
        )
      )
    )
  })
  
  
  output$results_pie <- renderHighchart({
    invalidateLater(5000, session)  # Refresh every 5 seconds
    
    poll_data <- dbGetQuery(db_conn, "
    SELECT title, contestants, votes FROM pigakura WHERE id = $1 LIMIT 1
  ", params = list(fixed_poll_id))
    
    if (nrow(poll_data) == 0) return(NULL)
    
    contestants <- strsplit(gsub("[{}\"]", "", poll_data$contestants[1]), ",")[[1]]
    votes_raw <- strsplit(gsub("[{}\"]", "", poll_data$votes[1]), ",")[[1]]
    votes_clean <- votes_raw[votes_raw != "" & votes_raw != "0"]
    
    # Extract just the candidate name from "IP - Name" entries
    candidate_votes <- sapply(votes_clean, function(v) {
      parts <- strsplit(v, " - ")[[1]]
      if (length(parts) == 2) trimws(parts[2]) else NA
    })
    candidate_votes <- candidate_votes[!is.na(candidate_votes)]
    
    # Tally votes by candidate name
    vote_counts <- sapply(contestants, function(name) sum(candidate_votes == name))
    
    ds <- data.frame(
      contestant = contestants,
      votes = vote_counts
    ) %>%
      dplyr::arrange(-votes)
    
    colors <- c("#2F4F4F", "#8B0000", "#556B2F", "#483D8B", "#8B4513")
    bar_colors <- rep(colors, length.out = nrow(ds))
    
    ds %>%
      hchart("column", hcaes(x = contestant, y = votes), colorByPoint = TRUE) %>%
      hc_colors(bar_colors) %>%
      hc_title(text = paste0("<b>", poll_data$title[1], "</b>"), useHTML = TRUE) %>%
      hc_subtitle(text = "<b><i>Powered by Ushindi Analytics Limited</i></b>", useHTML = TRUE) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_xAxis(title = list(text = "Contestants")) %>%
      hc_yAxis(title = list(text = "Votes"))
  })
  
  
  
  
  
  

}


shinyApp(ui, server)
