library(shiny)
library(lubridate)
library(glue)

EVENT_TYPES <- c("Review Due", "Authors Resubmit", "Decision EA")
RANGES <- c("one week" = 7, "two weeks" = 14, "one month (~30d)" = 30)

# Escape per RFC 5545
ical_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub(";", "\\\\;", x)
  x <- gsub(",", "\\\\,", x)
  x <- gsub("\r?\n", "\\\\n", x)
  x
}

# Fold long lines (safe)
fold_ical <- function(txt) {
  unlist(lapply(strsplit(txt, "\n", fixed = TRUE)[[1]], function(line) {
    parts <- character(0)
    while (nchar(line, type = "bytes") > 75) {
      cut <- 75
      parts <- c(parts, substr(line, 1, cut))
      line <- paste0(" ", substr(line, cut + 1, nchar(line)))
    }
    c(parts, line)
  })) |> paste(collapse = "\n")
}

build_ics <- function(summary, start_date, days, description = "", location = "Online-OJS") {
  dtstamp <- format(with_tz(Sys.time(), "UTC"), "%Y%m%dT%H%M%SZ")
  dtstart <- format(as.Date(start_date), "%Y%m%d")
  dtend   <- format(as.Date(start_date) + days, "%Y%m%d")
  uid     <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ"), "-", digest::digest(runif(1)), "@shiny-issn")
  
  lines <- c(
    "BEGIN:VCALENDAR",
    "PRODID:-//PeerReview Shiny//Calendar 1.0//EN",
    "VERSION:2.0",
    "CALSCALE:GREGORIAN",
    "METHOD:PUBLISH",
    "BEGIN:VEVENT",
    glue("UID:{uid}"),
    glue("DTSTAMP:{dtstamp}"),
    glue("DTSTART;VALUE=DATE:{dtstart}"),
    glue("DTEND;VALUE=DATE:{dtend}"),
    glue("SUMMARY:{ical_escape(summary)}"),
    if (nzchar(description)) glue("DESCRIPTION:{ical_escape(description)}") else NULL,
    glue("LOCATION:{ical_escape(location)}"),
    "END:VEVENT",
    "END:VCALENDAR"
  )
  
  fold_ical(paste(lines, collapse = "\n"))
}

ui <- fluidPage(
  tags$div(
    style = "display:flex; justify-content:space-between; align-items:center; background-color:#f7f7f7; padding:10px 20px; border-bottom:2px solid #e0e0e0;",
    tags$h3("IJANR .ics file Maker", style = "margin:0; color:#003366;"),
    tags$img(src = "logo.png", height = "60px", style = "border-radius:8px;")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("etype", "Event type", choices = EVENT_TYPES),
      dateInput("start", "Start date", value = Sys.Date()),
      radioButtons("range", "Duration",
                   choices = names(RANGES), selected = "one week"),
      textInput("summary_extra", "ID manuscript", placeholder = "99999"),
      textAreaInput("desc", "Notes (DESCRIPTION in calendar)",
                    placeholder = "Any extra instructions, links, checklist…",
                    rows = 4),
      hr(),
      downloadButton("dl_ics", "Download .ics")
    ),
    mainPanel(
      h4("Preview"),
      verbatimTextOutput("preview"),
      tags$hr(),
      tags$p(em(".ics file compatible with Google Calendar, Outlook, Apple Calendar, etc")),
      tags$p("All-day events span the selected start date through the selected range")
    )
  ),
  
  tags$footer(
    style = "
      position: fixed;
      bottom: 0;
      width: 100%;
      background-color: #f7f7f7;
      color: #333333;
      text-align: center;
      padding: 10px;
      border-top: 2px solid #e0e0e0;
      font-size: 14px;
    ",
    HTML('© 2025 <b>IJANR</b> — 
          <a href="https://revistas.uc.cl/ijanr" target="_blank" style="color:#003366; text-decoration:none;">
          International Journal of Agriculture and Natural Resources</a>')
  )
)

server <- function(input, output, session) {
  ics_text <- reactive({
    days <- unname(RANGES[input$range])
    base_sum <- input$etype
    sum_txt <- if (nzchar(input$summary_extra)) {
      paste0(base_sum, " — ", input$summary_extra)
    } else base_sum
    
    build_ics(
      summary    = sum_txt,
      start_date = input$start,
      days       = days,
      description = input$desc
    )
  })
  
  output$preview <- renderText({
    ics_text()
  })
  
  output$dl_ics <- downloadHandler(
    filename = function() {
      clean <- gsub("[^A-Za-z0-9_-]+", "_",
                    paste0(input$etype, "_", format(input$start, "%Y%m%d")))
      paste0(clean, ".ics")
    },
    content = function(file) {
      writeLines(ics_text(), file, useBytes = TRUE)
    }
  )
}

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Please install.packages('digest') before running this app.")
}

shinyApp(ui, server)