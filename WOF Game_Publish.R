library(shiny)

# Load dataset
clues_data <- read.csv("updated_scrape.csv", as.is = T)
clues_data <- clues_data[, 1:5]
names(clues_data)[1] <- "Clue"

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .letter-box {
        display: inline-block;
        width: 50px;
        height: 60px;
        margin: 2px;
        border: 3px solid #2C3E50;
        border-radius: 5px;
        text-align: center;
        line-height: 60px;
        font-size: 32px;
        font-weight: bold;
        background-color: #FFFFFF;
        color: #000000;
        box-shadow: 0 4px 6px rgba(0,0,0,0.3);
        vertical-align: middle;
      }

      .blank-box {
        display: inline-block;
        width: 50px;
        height: 60px;
        margin: 2px;
        border: 3px solid #BDC3C7;
        border-radius: 5px;
        text-align: center;
        line-height: 60px;
        font-size: 32px;
        font-weight: bold;
        background-color: #ECF0F1;
        color: #34495E;
        box-shadow: 0 4px 6px rgba(0,0,0,0.2);
        vertical-align: middle;
      }

      .space-separator {
        display: inline-block;
        width: 20px;
        height: 60px;
        vertical-align: middle;
      }

      .punctuation {
        display: inline-block;
        margin: 0 5px;
        font-size: 32px;
        font-weight: bold;
        color: #FFFFFF;
        vertical-align: middle;
        line-height: 60px;
      }

      .puzzle-container {
        background-color: #22B016;
        padding: 30px;
        border-radius: 10px;
        margin: 20px 0;
        text-align: center;
        box-shadow: 0 5px 15px rgba(0,0,0,0.3);
        line-height: 70px;
      }

      .main-panel {
        background-color: #ECF0F1;
        padding: 20px;
        border-radius: 10px;
      }

      .sidebar-panel {
        background-color: #BDC3C7;
        padding: 20px;
        border-radius: 10px;
      }
    "))
  ),
  titlePanel("Wheel of Fortune Puzzle Game"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      textInput("letter_guess", "Enter a Letter:", ""),
      actionButton("guess", "Guess"),
      actionButton("reveal", "Reveal Answer"),
      br(),
      br(),
      textOutput("wrong"),
      textOutput("feedback"),
      br(),
      p("This game gives you a random puzzle from Wheel of Fortune Seasons 30 - 42. Correct letter guesses will appear in the puzzle. Incorrect guesses will show in the 'Used Letters' section. Made by Kira Tebbe.")
    ),
    mainPanel(
      class = "main-panel",
      h3(textOutput("category_display")),
      div(
        class = "puzzle-container",
        htmlOutput("clue_display")
      ),
      h3(textOutput("revealed_answer"), style = "color: blue;"),
      h3(textOutput("vowels_remaining"), style = "color: red;")
    )
  ),
  tags$script(HTML("
    $(document).on('keydown', function (e) {
      if (e.key === 'Enter') {
        $('#guess').click();
      }
    });
  "))
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to store the current state
  values <- reactiveValues(
    current_category = NULL,
    current_clue = NULL,
    displayed_clue = NULL,
    revealed_positions = NULL,
    feedback_message = "",
    wrong_letters = "Used Letters: ",
    revealed_answer = NULL,
    vowels_remaining = TRUE,
    round = NULL
  )

  # Helper function: Format the clue with boxes around letters
  format_clue <- function(clue, revealed_positions) {
    clue_chars <- strsplit(clue, "")[[1]]
    formatted <- sapply(seq_along(clue_chars), function(i) {
      if (clue_chars[i] == " ") {
        '<span class="space-separator"></span>'
      } else if (clue_chars[i] %in% c("'", "-", ",", "!", "?", "&", ".", ":", ";")) {
        paste0('<span class="punctuation">', clue_chars[i], "</span>")
      } else if (revealed_positions[i]) {
        paste0('<span class="letter-box">', clue_chars[i], "</span>")
      } else {
        '<span class="blank-box"></span>'
      }
    })
    paste(formatted, collapse = "")
  }

  # Function to reset the game with a new puzzle
  reset_game <- function() {
    random_row <- clues_data[sample(nrow(clues_data), 1), ]
    random_category <- ifelse(is.na(random_row$Category) || random_row$Category == "", "Uncategorized", random_row$Category)
    random_clue <- ifelse(is.na(random_row$Clue) || random_row$Clue == "" || nchar(random_row$Clue) == 0, "NO CLUE AVAILABLE", random_row$Clue)
    random_round <- ifelse(is.na(random_row$Round) || random_row$Round == "", "Uncategorized", random_row$Round)
    isolate({
      values$current_category <- random_category
      values$current_clue <- toupper(as.character(random_clue))
      values$revealed_positions <- rep(FALSE, nchar(values$current_clue))
      values$displayed_clue <- format_clue(values$current_clue, values$revealed_positions)
      values$feedback_message <- ""
      values$wrong_letters <- "Used letters: "
      values$revealed_answer <- NULL
      values$vowels_remaining <- TRUE
      values$round <- random_round
    })
  }

  # Initialize the game
  reset_game()

  # Display the category
  output$category_display <- renderText({
    req(values$current_category)
    paste("Category:", values$current_category, " Round: ", values$round)
  })

  # Display the clue as HTML to preserve spaces
  output$clue_display <- renderUI({
    req(values$displayed_clue)
    HTML(values$displayed_clue)
  })

  # Display feedback message
  output$feedback <- renderText({
    values$feedback_message
  })

  # Display wrong letters
  output$wrong <- renderText({
    values$wrong_letters
  })

  output$vowels_remaining <- renderText({
    if (values$vowels_remaining) {
      paste("Vowels Remaining")
    } else {
      paste("No Vowels Remaining")
    }
  })

  # Display the revealed answer
  output$revealed_answer <- renderText({
    req(values$revealed_answer)
    values$revealed_answer
  })

  # Handle letter guesses
  observeEvent(input$guess, {
    guess <- toupper(trimws(input$letter_guess))
    if (nchar(guess) != 1 || !grepl("[A-Z]", guess)) {
      values$feedback_message <- "Please enter a single letter."
      return()
    }

    # Check if the letter exists in the clue
    clue_chars <- strsplit(values$current_clue, "")[[1]]
    if (guess %in% clue_chars) {
      indices <- which(clue_chars == guess)
      isolate({
        values$revealed_positions[indices] <- TRUE
        values$displayed_clue <- format_clue(values$current_clue, values$revealed_positions)
        values$feedback_message <- ""
      })
      # Check if all vowels are revealed
      vowel_positions <- which(clue_chars %in% c("A", "E", "I", "O", "U"))
      if (length(vowel_positions) > 0 && all(values$revealed_positions[vowel_positions])) {
        values$vowels_remaining <- FALSE
      }
    } else {
      values$feedback_message <- "Not in Puzzle."
      values$wrong_letters <- paste(values$wrong_letters, guess, ", ")

      # Clear the message after 3 seconds
      invalidateLater(3000, session)
      observe({
        isolate({
          if (values$feedback_message == "Not in Puzzle.") {
            values$feedback_message <- ""
          }
        })
      })
    }

    # Clear the input field
    updateTextInput(session, "letter_guess", value = "")
  })

  # Reveal the correct answer
  observeEvent(input$reveal, {
    values$revealed_answer <- paste("The Correct Answer is:", values$current_clue)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
