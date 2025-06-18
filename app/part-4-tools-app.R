# This is a bare-bones chat app like the one underlying the chat feature
# in the Shiny app at `app/app.R`.
# In the demo, I use `pak::pak("posit-dev/shinychat#52")` for a nice tool call UI.

# load in the data
source("app/generate-data.R")

# define database connection
library(DBI)
library(duckdb)

conn <- dbConnect(duckdb(), dbdir = ":memory:")
dbWriteTable(conn, "equipment_data", equipment_data, overwrite = TRUE)

# make a chat object
library(ellmer)
ch <- chat_anthropic(
  system_prompt = 
    c(readLines("app/txt/data_description.txt", warn = FALSE),
      btw::btw(equipment_data)@text,
      readLines("app/txt/extra_instructions.txt", warn = FALSE),
      "Be concise in your responses."
    ),
  model = "claude-sonnet-4-20250514"
)

# write a tool function and configure it
query_data <- function(query) {
  stopifnot(startsWith(tolower(query), "select"))
  dbGetQuery(conn, query)
}

ch$register_tool(tool(
  query_data,
  "Perform a SQL SELECT query on the `equipment_data` data.",
  query = type_string("A valid SQL SELECT statement to run on the mtcars table.")
))

# a quick trick: ask for a greeting from the model based on the prompt.
# then, remove the user turn "Please give me a friendly greeting..."
greeting_chat <- ch$clone()
greeting_chat$chat(
  "Please give me a friendly greeting. Include a few sample prompts in a two-level bulleted list.",
  echo = "none"
)

ch$set_turns(list(greeting_chat$get_turns()[[2]]))

# launch the app
live_browser(ch)
