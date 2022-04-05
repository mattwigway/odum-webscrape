# We are going to scrape the table of 2021-22 UNC basketball games from here:
# https://en.wikipedia.org/wiki/2021%E2%80%9322_North_Carolina_Tar_Heels_men%27s_basketball_team
# If a website table is actually coded as an HTML table (with <table> <thead> <tr> <th> <td> tags),
# rvest has a convenience function to extract data from an HTML table, 

# First, we load all of the same libraries
library(tidyverse)
library(rvest)
library(polite)

# Next, we set up a connection to the server. We can put a full URL here.
# You could put the wikipedia URL here. This is a mirror of it that won't change if the page
# is edited.
sess = bow("https://files.indicatrix.org/scrape/tarheels.html")
res = scrape(sess)

# now we need to find the table, and use the html_table function to turn it into a tabular
# dataset
data = html_element(res, "h2:contains('Schedule and results') + table") %>% html_table()

View(data)

# Manually remove the heading rows and extra columns
data = select(data, "Datetime, TV":"Site (attendance)  city, state") %>%
  filter(!(`Datetime, TV` %in% c("Exhibition", "Regular Season", "ACC Tournament", "NCAA Tournament") |
             str_starts(`Datetime, TV`, "*Non-conference")))

View(data)

# Often when web scraping there will not be a single HTML element/table cell you can select that will grab exactly
# what you want. For instance, suppose we wanted to graph the attendance from this table. It's stuck in a cell
# along with the name of the arena and the city. When we have a situation like this where there
# is text with a pattern to it, we can use "regular expressions" which match patterns.

# Regular expressions entail using special characters to write a pattern-matcher which
# can be applied to text. Most letters and numbers match themselves, but there are also
# special characters that modify match:
# . matches any letter
# ^ matches the start of a string
# $ matches the end
# [] forms a character class that matches anything within the brackets. For instance,
#     [ab] matches a or b
# ? matches 0 or 1 of the previous character or character class
# + matches 1 or more
# * matches 0 or more

# There are also special character codes to match groups of characters:
#  [:alpha:] matches any letter
#  [:digit:] matches any digit
#  [:alnum:] matches alphanumeric values
#  [:punct:] matches punctuation
#  [:whitespace:] matches whitespace

# \\ before any special character will cause it match that actual character rather
#     than its special meaning

# Anything enclosed in () will be extracted separately. Let's use regular expressions to
# extract the scores of each game. We'll run
# this once without saving the response, to see what the output looks like.
str_match(data$Result, "^([WL])[[:whitespace:]]*([[:digit:]]+)[[:punct:]]+([[:digit:]]+)[[:whitespace:]]*(OT)?$")

# Now, we can create four new columns with the extracted data. Since the first column of
# the extracted data is the full match, we select columns 2-5 and assign to new columns in the data
# frame.
data[,c("winloss", "unc_score", "other_score", "overtime")] =
  str_match(data$Result, "^([WL])[[:whitespace:]]*([[:digit:]]+)[[:punct:]]+([[:digit:]]+)[[:whitespace:]]*(OT)?$")[,2:5]

data$unc_score

# The scores have been stored as strings/chrs, i.e. text. We need to convert them to numbers
# to work with them.
data = mutate(data, unc_score=as.numeric(unc_score), other_score=as.numeric(other_score))

plot(1:nrow(data), data$unc_score, col="#4B9CD3", type = "lines")
lines(1:nrow(data), data$other_score)

# We can repeat this process to extract the arena and the attendance
data[,c("arena", "attendance", "city")] = str_match(data$`Site (attendance)  city, state`, "^(.*)\\(([,[:digit:]]+)\\)(.*)$")[,2:4]

View(data)

# We still need to do a little data cleaning on these columns. For the arena column, there
# is a space at the end of the arena name. The trimws() function will remove this.
data = mutate(data, arena=str_trim(arena))

# As before, we need to make the attendance into a number rather than a string (chr, i.e. text data)
# to plot it. We need to remove the commas as they will prevent parsing into numbers, and then
# use as.numeric to turn them into numbers
data = mutate(data, attendance=as.numeric(str_remove(attendance, ",")))

# now we can plot it
plot(1:nrow(data), data$attendance, type="lines")

# A few more examples of using regular expressions. We can parse the date, time,
# and network column
data[,c("date", "time", "network")] = 
  str_match(data$`Datetime, TV`, "^(.* 2022)\\*?([[:digit:]]+:[[:digit:]]+ [ap]\\.m\\.),[[:whitespace:]]*(.*)$")[,2:4]

