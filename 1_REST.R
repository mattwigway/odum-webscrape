# In this exercise, we will be retrieving data from the ProPublica Nonprofit
# Explorer API

# The API is documented at https://projects.propublica.org/nonprofits/api

# There are essentially two sub-APIs - one "catalog" API for finding datasets,
# and one "dataset" API for interacting with the datasets themselves. We will be
# working with the "dataset" API.

# First, we need to load some libraries
# We are going to use the httr2 library to interact with the OSBM API

# if httr2 is not found, run install.packages("httr2")
library(httr2)
# if glue is not found, run install.packages("glue")
library(glue)

# The main entrypoint for the ProPublica non-profit API is the /search.json path.
# There are a number of parameters that can be passed to this API - we're going
# to look for entitites involved in education in NC.
req = request("https://projects.propublica.org/nonprofits/api/v2/search.json") |>
    req_url_query("ntee[id]"=2, "state[id]"="NC")
resp = req_perform(req)

# check response code. HTTP 200 means OK.
stopifnot(resp_status(resp) == 200)

entities = resp |> resp_body_json()

# the "entities" variable now contains our entities. Take a look at it.
# However, it is a complex, hierarchical structure - and for this, a tabular
# format will work more or less fine.

# Many APIs will return data formatted like this, where there is metadata at the
# top level (e.g. $num_pages)
entities$num_pages

# And then the data is in $organizations
entities$organizations

# This maps onto a JSON structure like this:
# {
#    "num_pages": 286,
#    "organizations": [
#      {
#         "name": "..."
#      }
#    ]
# }

# We can easily convert to a table
organizations = bind_rows(entities$organizations)

# Pagination
# Do we think there are only 25 educational non-profits in NC? Probably not.
# like many APIs, the ProPublic API paginates - i.e. splits results up into
# pages.

# httr2 provides tools to handle pagination. In this case, the ProPublica API takes a
# ?page parameter; this is one of the common ways to handle pagination. We can tell httr2
# to make the request repeatedly and update the page as we go like so:
req = request("https://projects.propublica.org/nonprofits/api/v2/search.json") |>
    req_url_query("ntee[id]"=2, "state[id]"="NC") |>
    # this prevents us from averaging more than 1 request per second, to avoid overloading the server
    req_throttle(capacity = 60, fill_time_s = 60)

resps = req_perform_iterative(
    req,
    # This will adjust the pages parameter, adding 1 and repeating the request for the next page
    # There are lots of options to work with different sites, see the help for iterate_with_offset,
    # iterate_with_cursor, and iterate_with_link_url
    next_req=iterate_with_offset(
        "page",
        start=0,
        offset=1,
        # This calculates how many total pages to request, based on the first
        # response
        resp_pages=\(resp) resp_body_json(resp)$num_pages,
    )
    # This will turn off the maximum requests (default 20 requests)
    # I'm leaving it out because it makes it slow, but it does work
    #max_reqs = Inf
)

# We can convert that to a data frame
organizations = resps |>
    # map applies a function to every element of a list - in this case, every page
    # We convert each page to a table
    map(\(resp) bind_rows(resp_body_json(resp)$organizations)) |>
    # then, we stack all those tables together
    list_rbind()

# More detail on an organization's non-profit filings
# We can get more detail on the filings of a single organization from the /:ein.json
# endpoint, where :ein is the ein from the organizations table.
ein = organizations$ein[1]
# Glue inserts the actual variable values into the string for any variable enclosed in {}
req = request(glue("https://projects.propublica.org/nonprofits/api/v2/organizations/{ein}.json")) |>
    req_throttle(capacity = 60, fill_time_s = 60)
resp = req_perform(req)

org = resp |> resp_body_json()

# Take a look at that org object
# It's a little more complicated - because there are multiple filings
# We can create a table of all the filings for one organization like this
# (note that there are two sub 
filings = bind_rows(org$filings_with_data)

# We can use map to do this with multiple orgs as well
# I have 1:5 here to select only the first five organizations,
# to avoid overwhelming ProPublica servers, but it should work if removed
filings = organizations$ein[1:5] |> 
    # this is just another way to write a function, which you need to use
    # for a multiline function
    map(function (ein) {
        req = request(glue("https://projects.propublica.org/nonprofits/api/v2/organizations/{ein}.json")) |>
            req_throttle(capacity = 60, fill_time_s = 60)
        resp = req_perform(req)

        org = resp |> resp_body_json()

        # Take a look at that org object
        # It's a little more complicated - because there are multiple filings
        # We can create a table of all the filings for one organization like this
        # (note that there are two sub 
        filings = bind_rows(org$filings_with_data)

        # The ProPublica API already incldues the EIN in each filing, so this is not strictly
        # necessary, but is an important step if you are working with an API that does not.
        filings$ein = ein
    
        # return the data frame to be stiched together
        return(filings)
    }) |>
    list_rbind()

# We now have two datasets: organizations and filings, which we can link by EIN. For example, this is
# total explenditure by non-profit educational organizations in NC (or would be if we weren't limiting
# the number of results above.
organizations[1:5,] |>
    left_join(filings, by="ein") |>
    group_by(tax_prd_yr) |>
    summarize(surplus=sum(totrevenue - totfuncexpns)) |>
    ggplot(aes(x=tax_prd_yr, y=surplus)) +
        geom_line()
