
#' Create Session Token
#'
#' Create the session token needed for other DeepCrawl requests.
#' Lasts for 6 hours.
#'
#' @param user User ID
#' @param api_key API key
#'
#' @return A string with the session token
#' @export
#'
#' @examples
#'
#' user    <- '42'
#' api_key <- 'b72OqnAniXHgLAjA8VHCUQ10UQ7EzE-I7eYNLqqWXlSAxYa2JU3AtvRAaIwVDt5kvZJW8Y9tQTjGoOPYb6mIzw'
#' session_token <- create_session_token(user, api_key)
#'
create_session_token <- function(user, api_key){

  authenticate_url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com/sessions'

  session_response <- httr::POST(authenticate_url, httr::authenticate(user, api_key))

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed$token)

}



#' Create crawling project
#'
#' Set up a crawl
#'
#' @param name A character string giving a name for the project
#' @param sites A list of sites to crawl
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#'
#' @return The crawl's ID number. Also, will define a crawl on deepweb.
#' @export
#'
#' @examples
#' sites <- c("http://scottishsalmon.co.uk/", "http://www.independent.co.uk/voices/comment/the-loss-of-scottish-salmon-is-a-cultural-catastrophe-10158356.html")
#' project_id <- create_project('test crawl', sites, session_token, 424)
#'
create_project <- function(name, site, session_token, account_id){

  project_data <- list(
    name = name,
    site_primary = sites[1],
    v2_crawl_type_codes = c('web', 'list', 'google_analytics', 'backlinks'),
    crawl_rate = 1,
    limit_levels_max = 1,
    limit_pages_max = length(sites),
    auto_finalize = TRUE,
    start_urls = sites
  )

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com'

  session_response <- httr::POST(url,
                                 path = c('accounts', account_id, 'projects'),
                                 httr::add_headers('X-Auth-Token' = session_token),
                                 body = project_data,
                                 encode = 'json')

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed$id)

}


#' Create crawl
#'
#' Set a project's crawl to start
#'
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#' @param project_id The ID of the project, from `create_project`
#'
#' @return The crawl's ID number. Also, will define a crawl on deepweb.
#' @export
#'
#' @examples
#' crawl_id <- set_crawl_urls('test crawl', sites, session_token, 424)
#'
create_crawl <- function(session_token, account_id, project_id){

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com'

  data <- list(status = 'crawling')

  session_response <-  httr::POST(url,
                                  httr::add_headers('X-Auth-Token' = session_token),
                                  path = c('accounts', account_id, 'projects', project_id, 'crawls'),
                                  body = data,
                                  encoding = 'form')

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed$id)

}



