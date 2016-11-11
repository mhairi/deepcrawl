


#' Get Account Info
#'
#' Get information about accounts
#'
#' @param session_token Session token from `create_session_token`.
#'
#' @return A list with one element for each account
#' @export
#'
#' @examples
#'
#' get_account_info(session_token)
#'
get_account_info <- function(session_token){

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com/accounts'

  session_response <-  httr::GET(url, httr::add_headers('X-Auth-Token' = session_token))

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed)

}



#' List all projects
#'
#' List all projects
#'
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#'
#' @return A list with one element for each project
#' @export
#'
#' @examples
#'
#' get_projects_info(session_token, 424)
get_projects_info <- function(session_token, account_id){

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com'

  session_response <- httr::GET(url,
                                 path = c('accounts', account_id, 'projects'),
                                 httr::add_headers('X-Auth-Token' = session_token))

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed)

}

#' List all crawls
#'
#' List all crawls for a given project
#'
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#' @param project_id The ID of the project, from `create_project`
#'
#' @return A list with one element for each crawl
#' @export
#'
#' @examples
#'
#' get_crawls_info(session_token, 424, 18823)
get_crawls_info <-  function(session_token, account_id, project_id){

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com'

  session_response <- httr::GET(url,
                                path = c('accounts', account_id, 'projects', project_id, 'crawls'),
                                httr::add_headers('X-Auth-Token' = session_token))

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed)

}

#' List all reports
#'
#' List all reports for a given crawl within a project
#'
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#' @param project_id The ID of the project, from `create_project
#' @param crawl_id The ID of the crawl, from `create_crawl`
#'
#' @return A list with one element for each report
#' @export
#'
#' @examples
#'
#' get_reports_info(session_token, 424, 18821, 114538)
get_reports_info <-  function(session_token, account_id, project_id, crawl_id){

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com'

  session_response <- httr::GET(url,
                                path = c('accounts', account_id,
                                         'projects', project_id,
                                         'crawls',   crawl_id,
                                         'reports'),
                                httr::add_headers('X-Auth-Token' = session_token))

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed)

}

#' Get all data from a report
#'
#' Get the data from a given report
#'
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#' @param project_id The ID of the project, from `create_project
#' @param crawl_id The ID of the crawl, from `create_crawl`
#' @param report_id The ID of the report (can be found within `get_report_info`
#'
#' @return A list with one element for each report
#' @export
#'
#' @examples
#'
#' get_report_data(session_token, 424, 18821, 114538, 42384583)
get_report_data <- function(session_token, account_id, project_id, crawl_id, report_id){

  url <- 'https://prod-1-dc-api-oopeix3r.deepcrawl.com'

  session_response <- httr::GET(url,
                                path = c('accounts', account_id,
                                         'projects', project_id,
                                         'crawls',   crawl_id,
                                         'reports',  report_id,
                                         'report_rows'),
                                httr::add_headers('X-Auth-Token' = session_token))

  httr::stop_for_status(session_response)

  if (httr::http_type(session_response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(session_response, "text", encoding = 'utf-8'), simplifyVector = FALSE)

  return(parsed)

}

#' Extract useful report data
#'
#' Get a data frame from the reports object
#'
#' @param session_token Session token from `create_session_token`.
#' @param account_id The ID for the account (can be found with get_account_info)
#' @param project_id The ID of the project, from `create_project
#' @param crawl_id The ID of the crawl, from `create_crawl`
#'
#' @return A dataframe with one observation for each URL
#' @export
#'
#' @examples
#'
#' get_all_report_data(session_token, 424, 18821, 114538)
get_all_report_data <- function(session_token, account_id, project_id, crawl_id){
  # Get the ID of the report we require
  reports <- get_reports_info(session_token, account_id, project_id, crawl_id)

  n_rows <- purrr::map_int(reports, 'total_rows')
  ids <- purrr::map_int(reports, 'id')
  ids <- ids[n_rows > 0]

  get_report_df <- function(id){
    # Get the data from this report
    raw_data <- get_report_data(session_token, account_id, project_id, crawl_id, id)
    data <- purrr::map(raw_data, 'data')

    # Change NULLs to missing values
    data <- purrr::map(data, ~purrr::map_if(.x, is.null, ~ NA))

    # Convert to dataframe and combine
    df <- purrr::map(data, tibble::as_data_frame)

    # Find overallaping varaibales
    cols <- purrr::map(df, names)
    cols <- purrr::reduce(cols, intersect)

    # Combine data frames
    df <- purrr::map(df, dplyr::select_, .dots = cols)
    df <- dplyr::bind_rows(df)

    return(df)
  }

  dfs <- purrr::map(ids, purrr::possibly(get_report_df, NULL))
  dfs <- purrr::keep(dfs, ~!is.null(.x))
}

# curl -X POST -H 'X-Auth-Token:abcdef123' 'https://prod-1-dc-api-oopeix3r.deepcrawl.com/accounts/1/projects/2/crawls' -d 'status=crawling'
# project_id <- 18821
# crawl_id <- 114538
# crawl_id <- 114540
