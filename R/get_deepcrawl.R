#' Get deepcrawl data for a list of sites
#'
#' @param sites Vector of URLs to get data for
#' @param api_key Account API key
#' @param account_id Account ID number
#'
#' @return A dataframe with one row per site
#' @export
#'
#' @examples
#'
#' sites <- c("http://www.independent.co.uk/voices/comment/the-loss-of-scottish-salmon-is-a-cultural-catastrophe-10158356.html",
#' "http://www.wrs.co.uk/", "http://thecodeofgoodpractice.co.uk/wp-content/uploads/2015/02/cogp_overview.pdf",
#' "http://www.bbc.co.uk/news/uk-scotland-scotland-business-35643363",
#' "http://www.bbc.co.uk/news/uk-scotland-scotland-business-35379470",
#' "http://www.dailymail.co.uk/health/article-205547/Salmon-health-alert.html")
#'
#' df <- get_deepcrawl(sites)
#' df
#'
get_reports_temp <- function(sites, api_key = 'b72OqnAniXHgLAjA8VHCUQ10UQ7EzE-I7eYNLqqWXlSAxYa2JU3AtvRAaIwVDt5kvZJW8Y9tQTjGoOPYb6mIzw', account_id = 424){

  session_token <- create_session_token(user, api_key)
  account_id    <- get_account_info(session_token)[[1]]$id
  project_id    <- create_project('Auto-generated Project', sites, session_token, account_id)
  crawl_id      <- create_crawl(session_token, account_id, project_id)

  while (TRUE){
    Sys.sleep(3)
    crawls <- get_crawls_info(session_token, account_id, project_id)

    if (crawls[purrr::map_int(crawls, 'id') == crawl_id][[1]]$status == 'finished'){
      reports <- get_reports_info(session_token, account_id, project_id, crawl_id)
      return(reports)
    }
  }


}


