#' Test Function
#'
#' @param x Numeric input character vector
#' @return A numeric vector the same length as `x`.
#'   `NA` strings have `NA` length.
#' @seealso [nchar()]
#' @export
#' @examples
#' test_function(6)
test_function <- function(x){x+7}


#' Quick Summary of Tweets
#'
#' @param data Csv object that was retrieved from Twarc2
#' @return Gives a quick summary of the tweet dataset `data`.
#' @export
#' @examples
#' quick_sum(twarc_data)
quick_sum <- function(data){
  J <- length(unique(data$author.id))
  R <- nrow(data)
  cat("In your tweet dataset",J,"of unique users sent",R,"tweets. \n")
  range <- round(range(data$created_at)[2]-range(data$created_at)[1])
  cat("The time range of tweets are about",range,"days.\n")
  P <- length(unique(data$geo.coordinates.coordinates))
  cat("As for the GeoLocation info,",round(P/J,4) * 100, "percent of your users publicly shared their geolocation coordinates.")
}


#' Plot Timeline of the Tweets (Rtweet)
#'
#' @param data object that was retrieved from Twarc2
#' @return USER RTWEET. This is now deprecated. Plot the frequency change of tweets over time. This is directly borrowed from rtweet's `ts_plot` function.
#' @export
#' @examples
#' quick_sum(twarc_data)
plot_frequency <- function(data){ts_plot(data)}

#' Plot Timeline of the Tweets (by Hour)
#'
#' @param data object that was retrieved from Twarc2
#' @return Plot the frequency change of tweets over time.
#' @export
#' @examples
#' quick_sum(twarc_data)
plot_hourly <- function(data){plot(table(hour(data$created_at)))}


#' Re-format Location Info
#'
#' @param data object that was retrieved from Twarc2
#' @return This approach is based on TIMME's location re-formatting. Using the public location info, it creates three new columns:"city","state"
#' @export
#' @examples
#' quick_sum(twarc_data)
location_reformat <- function(data){}


#' Remove Unnecessary Variables
#'
#' @param data object that was retrieved from Twarc2
#' @return This function reduces the number of columns from 75 to around 50. Use with caution. You may not want to remove all of them; but as far as I see, most people do not need these unneccessary variables.
#' @export
#' @examples
#' quick_sum(twarc_data)
reduce_variable <- function(data){
data <- data %>% dplyr::select(c(
  -entities.cashtags,
  -entities.urls,
  -entities.mentions,
  -entities.hashtags,
  -author.entities.description.cashtags,
  -author.entities.description.hashtags,
  -author.entities.description.mentions,
  -author.entities.description.urls,
  -author.entities.url.urls,
  -lang,
  -context_annotations,
  -entities.annotations,
  -attachments.media,
  -attachments.media_keys,
  -attachments.poll.duration_minutes,
  -attachments.poll.end_datetime,
  -attachments.poll.id,
  -attachments.poll.options,
  -attachments.poll.voting_status,
  -attachments.poll_ids,
  -author.profile_image_url,
  -author.pinned_tweet_id,
  -author.url,
  -author.protected,
  -X__twarc.retrieved_at,
  -X__twarc.url,
  -X__twarc.version,
  -X))
}


#' Clean Data
#'
#' @param data object that was retrieved from Twarc2
#' @return Removes unnecessary variables, removes accounts with too many or too less friends,
#' @export
#' @examples
#' quick_sum(twarc_data)
clean_data <- function(data){
  nrows_1 <- nrow(data)
  J <- length(unique(data$author_id))
  data <- subset(data, lang == "en")
  data <- data %>% dplyr::select(c(
    -entities.cashtags,
    -entities.urls,
    -entities.mentions,
    -entities.hashtags,
    -author.entities.description.cashtags,
    -author.entities.description.hashtags,
    -author.entities.description.mentions,
    -author.entities.description.urls,
    -author.entities.url.urls,
    -lang,
    -context_annotations,
    -entities.annotations,
    -attachments.media,
    -attachments.media_keys,
    -attachments.poll.duration_minutes,
    -attachments.poll.end_datetime,
    -attachments.poll.id,
    -attachments.poll.options,
    -attachments.poll.voting_status,
    -attachments.poll_ids,
    -author.profile_image_url,
    -author.pinned_tweet_id,
    -author.url,
    -author.protected,
    -X__twarc.retrieved_at,
    -X__twarc.url,
    -X__twarc.version,
    -X))
  data <- subset(data, public_metrics.retweet_count/author.public_metrics.tweet_count < 0.9)
  data <- subset(data, nchar(text) >20)
  data <- subset(data, author.public_metrics.following_count >5 &
                   author.public_metrics.following_count <1000)
  nrows_2 <- nrow(data)
  diff <- nrows_2-nrows_1
  message("The number of tweets now decreased by ",-1 * diff,".\n","Also, now we have ",
        length(unique(data$author_id))," users (instead of ",J,").")
  return(data)
}


