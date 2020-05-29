# file_path = "data/test/tweet_activity_metrics_LesFlicks_20200201_20200301_en.csv"
get_data <- function(file_path){
  input_data <- read.csv(file_path)
  # special_chars <- paste(input_data$Tweet.text, collapse = ",")
  f_special_chars <- function(s) regmatches(s, regexpr("[^\001-\177]",s))
  special_chars <- unique(sapply(input_data$Tweet.text[grep("[^\001-\177]", input_data$Tweet.text)], f_special_chars))
  
  f_id <- function(s) gsub("https://twitter.com/LesFlicks/status/", "", s, fixed = TRUE)
  f_time <- function(s) as.numeric(gsub(":",".",strsplit(as.character(s), " ")[[1]][2]))
  f_date <- function(s) strsplit(as.character(s), " ")[[1]][1]
  f_day <- function(s) weekdays(as.Date(as.character(s),'%Y-%m-%d'))
  f_clean_newline <- function(s) gsub("\n"," ", as.character(s))
  f_clean_specialchars <- function(s){ 
    s <- as.character(s)
    # special_chars <- c("\U0001f37f","\U0001f44f","\U0001f3f3","\U0001f308")
    for (sc in special_chars){
      s <- gsub(sc,"", s)
    }
    gsub("’"," ",s)
    return(s)
  }
  f_hastags <- function(s) paste(sort(strsplit(f_clean_newline(f_clean_specialchars(s)), split = " ")[[1]][grep("#.+",strsplit(f_clean_newline(f_clean_specialchars(s)), split = " ")[[1]])]), collapse = ",")
  f_mentions <- function(s) paste(sort(strsplit(f_clean_newline(f_clean_specialchars(s)), split = " ")[[1]][grep("@.+",strsplit(f_clean_newline(f_clean_specialchars(s)), split = " ")[[1]])]), collapse = ",")
    
  twitter_data <- data.frame(id=sapply(input_data$Tweet.permalink, f_id),
                             Tweet=sapply(input_data$Tweet.text, f_clean_newline),
                             time=sapply(input_data$time, f_time),
                             date=sapply(input_data$time, f_date),
                             weekday="",
                             impressions=input_data$impressions,
                             engagements=input_data$engagements, 
                             hashtags=sapply(input_data$Tweet.text, f_hastags),
                             hashtag_clicks=input_data$hashtag.clicks,
                             mentions=sapply(input_data$Tweet.text, f_mentions),
                             retweets=input_data$retweets,
                             likes=input_data$likes,
                             replies=input_data$replies,
                             user.profile.clicks=input_data$user.profile.clicks,
                             url.clicks=input_data$url.clicks,
                             follows=input_data$follows,
                             media.views=input_data$media.views,
                             media.engagements=input_data$media.engagements,
                             permalink=input_data$Tweet.permalink,
                             stringsAsFactors = FALSE
                             )
  twitter_data$weekday=factor(sapply(twitter_data$date, f_day),
                              labels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday"))
  # twitter_data$hashtags
  # twitter_data$mentions
  return(twitter_data)
}
# Impressions = “seen”
# Correlation of time of post, day of week on number of impressions
# - X day of week
# - Y Time of day (Split into 2 hour blocks)
# - Lines by post type
# - # of impressions
# - > Pull all months together

# output_path <- "~/Documents/Andrea/LesFlick/data_reporting/output/2020_05_25/"
plot_impressions_by_day_time <- function(twitter_data, output_path){
  library(ggplot2)
  colours <- RColorBrewer::brewer.pal(n = 7, name = "Spectral")
  colours[4] <- "#FFFF33"
  colours[5] <- "#b2df8a"
  p <- ggplot(twitter_data, mapping = aes(x=time, y=impressions,col=weekday))  +
    geom_point(size = 2) +
      theme_bw() + scale_color_manual(values = colours)
  p
  d=data.frame(x1=c(0,8,16), x2=c(4,12,20), y1=0, y2=max(twitter_data$impressions))
  # p <- ggplot(twitter_data, mapping = aes(x=time, y=impressions,col=weekday))  +
  p <- ggplot(twitter_data, mapping = aes(x=time, y=impressions, col=weekday))  +
    geom_vline(xintercept = c(4,8,12,16,20), colour="grey85") +
    geom_point(size = 1.5) +
    geom_line() +
    theme_bw() + scale_color_manual(values = colours)  +
    # ylim(0,1750) + 
    theme(panel.grid = element_blank(),axis.text = element_text(colour = "black")) +
    facet_grid(weekday~.,scales = "free") +
    # geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill="grey"), color="black", alpha=0.5) +
    theme(legend.position = "none")+
    theme(strip.background.x = element_rect(fill = "grey95",colour = "white"),
          strip.text.x = element_text(color = "black",face = "bold"),
          strip.background.y = element_rect(color = "grey85",fill = "grey85"),
          strip.text.y = element_text(color = "black",face = "bold"))
  # p
  ggsave(plot = p, filename = paste(output_path, "impressions_by_day_time.png", sep=""), width = 7, height=10)
  return(p)
}

get_date_range <- function(twitter_data){
  from <- strsplit(min(twitter_data$date), split="-")[[1]]
  to <- strsplit(max(twitter_data$date), split="-")[[1]]
  return(sprintf("%s - %s",
                 paste(from[c(3,2,1)], collapse=" "),
                 paste(to[c(3,2,1)], collapse=" ")
                 )
         )
}
# Correlation of time of post, day of week and type of post to the number of clicks
# - X day of week
# - Y Time of day
# -  Lines by post type
# - # of impressions

# Hashtag and clicks
plot_hashtags <- function(twitter_data, output_path){
  twitter_data <- twitter_data[order(twitter_data$hashtag_clicks,decreasing = TRUE),]
  maxChar <- max(nchar(twitter_data$hashtags[1:6]))
  p <- ggplot2::ggplot(twitter_data[1:6,],mapping = ggplot2::aes(x = hashtags, y = hashtag_clicks)) + 
    ggplot2::geom_point() +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.ticks.x   = ggplot2::element_blank()) + 
    ggplot2::theme(axis.text.y   = ggplot2::element_text(size = 14)) +
    ggplot2::theme(axis.text.x   = ggplot2::element_text(hjust = 1,vjust=1,colour = "black",angle = 45)) +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),panel.grid.minor.y = ggplot2::element_blank()) +
    ggplot2::theme(plot.margin= ggplot2::unit(c(0.5,0.5,0.5,5),"cm"))   # top, right, bottom, and left  
  ggsave(plot = p, filename = paste(output_path, "hastag_clicks.png", sep=""), width = 7, height=10)
  return(p)
}
# 

get_top_quantitative <- function(twitter_data, verbose = TRUE){
    quant_fields <- c(
    "engagements",
    "retweets",
    "likes",
    "user.profile.clicks",
    "url.clicks",
    "follows",
    "media.views",
    "media.engagements"
  )
  out <- vector("list", length(quant_fields))
  names(out) <- quant_fields
  
  for(fieldN in 1:length(quant_fields)){
    which_col <- which(colnames(twitter_data) == quant_fields[fieldN])
    tweet_col <- match(c("Tweet"),colnames(twitter_data))
    tmp <- twitter_data[order(twitter_data[,which_col],decreasing = TRUE)[1:5], c(tweet_col,which_col)]
    if(!is.null(dim(tmp))){
      # tmp <- rbind(tmp, c("", ""))
      # rownames(tmp) <- c(1:5, "")
      rownames(tmp) <- 1:5
      if (verbose) knitr::kable(tmp, row.names = FALSE, align = "l")
      out[[fieldN]] <- tmp
    }
    
    # print(xtable::xtable(tmp[1:5, c(Tweet_col,which_col)],digits = 0))
    # print(xtable::xtable(anova_condition_lac,digits = 6), file = file_lac_tex,append = T)
    # write("\nPOST HOC: Tukey multiple comparisons of means\n95% family-wise confidence level\n",file = file_lac_tex,append = T)
    # print(xtable::xtable(anova_condition_lac_posthoc,digits = 6), file = file_lac_tex,append = T)
  }
  return(out)
}

