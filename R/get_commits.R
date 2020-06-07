#' get df of recruits based on certain paramters
#' 
#' @param college college name (to avoid confusion like Southern Cal and South Carolina, which would both be USC)
#' @param year the year of the recruits (it works with any year 247 has data for)
#'
#' 
#' @return data.frame
#' 
#' @import dplyr
#' @import rvest
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import sys
#' 
#' @export

get_commits <- function(college,year){
  
  college <- college %>% tolower() %>% str_replace(" ","-")
  
  build_html <- paste("https://247sports.com/college/",
                      college,
                      "/Season/",
                      year,
                      "-Football/Commits/",
                      sep="")
  
  commits <- read_html(build_html) %>% 
    html_nodes(".recruit") %>%
    html_text() %>%
    data.frame() 
  
  colnames(commits) <- c("commit")
  
  commits <- commits %>%
    separate(commit,into=c("Player","HS"),sep = "  ",extra="merge") %>% 
    separate(HS, into=c("HS","Hometown"), sep="[(]")
  
  commits$Hometown <- str_replace(commits$Hometown,"[)]","")
  
  Sys.sleep(1)
  
  positions <- read_html(build_html) %>% 
    html_nodes('.position') %>% 
    html_text() %>% 
    data.frame()
  
  positions <- data.frame(positions[1:nrow(commits),])
  
  colnames(positions) <- c("positions")
  
  commits <- cbind(commits,positions)
  
  Sys.sleep(1)
  
  score <- read_html(build_html) %>% 
    html_nodes('.score') %>% 
    html_text() %>% 
    data.frame() 
  
  colnames(score) <- c("score")
  
  score <- data.frame(score[!(score$score=="Rating"),])
  
  score <- data.frame(score[1:nrow(commits),])
  
  colnames(score) <- c("score")
  
  commits <- cbind(commits,score)
  
  commits$score <- as.numeric(as.character(commits$score))
  
  commits$stars <- case_when(
    commits$score > 0.98 ~ 5,
    commits$score > 0.9 ~ 4,
    commits$score > 0.79 ~ 3,
    commits$score > 0.7 ~ 2,
    TRUE ~ 1
  )
  
  commits$commit_status <- college
  
  return(commits)
  
}