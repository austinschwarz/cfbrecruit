#' get df of players based on positions
#' 
#' @param position position of players
#' @param year HS grad year
#' 
#' @return data.frame of players of that position
#' 
#' @import dplyr
#' @import rvest
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import sys
#' 
#' @export

get_position <- function(position,years){
  
  players <- NULL
  
  position <- position %>% toupper()
  
  for (year in years){
    
    build_html <- paste("https://247sports.com/Season/",year,"-Football/CompositeRecruitRankings/?InstitutionGroup=Highschool&Position=",position,sep="")
  
    hs_players <- read_html(build_html) %>% 
      html_nodes(".recruit") %>%
      html_text() %>%
      data.frame() 
  
    colnames(hs_players) <- c("player")
  
    hs_players <- hs_players %>%
      separate(player,into=c("Player","HS"),sep = "  ",extra="merge") %>% 
      separate(HS, into=c("HS","Hometown"), sep="[(]")
  
    hs_players$Hometown <- str_replace(hs_players$Hometown,"[)]","")
  
    Sys.sleep(1)
  
    score <- read_html(build_html) %>% 
      html_nodes('.score') %>% 
      html_text() %>% 
      data.frame() 
  
    colnames(score) <- c("score")
  
    score <- score[!(score$score=="Rating"),]
  
    hs_players <- cbind(hs_players,score)
  
    hs_players$score <- as.numeric(as.character(hs_players$score))
  
    hs_players$stars <- case_when(
      hs_players$score > 0.98 ~ 5,
      hs_players$score > 0.9 ~ 4,
      hs_players$score > 0.79 ~ 3,
      hs_players$score > 0.7 ~ 2,
      TRUE ~ 1
    )
  
    commits <- read_html(build_html) %>% html_nodes('.status') %>% html_nodes("a") %>% html_attr("href")
    i <- 1
    j <- 1
    commit_status <- vector(mode = "character",length = nrow(hs_players))
  
    for (link in commits){
      first_letter <- link %>% substring(1,1)
      if (first_letter == "h"){
        url <- link %>% str_split("/")
        school <- url[[1]][5] %>% simpleCap()
        commit_status[j] <- school
        j = j+1
      }
      else {
        peek <- commits[i-1]
        peek_letter <- peek %>% substring(1,1)
        if (peek_letter == "/"){
          commit_status[j] <- "Undecided"
          j=j+1
        }
      }
      i = i+1
    }
  
    hs_players <- cbind(hs_players,commit_status)
    hs_players$year <- year
    
    players <- rbind(players,hs_players)
  
  }
  
  return(players)
  
}

#' helper function to format committs
#' 
#' @param school name of school
#' 
#' @import dplyr
#' @import stringr
#' 
#' @return string of corrected school name

simpleCap <- function(x) {
  x <- x %>% str_replace("-"," ")
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}