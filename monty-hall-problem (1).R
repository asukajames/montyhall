#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Selecting an initial door randomly
#' 
#' @description
#' select_door randomly samples a vector that shows the possible door options for contestant can choose.
#' 
#' @details
#' This function replicates the game show, "Let's Make a Deal" by selecting one of the three doors that a contestant can choose.
#' This function randomly selects a door that will be used to test if switching or changing the door strategy works better.  
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#' The function returns a length 1 numeric vector indicating which door that the contestant initially chose.
#' 
#' @examples
#' select_door()
#'
#'  To test this code simply use it several times.
#'  select_door()  
#'  select_door()
#'  select_door()
#'  
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens one of the doors. 
#' 
#' @description
#' `open_goat_door` selects a door with a goat behind it to reveal to the
#'  contestant.
#'  
#' @details
#'  This function replicates that the host opens one of the doors with a goat behind and that
#'  contestant did not pick. With this information, contestant can choose whether 
#'  they want to stick with their initial choice or to switch the door in the next function.
#'
#' @param 
#'  This function has two arguments. 
#'  1. game is inputted by the create game function 
#'  2. a.pick is inputted by the select door function.
#'  
#' @return 
#'  This function returns a length of 1 numeric vector
#'  indicating which goat door is revealed to the contestant. 
#' 
#' @examples
#' open_goat_door <- function( game, a.pick )
#' To test the function
#' this.game <- create_game()
#' this.game  
#' my.initial.pick <- select_door()
#' my.initial.pick
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' The contestant decides if they switch or stick with their initial choice.  
#' 
#' @description
#' 'change_door' enables the contestant to decide if they want to switch or keep their initial choice.
#' 
#' @details
#'  This function replicates that contestant is allowed to stay with their initial pick or 
#'  switch their initial pick to the unopened door. 
#'  
#' @param 
#' This function has three arguments:
#'  1. stay = TRUE or FALSE 
#'  2. opened.door that is inputted by the goat door function 
#'  3. a.pick is inputted by the select door function
#'
#' @return 
#' This function returns a length of 1 numeric vector
#'  indicates the contestant's final pick.
#'  
#' @examples
#' #'  opened.door <- open_goat_door( this.game, my.initial.pick )
#' change_door( stay=T, 
#' opened.door=opened.door, 
#' a.pick=my.initial.pick )
#' 
#' change_door( stay=F, 
#' opened.door=opened.door, 
#' a.pick=my.initial.pick )
#' my.final.pick <- change_door( stay=F, 
#' opened.door=opened.door, 
#' a.pick=my.initial.pick )
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine If Contestant Has Won
#' 
#' @description
#' 'determine_winner' shows if the contestant has won this game or not. 
#' 
#' @details
#' This function replicates if the contestant final pick was a goat's or car's door.
#' 
#' @param 
#' This function has two arguments: 
#'  1. final pick is inputted by the change door function
#'  2. game is inputted by the create game function 
#'  
#' @return
#' This function returns a length of 1 character vector 
#' showing the result of the game. 
#' 
#' @examples
#' my.final.pick <- change_door( stay=T, 
#' opened.door=opened.door, 
#' a.pick=my.initial.pick )
#' 
#' determine_winner( final.pick=my.final.pick, game=this.game )
#' 
#' my.final.pick <- change_door( stay=F, 
#' opened.door=opened.door, 
#' a.pick=my.initial.pick )
#' 
#' determine_winner( final.pick=my.final.pick, game=this.game )
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Determining the Outcomes for Strategies 
#' 
#' @description
#' It shows the game result.
#' 
#' @details
#' This function shows the outcomes for each strategy. 
#' 
#' @param ... no arguments are used by this function. 
#' 
#' @return 
#' This function returns the data frame which has two columns and rows;
#'  strategy the contestant used and the outcomes. 
#'  
#' @examples
#' play_game( )
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' The Game Simulation 
#' 
#' @description
#' This function can simulate the strategies. 
#'  In order to get the most accurate outcomes, the function simulates 100 times. 
#'  
#' @details
#' This function can simulate n times. 
#' 
#' @param 
#' n = the number of games
#' 
#' @return 
#' This function shows the winning rate for each strategy. 
#' 
#' @examples
#' play_n_games <- function( n=100 )
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
