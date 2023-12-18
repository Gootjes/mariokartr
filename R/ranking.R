#' @title Create a Ranking
#' Uses Mario Kart Wii rating system
#'
#' @examples
#' r <- Ranking(c("a", "b", "c"))
#' r$addPlayer("d")
#'
#' r$processGame(c("c", "d", "a", "b")) # first place is c, second place is d, ...
#'
#' r$ranking() # Get the ranking
#'
#' @references https://wiki.tockdom.com/wiki/Player_Rating
#' @export
Ranking <- function(players = NULL, defaultScore = 5000) {

  private <- environment()

  private$PLAYERS <- c()
  private$GAMES_PLAYED <- c()

  public <- new.env()

  public$playerExists <- function(player) {
    player %in% names(private$PLAYERS)
  }

  public$getPlayerPoints <- function(player) {
    if(missing(player)) return(private$PLAYERS)
    if(!public$playerExists(player)) {
      stop(paste("Player does not exist: ", player))
    }
    private$PLAYERS[[player]]
  }

  public$addPlayer <- function(name) {
    if (name %in% names(private$PLAYERS)) {
      stop(paste("Player already exists:", name))
    }
    private$PLAYERS[name] <- defaultScore
    private$GAMES_PLAYED[name] <- 0
  }

  public$addPlayerPoints <- function(player, points) {
    private$PLAYERS[player] <- public$getPlayerPoints(player) + points
  }

  public$ensurePlayerExists <- function(player) {
    if(!public$playerExists(player)) {
      public$addPlayer(player)
    }
  }

  public$processGame <- function(result, implicitlyAddPlayers = TRUE) {

    if(implicitlyAddPlayers) {
      for(n in result) public$ensurePlayerExists(n)
    }

    for(n in result) {
      private$GAMES_PLAYED[[n]] <- private$GAMES_PLAYED[[n]] + 1
    }

    playerCount <- length(result)
    POINTS = rep(0, 12)
    # pointsIdx = playerCount - 1
    for (i in (seq_len(playerCount))) {
      playerPosition = i
      playerIdentity = result[[i]]
      totalRating = 0.0
      for (j in (seq_len(playerCount))) {
        if (i != j) {
          opponentIdentity = result[[j]]

          ahead = FALSE
          behind = FALSE

          opponentPosition = j
          if (playerPosition < opponentPosition) {
            ahead = TRUE
          } else if(playerPosition > opponentPosition) {
            behind = TRUE
          }

          if (ahead) {
            totalRating = totalRating + calcPosPoints(public$getPlayerPoints(playerIdentity), public$getPlayerPoints(opponentIdentity))
          } else if (behind) {
            totalRating = totalRating + calcNegPoints(public$getPlayerPoints(playerIdentity), public$getPlayerPoints(opponentIdentity))
          }
        }

      }

      points = totalRating
      if(playerPosition == 1) {
        if (points < 1) {
          points = 1
        }
      }
      POINTS[i] = points

      # GP scoring
      # addedPoints = VS_POINT_DISTRIBUTION[pointsIdx + 1][playerPosition - 1 + 1] # R is 1-based
      # setPlayerPoints(player, playerPreviousPoints + addedPoints)
    }

    for (i in (seq_len(playerCount))) {
      playerIdentity <- result[[i]]
      public$addPlayerPoints(playerIdentity, POINTS[i])
    }

  }

  public$processGames <- function(x,
                                  gameIdVar = "Race",
                                  playerIdVar = "Name",
                                  playerPositionVar = "Rank") {
    for(race in sort(unique(x[[gameIdVar]]))) {
      x_race <- x[x[[gameIdVar]] == race,]
      x_race[] <- x_race[order(x_race[[playerPositionVar]]),]

      public$processGame(x_race$Name)
    }
  }

  public$ranking <- function() {
    if(length(private$PLAYERS) == 0) stop("No players")
    x <- data.frame(name = names(private$PLAYERS),
               score = private$PLAYERS,
               games = private$GAMES_PLAYED,
               row.names = NULL)

    x[] <- x[order(x$score, decreasing = T),]

    x$rank <- 1:nrow(x)

    x
  }

  public$players <- function() {
    names(private$PLAYERS)
  }

  class(public) <- c("mariokartscore", class(public))

  if(!is.null(players)) {
    for(player in players) {
      public$addPlayer(player)
    }
  }

  public
}

#' @export
print.mariokartscore <- function(x) {
  if (length(x$players()) > 0) {
    print(x$ranking())
  } else {
    cat("No players", "\n", sep = "")
  }
}

