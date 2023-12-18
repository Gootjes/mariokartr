# Player Rating system as implemented for Mario Kart (Wii)
Biases players who have played more games (and played well in those games).

```r
r <- Ranking(c("a", "b", "c"))

r$addPlayer("d")

r$processGame(c("c", "d", "a", "b")) # first place is c, second place is a, ...

r$ranking() # Get the ranking

```
