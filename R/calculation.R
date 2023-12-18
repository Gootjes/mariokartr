
# https://github.com/riidefi/mkw/blob/master/source/game/system/Rating.cpp
spline_terms = c(0, 1, 8, 50, 125);
calc <- function(x, unused) {
  term = -2;
  ret = 0.0;
  for (i in (seq_len(9) - 1)) {
    termIdx = term;
    if (term < 0)
      termIdx = 0;
    if (term > 4)
      termIdx = 4;

    y = x - term;
    if (y < 0.0)
      y = -y;

    b = 0.0;
    if (y >= 0.0) {
      if (y < 1.0) {
        b = (3.0 * y * y * y - 6.0 * y * y + 4.0) / 6.0;
      } else if (y <= 2.0) {
        y = y - 2.0;
        b = (-y * y * y) / 6.0;
      }
    }
    term = term + 1;
    ret = ret + (b*spline_terms[termIdx + 1]); # r is 1-based
  }

  ret
}

calcSpline <- function(x, term) {
  y = x - 2.0;
  b = (-y * y * y) / 6.0;

  b*spline_terms[term + 1]; # r is 1-based
}

calcPoints <- function(difference, isPos) {
  min = -9998;
  if (difference < min) {
    difference = min;
  }
  max = 9998;
  if (difference > max) {
    difference = max;
  }
  if (!isPos) {
    difference = -difference;
  }

  uvar2 = (difference) + 9998;
  ivar1 = uvar2 / 5000;

  x = (1.0/4999) * uvar2;
  points = calc(x, ivar1);

  if(isPos) {
    points
  } else {
    -points
  }
}

sub <- function(a, b) b - a
calcPosPoints <- function(thisPoints, opponentPoints) {
  calcPoints(sub(thisPoints, opponentPoints), TRUE);
}
calcNegPoints <- function(thisPoints, opponentPoints) {
  calcPoints(sub(thisPoints, opponentPoints), FALSE);
}
