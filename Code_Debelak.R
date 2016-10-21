OR_calc <- function(resp) { # resp ist eine 0-1-Matrix; Die Zeilen sind Personen, die Spalten Aufgaben
  i <- ncol(resp)
  r <- apply(resp, 1, sum) # Summenscore aller Personen
  OR <- array(dim=c(i,i))
  for (a in 1:i) {
    for(b in a:i) {
      zaehler <- sum((resp[,a] == 1) * (resp[,b] == 1)) * sum((resp[,a] == 0) * (resp[,b] == 0))
      nenner <- sum((resp[,a] == 1) * (resp[,b] == 0)) * sum((resp[,a] == 0) * (resp[,b] == 1))
      if (nenner > 0) {
        OR[a,b] <- zaehler / nenner
      } else {
        OR[a,b] <- 1 
      }
      OR[b,a] <- OR[a,b]
    }
  }
  return(OR)
}

MH_calc <- function(resp) { # resp ist eine 0-1-Matrix; Die Zeilen sind Personen, die Spalten Aufgaben
  i <- ncol(resp)
  MH <- array(dim=c(i,i))
  for (a in 1:(i-1)) {
    for(b in (a+1):i) {
      zaehler <- 0
      nenner <- 0
      r <- apply(resp[,c(-a,-b)], 1, sum) # Restscore aller Personen, d.h. ohne Spalten a und b
      for(c in 0:i) {
        if(sum(r==c) > 0) {
          zaehler <- zaehler + sum((resp[r==c,a] == 1) * (resp[r==c,b] == 1)) * sum((resp[r==c,a] == 0) * (resp[r==c,b] == 0))/sum(r==c)
          nenner <- nenner + sum((resp[r==c,a] == 1) * (resp[r==c,b] == 0)) * sum((resp[r==c,a] == 0) * (resp[r==c,b] == 1))/sum(r==c)
        }
      }
      if (nenner > 0) {
        MH[a,b] <- zaehler / nenner
      } else {
        MH[a,b] <- 1 
      }
      MH[b,a] <- MH[a,b]
    }
  }
  return(MH)
}