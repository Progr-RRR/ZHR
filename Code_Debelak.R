OR_calc <- function(resp) { # resp ist eine 0-1-Matrix; Die Zeilen sind Personen, die Spalten Aufgaben
  ## check validity of argument(s):
  stopifnot(is.matrix(resp), is.numeric(resp))
  i <- ncol(resp)
  r <- apply(resp, 1, sum) # Summenscore aller Personen
  OR <- array(dim=c(i,i))  # = matrix(, i, i)
  for (a in 1:i) {
    for(b in a:i) {
      ## MM: nice example of using logicals  as 1_{..} indicators, i.e. 0/1 variables: Counting "events":
      zaehler <- sum((resp[,a] == 1) * (resp[,b] == 1)) * sum((resp[,a] == 0) * (resp[,b] == 0))
      nenner  <- sum((resp[,a] == 1) * (resp[,b] == 0)) * sum((resp[,a] == 0) * (resp[,b] == 1))
      if (nenner > 0) {
        OR[a,b] <- zaehler / nenner
      } else {
        OR[a,b] <- 1
      }
      ## if(a != b)
      OR[b,a] <- OR[a,b]
    }
  }
  ## return
  OR
}

MH_calc <- function(resp) { # resp ist eine 0-1-Matrix; Die Zeilen sind Personen, die Spalten Aufgaben
  i <- ncol(resp)
  MH <- array(dim=c(i,i))
  for (a in 1:(i-1)) {
    for(b in (a+1):i) {
      zaehler <- 0
      nenner <- 0
      ## Restscore aller Personen, d.h. ohne Spalten a und b :
      r <- apply(resp[,c(-a,-b)], 1, sum)
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

## Local Variables:
## mode: R
## ess-indent-offset: 2
## End:
