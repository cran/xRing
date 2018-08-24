
rollMax =  function(x, k, align = c("center", "left", "right"), fill=NA) {
  n_s <- length(x) - k + 1
  out <- rep(NA, n_s)
  for (i in seq(1, n_s )) { # seq_len
    out[i] <- max(x[i + 0:(k - 1)])
  }
  out <- switch(
    match.arg(align),
    "left" = {
      c(out, rep(fill, k - 1))
    },
    "center" = {
      c(rep(fill, floor((k - 1) / 2)), out, rep(fill, ceiling((k - 1) / 2)))
    },
    "right" = {
      c(rep(fill, k - 1), out)
    }
  )
  return(out)
}

