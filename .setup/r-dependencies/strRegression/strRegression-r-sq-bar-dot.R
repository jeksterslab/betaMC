.RSqBar <- function(rsq,
                    k,
                    n) {
  return(
    1 - (1 - rsq) * (
      (n - 1) / (n - k)
    )
  )
}
