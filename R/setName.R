setName = function(x, newName) {
  colnames(x$trw) <- newName
  x$name <- newName
  return(x)
}
