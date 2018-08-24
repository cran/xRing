rmNA = function(x) {
  as.data.frame(x[apply(x, 1,
                        function(x)
                          !all(is.na(x))), , drop = FALSE])
}
