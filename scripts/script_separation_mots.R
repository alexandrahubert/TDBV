j <- "What kind of cheese isn't your cheese? (wonder) Nacho cheese! (groan) (Laugh)"     

j1 <- ref_carhyce$localisation

stringr::word(j1, -1, sep = ' A ')
# Remove parenthesis
k <- substring(k, 2, nchar(k)-1)

