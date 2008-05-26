NthChina <- scan("NthChina.txt", what=list(time=0,
                 latitude=0, longitude=0, magnitude=0,
                 region=0), skip=1, quiet=TRUE)
NthChina <- as.data.frame(NthChina)
NthChina$magnitude <- NthChina$magnitude - 6
NthChina$time <- NthChina$time - 1480
