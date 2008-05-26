Ogata <- scan("Ogata.txt", what = list(magnitude = 0,
                 time = 0), skip = 2, sep = ',', quiet=TRUE)
Ogata <- as.data.frame(Ogata)
Ogata <- Ogata[1:nrow(Ogata)-1, ]
Ogata$magnitude <- Ogata$magnitude - 3.5

