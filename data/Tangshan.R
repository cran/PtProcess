Tangshan <- scan("Tangshan.txt", what=list(latitude=0,
                 longitude=0, magnitude=0,
                 year=0, month=0, day=0, hour=0, minute=0,
                 second=0, time=0), skip=0, quiet=TRUE)

Tangshan <- as.data.frame(Tangshan)
