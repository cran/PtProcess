Phuket <- scan("Phuket.txt", what=list(
                latitude=0, longitude=0, depth=0,
                mb=0, Ms=0, magnitude=0,
                time=0, second=0, minute=0, hour=0,
                day=0, month=0, year=0),
                skip=0, quiet=TRUE)

Phuket <- as.data.frame(Phuket)

