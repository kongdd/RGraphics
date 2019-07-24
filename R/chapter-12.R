
figure12.1 <- function() {
    TitanicDF <- as.data.frame(Titanic)
    TitanicList <- lapply(TitanicDF[1:4], rep, TitanicDF$Freq)
    TitanicSets <- 
        data.frame(passenger=TitanicList$Class != "Crew",
                   adult=TitanicList$Age == "Adult",
                   male=TitanicList$Sex == "Male",
                   survivor=TitanicList$Survived == "Yes")
    head(TitanicSets)

    gplots::venn(TitanicSets[1:2])

    par(mar=rep(2, 4))
    plot(venneuler::venneuler(TitanicSets[1:2]), 
         col=hcl(0, 0, c(60, 80), .5),
         alpha=NA, border="black")

    gplots::venn(TitanicSets[1:3])

    par(mar=rep(2, 4))
    plot(venneuler::venneuler(TitanicSets[1:3]), 
         col=hcl(0, 0, seq(40, 80, 20), .5),
         alpha=NA, border="black")

    pdf("Figures/special-venn-3-%d.pdf", onefile=FALSE,
        width=6, height=6)
    gplots::venn(TitanicSets)

    dev.off()
    png("Figures/special-venn-3-%d.png", width=240, height=240, pointsize=8)
    gplots::venn(TitanicSets)

    dev.off()

    par(mar=rep(2, 4))
    plot(venneuler::venneuler(TitanicSets[1:4]), 
         col=hcl(0, 0, seq(20, 80, 20), .5),
         alpha=NA, border="black")
}

figure12.2 <- function() {
    TeachingDemos::faces(USJudgeRatings[1:5, ], nrow=1, ncol=5)
    TeachingDemos::faces2(USJudgeRatings[1:5, ], nrows=1, ncols=5, scale="all")

    symbols::symbol(USJudgeRatings[1:25, ], type="face")
}

figure12.3 <- function() {
    data("soil", envir=environment())
    place2region <- as.data.frame(rbind(c("Cnt1", "Lima"),
                                        c("Cnt2", "Lima"),
                                        c("Cnt3", "Lima"),
                                        c("Chz", "Ancash"),
                                        c("Chmar", "Huanuco"),
                                        c("Hco1", "Huanuco"),
                                        c("Hco2", "Huanuco"),
                                        c("Hco3", "Huanuco"),
                                        c("Hyo1", "Junin"),
                                        c("Hyo2", "Junin"),
                                        c("Namora", "Cajamarca"),
                                        c("SR1", "Junin"),
                                        c("SR2", "Junin")))
    soils <- merge(soil, place2region,
                   by.x="place", by.y="V1")[c("sand", "slime", "clay")]
    names(soils) <- c("sand", "silt", "clay")

    vcd::ternaryplot(soils, col="black", 
                     grid_color="black", labels_color="black")

    plotrix::triax.plot(soils,  cex.ticks=.5)

    TTsoils <- soils
    names(TTsoils) <- c("SAND", "SILT", "CLAY")
    soiltexture::TT.plot(tri.data=TTsoils)
}

figure12.4 <- function() {
    hourSpeed <- aggregate(RGraphics::hourlySpeed["Speed"], 
                           list(hour=RGraphics::hourlySpeed$hour),
                           mean)
    head(hourSpeed)

    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    print(
        with(RGraphics::wind9am,
             polarFreq(data.frame(ws=Speed, wd=Dir, date=Date),
                       cols=gray(10:1/11), border.col="black"))
    
    )

    plotrix::polar.plot(hourSpeed$Speed, hourSpeed$hour * 15,
                        start=90, clockwise=TRUE, lwd=5,
                        label.pos=seq(15, 360, 15), labels=1:24,
                        radial.lim=c(0, 4.5))
}

figure12.5 <- function() {
    station22254dir <- with(RGraphics::wind9am, Dir[Station == 22254])

    station22254 <- circular::circular(station22254dir, 
                                       units="degrees",
                                       zero=pi/2, rotation="clock")

    windHours <- circular::circular(RGraphics::hourlySpeed$hour,
                                    units="hours", 
                                    zero=pi/2, rotation="clock")

    par(mar=rep(2, 4), xpd=NA)
    plot(station22254, stack=TRUE, sep=.06)

    par(mar=rep(2, 4), xpd=NA)
    plot(density(station22254, bw=45), 
         main="", xlab="", ylab="")

    par(mar=rep(1, 4), xpd=NA)
    circular::rose.diag(station22254, bins=36, prop=3)

    par(mar=rep(1, 4), xpd=NA)
    plot(windHours, col=NA, shrink=1.2, axes=FALSE)
    lines(windHours, 
          0.5*RGraphics::hourlySpeed$Speed/max(RGraphics::hourlySpeed$Speed),
          nosort=TRUE, lty="dotted", join=FALSE)
    circular::axis.circular(template="clock24")
}

figure12.6 <- function() {

    print(
        with(RGraphics::wind9am,
             windRose(data.frame(ws=Speed, wd=Dir, 
                                 date=Date, station=factor(Station)),
                  
                      paddle=FALSE, type="station", width=2))
    )
}

figure12.7 <- function() {

    data("NHANES", envir=environment())
    plot(Serum.Iron ~ Transferin, NHANES)

    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    print(
        hexbin::hexbinplot(Serum.Iron ~ Transferin, NHANES)
    
    )

    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    print(
        hexbin::hexbinplot(Serum.Iron ~ Transferin | Sex, NHANES)
    )
}
