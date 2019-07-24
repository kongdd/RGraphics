figure2.1 <- function() {
    par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 1, 1))
    y <- rnorm(20)
    plot(y, type="p")
    plot(y, type="l")
    plot(y, type="b")
    plot(y, type="h")
}

figure2.2 <- function() {
    par(mfrow=c(2, 2), cex=0.6, mar=c(4, 4, 4, 2), mex=0.8)
    plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings),
         id.n=1, cex.caption=0.8, which=1:4,
         panel=function(...) { panel.smooth(..., col.smooth="gray") })
}

figure2.3 <- function() {
    subset <- sample(1:150, 20)
    cS <- as.character(Sp <- iris$Species[subset])
    cS[Sp == "setosa"] <- "S"
    cS[Sp == "versicolor"] <- "V"
    cS[Sp == "virginica"] <- "g"
    ai <- cluster::agnes(iris[subset, 1:4])

    par(mfrow=c(2, 1), cex=0.5, pty="s")
    plot(ai, which=1, col=c("gray90", "gray"), labels = cS)
    plot(ai, which=2, labels = cS)
}

figure2.4 <- function() {
    plottitle <- function(plotfun, funarg, outer=FALSE, cex=.7, line=1) {
        ncp <- nchar(plotfun)
        nca <- nchar(funarg)
        mtext(paste(plotfun, "(", 
                    paste(rep(" ", nca), collapse=""),
                    ")", sep=""),
              family="mono", cex=cex, line=line, font=2, outer=outer)
        mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                    funarg, " ", sep=""),
              family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
    }
    plot2title <- function(plotfun, funarg, 
                           extrafn, extraarg, 
                           outer=FALSE, cex=.7, line=.5) {
        ncp <- nchar(plotfun)
        nca <- nchar(funarg)
        ncep <- nchar(extrafn)
        ncea <- nchar(extraarg)
        mtext(paste(plotfun, 
                    "(",  paste(rep(" ", nca), collapse=""),
                    ")\n", 
                    extrafn, "(",
                    paste(rep(" ", ncea), collapse=""),
                    ")", sep=""),
              family="mono", cex=cex, line=line, font=2, outer=outer)
        mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                    funarg, " \n", 
                    paste(rep(" ", ncep + 1), collapse=""),
                    extraarg, " ", sep=""),
              family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
    }
    dohplot <- function(plotfn, ..., funarg, 
                        extrafn=NULL, extraarg=NULL, namefudge=FALSE,
                        main="", xlab="", ylab="", axes=FALSE, box=TRUE) {
        if (is.null(xlab) || is.null(ylab)) {
            do.call(plotfn,
                    list(..., main=""))
        } else if (is.null(axes)) {
            do.call(plotfn,
                    list(..., main="", xlab="", ylab=""))
        } else {
            do.call(plotfn,
                    list(..., main="", axes=FALSE, xlab="", ylab=""))
        }
        if (is.null(extrafn)) {
            plottitle(plotfn, funarg)
        } else {
            plot2title(if (namefudge) paste(" ", plotfn, sep="") else plotfn, 
                       funarg, extrafn, extraarg)
        }
        if (box)
            box() # col="gray")
    }

    par(mfrow=c(3, 4), mar=c(1, 1, 3, 1), mex=.7, mgp=c(3, 100, 100))
    dohplot("plot", (1:10)^2, funarg="numeric",
            xlim=c(0, 11), ylim=c(-10, 110))
    dohplot("plot", table(rep(1:3, 1:3)), funarg="table", 
            lwd=2, xlim=c(0, 4), ylim=c(0, 4))
    # Empty
    plot.new()
    plot.new()
    dohplot("barplot", table(rep(1:3, 1:3)), funarg="", 
            extrafn="plot", extraarg="factor",
            xlim=c(-1, 5), ylim=c(0, 4), names.arg="")
    dohplot("pie", c(1, 2, 4), funarg="", col=gray(1:3/4), cex=.7,
            labels="", axes=NULL)
    dohplot("dotchart", 1:3, 
            funarg="numeric", pch=21, bg="gray",  
            lcolor="black", xlim=c(0, 4))
    # Empty
    plot.new()
    dohplot("boxplot", (1:10)^2, funarg="numeric", 
            col="gray", ylim=c(-10, 110))
    dohplot("hist", (1:100)^2, funarg="", col="gray", 
            breaks=6,
            xlim=c(-1000, 11000), ylim=c(0, 50))
    dohplot("stripchart", (1:10)^2, funarg="numeric", 
            method="stack",
            cex=1, xlim=c(-10, 110), ylim=c(-1, 3), pch=21, bg="gray")
    # stem()
    plot.new()
    txt <- capture.output(stem((1:10)^2))[-2]
    text(.05, (1:length(txt))/(length(txt) + 1), txt, adj=0, family="mono", cex=.7)
    box() # col="gray")
    plottitle("stem", "")
}

figure2.5 <- function() {
    plottitle <- function(plotfun, funarg, outer=FALSE, cex=.7, line=1) {
        ncp <- nchar(plotfun)
        nca <- nchar(funarg)
        mtext(paste(plotfun, "(", 
                    paste(rep(" ", nca), collapse=""),
                    ")", sep=""),
              family="mono", cex=cex, line=line, font=2, outer=outer)
        mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                    funarg, " ", sep=""),
              family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
    }
    plot2title <- function(plotfun, funarg, 
                           extrafn, extraarg, 
                           outer=FALSE, cex=.7, line=.5) {
        ncp <- nchar(plotfun)
        nca <- nchar(funarg)
        ncep <- nchar(extrafn)
        ncea <- nchar(extraarg)
        mtext(paste(plotfun, 
                    "(",  paste(rep(" ", nca), collapse=""),
                    ")\n", 
                    extrafn, "(",
                    paste(rep(" ", ncea), collapse=""),
                    ")", sep=""),
              family="mono", cex=cex, line=line, font=2, outer=outer)
        mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                    funarg, " \n", 
                    paste(rep(" ", ncep + 1), collapse=""),
                    extraarg, " ", sep=""),
              family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
    }
    dohplot <- function(plotfn, ..., funarg, 
                        extrafn=NULL, extraarg=NULL, namefudge=FALSE,
                        main="", xlab="", ylab="", axes=FALSE, box=TRUE) {
        if (is.null(xlab) || is.null(ylab)) {
            do.call(plotfn,
                    list(..., main=""))
        } else if (is.null(axes)) {
            do.call(plotfn,
                    list(..., main="", xlab="", ylab=""))
        } else {
            do.call(plotfn,
                    list(..., main="", axes=FALSE, xlab="", ylab=""))
        }
        if (is.null(extrafn)) {
            plottitle(plotfn, funarg)
        } else {
            plot2title(if (namefudge) paste(" ", plotfn, sep="") else plotfn, 
                       funarg, extrafn, extraarg)
        }
        if (box)
            box() # col="gray")
    }

    set.seed(1500)
    # mgp draws the axes miles off the page
    par(mfrow=c(4, 4), mar=c(1, 1, 3, 1), mex=.7, mgp=c(3, 100, 100))
    dohplot("plot", 1:10, (1:10)^2, funarg="num,num", 
            pch=21, bg="gray", 
            xlim=c(0, 11), ylim=c(-10, 110))
    x <- rnorm(10000)
    dohplot("smoothScatter", x, x + rnorm(10000)/3,  funarg="",
            nbin=64, colramp=function(n) { gray(n:1/(n + 1)) },
            xlim=c(-5, 5), ylim=c(-5, 5))
    x <- sample(1:4, 20, replace=TRUE)
    y <- x + sample(0:1, 20, replace=TRUE)
    dohplot("sunflowerplot", x, y,
            funarg="", seg.col="black", size=.07,
            xlim=c(0, 5), ylim=c(0, 6))
    # Empty gap 
    plot.new()
    dohplot("boxplot", list((1:10)^2, 120 - (1:10)^2), funarg="list", 
            extrafn="plot", extraarg="fac,num", col="gray", boxwex=0.5,
            ylim=c(-10, 130))
    dohplot("barplot", rbind(1:3, (1:3)^2), funarg="matrix",
            xlim=c(0, 4), ylim=c(0, 13))
    dohplot("barplot", rbind(1:3, (1:3)^2), funarg="matrix",
            beside=TRUE,
            xlim=c(0, 10), ylim=c(0, 11))
    # Empty gap for dotchart
    plot.new()
    fig <- par("fig")
    dohplot("stripchart", list((1:10)^2, 140 - (1:10)^2), funarg="list",
            extrafn="plot", extraarg="num,fac",
            xlim=c(-10, 150), ylim=c(0, 3), pch=21, bg="gray", cex=1)
    dohplot("spineplot", 
            rep(1:3, each=6), 
            factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))),
            funarg="num,fac", box=FALSE)
    dohplot("cdplot", 
            rep(1:3, each=6), 
            factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))),
            funarg="", box=FALSE)
    # Empty gap 
    plot.new()
    dohplot("spineplot", 
            factor(rep(1:3, each=6)), 
            factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3))),
            funarg="fac,fac", off=5,
            extrafn="plot", extraarg="fac,fac",
            namefudge=TRUE,
            box=FALSE)
    dohplot("assocplot", 
            table(rep(1:2, each=3),
                  c(rep(1:2, 1:2), rep(1:2, 2:1))),
            funarg="",
            col=c("black", "gray"), axes=NULL)
    dohplot("fourfoldplot", 
            table(rep(1:2, each=3),
                  c(rep(1:2, 1:2), rep(1:2, 2:1))),
            color=gray(1:2/3),
            # NOTE: can't make 'space' too small or font size of labels
            # goes to zero and get error from ghostscript
            funarg="", xlab=NULL, box=FALSE, space=0.03)
    dohplot("mosaicplot", 
            table(factor(rep(1:3, each=6)), 
                  factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3)))),
            funarg="", off=10,
            extrafn="plot", extraarg="table",
            cex.axis=.1, box=FALSE)
    # Put dotchart in gap
    par(fig=c(fig[1] - .1, fig[2:4]), new=TRUE)
    dotdata <- rbind(1:3, (1:3)^2) # rbind(table(gy), table(gx))
    colnames(dotdata) <- rep("", 3)
    dohplot("dotchart", dotdata, funarg="matrix",
            labels="", pch=c(16, 21), bg="gray",
            lcolor="black", xlim=c(0, 13), box=FALSE)
}

figure2.6 <- function() {
    plottitle <- function(plotfun, funarg, outer=FALSE, cex=.7, line=1) {
        ncp <- nchar(plotfun)
        nca <- nchar(funarg)
        mtext(paste(plotfun, "(", 
                    paste(rep(" ", nca), collapse=""),
                    ")", sep=""),
              family="mono", cex=cex, line=line, font=2, outer=outer)
        mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                    funarg, " ", sep=""),
              family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
    }
    plot2title <- function(plotfun, funarg, 
                           extrafn, extraarg, 
                           outer=FALSE, cex=.7, line=.5) {
        ncp <- nchar(plotfun)
        nca <- nchar(funarg)
        ncep <- nchar(extrafn)
        ncea <- nchar(extraarg)
        mtext(paste(plotfun, 
                    "(",  paste(rep(" ", nca), collapse=""),
                    ")\n", 
                    extrafn, "(",
                    paste(rep(" ", ncea), collapse=""),
                    ")", sep=""),
              family="mono", cex=cex, line=line, font=2, outer=outer)
        mtext(paste(paste(rep(" ", ncp + 1), collapse=""),
                    funarg, " \n", 
                    paste(rep(" ", ncep + 1), collapse=""),
                    extraarg, " ", sep=""),
              family="mono", col="gray60", cex=cex, line=line, font=2, outer=outer)
    }
    dohplot <- function(plotfn, ..., funarg, 
                        extrafn=NULL, extraarg=NULL, namefudge=FALSE,
                        main="", xlab="", ylab="", axes=FALSE, box=TRUE) {
        if (is.null(xlab) || is.null(ylab)) {
            do.call(plotfn,
                    list(..., main=""))
        } else if (is.null(axes)) {
            do.call(plotfn,
                    list(..., main="", xlab="", ylab=""))
        } else {
            do.call(plotfn,
                    list(..., main="", axes=FALSE, xlab="", ylab=""))
        }
        if (is.null(extrafn)) {
            plottitle(plotfn, funarg)
        } else {
            plot2title(if (namefudge) paste(" ", plotfn, sep="") else plotfn, 
                       funarg, extrafn, extraarg)
        }
        if (box)
            box() # col="gray")
    }

    # mgp draws the axes miles off the page
    par(mfrow=c(3, 4), mar=c(1, 1, 3, 1), mex=.7, mgp=c(3, 100, 100))
    mdf <- cbind(3:6, (3:6)^2, (3:6)^3)
    names(mdf) <- c("Y1", "Y2", "Y3")
    aaa <- seq(0, pi, length=10)
    xxx <- rep(aaa, 10)
    yyy <- rep(aaa, each=10)
    zzz <- sin(xxx) + sin(yyy)
    # gap for pairs(matrix)
    plot.new()
    dohplot("matplot", mdf[order(mdf[, 1]), ], funarg="", 
            pch=21:23, bg=c("white", "black", "gray"), type="o",
            col="black", xlim=c(0, 6), ylim=c(-10, 230))
    df <- USJudgeRatings[1:6, ]
    rownames(df) <- NULL
    dohplot("stars", df, funarg="", ncol=2, lwd=1,
            len=.8, col.stars=rep("gray", 13), mar=c(1, 1, 3, 1))
    # gap
    plot.new()
    dohplot("image", matrix(zzz, ncol=10), funarg="", col=gray(1:12/13))
    dohplot("contour", matrix(zzz, ncol=10), funarg="", 
            levels=seq(0, 2, .25), labcex=.4)
    # gap for filled.contour(matrix)
    plot.new()
    dohplot("persp", matrix(zzz, ncol=10), funarg="",
            theta=30, phi=45, col="gray")
    dohplot("symbols", xxx, yyy, funarg="",
            circles=zzz, inches=.03, axes=NULL)
    # gap for coplot(y ~ x | g)
    plot.new()
    dohplot("mosaicplot", 
            table(factor(rep(1:3, each=6)), 
                  factor(c(rep(1:3, 3:1), rep(1:3, 2), rep(1:3, 1:3)))),
            funarg="", cex.axis=.1, off=10,
            box=FALSE)
}

figure2.7 <- function() {
    par(mfrow=c(2, 2), mar=c(2.5, 2, 1, 1), cex=0.6)
    boxplot(decrease ~ treatment, data = OrchardSprays,
            log = "y", col="light gray")
    boxplot(decrease ~ treatment, data = OrchardSprays,
            log = "y", col="light gray", 
            boxwex=0.5)

    par(las=2, xpd=NA)
    barplot(VADeaths[1:2,], angle = c(45, 135), 
            density = 20, col = "gray",
            names=c("RM", "RF", "UM", "UF"))
    barplot(VADeaths[1:2,], angle = c(45, 135), 
            density = 20, col = "gray",
            names=c("RM", "RF", "UM", "UF"),
            horiz=TRUE)
}

figure2.8 <- function() {
    par(mfrow=c(2, 2), mar=c(2, 2, 1, 1), cex=0.6)
    y <- rnorm(20)
    plot(y, type="l", lwd=3)
    plot(y, type="l", col="gray")
    plot(y, type="l", lty="dashed")
    plot(y, type="l", ylim=c(-4, 4))
}

figure2.9 <- function() {
    par(cex=.5)
    plot(function(x) { 
        sin(x)/x 
    }, 
    from=-10*pi, to=10*pi, 
    xlab="", ylab="", n=500)

    par(mfrow=c(1, 2))
    par(mar=c(7, 0, 3, 1))
    par(mex=0.7)

    hc <- hclust(dist(USArrests), "ave")
    dend1 <- as.dendrogram(hc)
    dend2 <- cut(dend1, h=70)
    par(cex=0.7)
    par(mar=c(1, 0, 2, 8))
    #  dend2$lower is *NOT* a dendrogram, but a list of .. :
    plot(dend2$lower[[3]], 
         horiz = TRUE, type = "tr", axes=FALSE, cex=0.8)
    par(mar=c(8, 0, 2, 0))
    # "inner" and "leaf" edges in different type & color :
    plot(dend2$lower[[2]], 
         edgePar = list(col=c("black", "gray")), edge.root=TRUE, 
         axes=FALSE, cex=0.8)
}
