
figure5.1 <- function() {
    print(
        qplot(temperature, pressure, data=pressure)
    )
}

figure5.2 <- function() {
    print(
        qplot(temperature, pressure, data=pressure,
              main="Vapor Pressure of Mercury",
              geom=c("point", "line"))
    )
}

figure5.3 <- function() {
    # grid.newpage()
    layvp <- viewport(layout=grid.layout(1, 5,
                                         heights=unit(1, "inch"),
                                         widths=unit(c(1, .5), "inch")),
                      name="vplay")
    vpi <- function(i) {
        viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
    }
    pushViewport(layvp)
    pushViewport(viewport(layout.pos.col=1:5))
    grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
    popViewport()
    pushViewport(vpi(1))
    upViewport()
    pushViewport(vpi(3))
    upViewport()
    pushViewport(vpi(5))
    upViewport(0)
    for (i in c(1, 3, 5)) {
        grid.roundrect(height=.5,
                       vp=paste("vplay::vp", i, sep=""),
                       name=paste("rr", i, sep=""),
                       gp=gpar(fill="white"))
    }
    grid.text("data", vp="vplay::vp1")
    grid.text("aesthetic", vp="vplay::vp3")
    grid.text("geom", vp="vplay::vp5")
    arr <- arrow(length=unit(3, "mm"), type="closed")
    grid.segments(grobX("rr1", 0), .5,
                  grobX("rr3", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr3", 0), .5,
                  grobX("rr5", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
}

figure5.4 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_point(aes(x=disp, y=mpg))
    )

    print(
        p + geom_point(aes(x=disp, y=mpg, shape=gear),
                       size=4) +
            theme(legend.position="none")
    )

    print(
        p + geom_text(aes(x=disp, y=mpg, label=gear))
    )

    lmcoef <- coef(lm(mpg ~ disp, mtcars2))

    print(
        p + geom_point(aes(x=disp, y=mpg)) +
            geom_abline(intercept=lmcoef[1], slope=lmcoef[2])
    )
}

figure5.5 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_point(aes(x=disp, y=mpg)) +
            scale_y_continuous(name="miles per gallon") +
            scale_x_continuous(name="displacement (cu.in.)")
    )

    print(
        p + geom_point(aes(x=disp, y=mpg)) +
            scale_y_continuous(limits=c(0, 40)) 
    )

    print(
        p + geom_point(aes(x=disp, y=mpg, 
                           color=trans), size=4) +
            scale_color_manual(values=c(automatic=gray(2/3),
                                        manual=gray(1/3)))
    )
}

figure5.6 <- function() {
    # grid.newpage()
    layvp <- viewport(layout=grid.layout(1, 7,
                                         heights=unit(1, "inch"),
                                         widths=unit(c(1, .5), "inch")),
                      name="vplay")
    vpi <- function(i) {
        viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
    }
    pushViewport(layvp)
    pushViewport(viewport(layout.pos.col=1:7))
    grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
    popViewport()
    pushViewport(vpi(1))
    upViewport()
    pushViewport(vpi(3))
    upViewport()
    pushViewport(vpi(5))
    upViewport()
    pushViewport(vpi(7))
    upViewport(0)
    for (i in c(1, 3, 5, 7)) {
        grid.roundrect(height=.5,
                       vp=paste("vplay::vp", i, sep=""),
                       name=paste("rr", i, sep=""),
                       gp=gpar(fill="white"))
    }
    grid.text("data", vp="vplay::vp1")
    grid.text("scale", vp="vplay::vp3")
    grid.text("aesthetic", vp="vplay::vp5")
    grid.text("geom", vp="vplay::vp7")
    arr <- arrow(length=unit(3, "mm"), type="closed")
    grid.segments(grobX("rr1", 0), .5,
                  grobX("rr3", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr3", 0), .5,
                  grobX("rr5", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    # grid.curve(grobX("rr1", 27), grobY("rr1", 27), 
    #            grobX("rr5", 153), grobY("rr5", 153),
    #            square=FALSE, ncp=8, curvature=-.3,
    #            arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr5", 0), .5,
                  grobX("rr7", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
}

figure5.7 <- function() {
    # grid.newpage()
    layvp <- viewport(layout=grid.layout(1, 9,
                                         heights=unit(1, "inch"),
                                         widths=unit(c(1, .5), "inch")),
                      name="vplay")
    vpi <- function(i) {
        viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
    }
    pushViewport(layvp)
    pushViewport(viewport(layout.pos.col=1:9))
    grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
    popViewport()
    pushViewport(vpi(1))
    upViewport()
    pushViewport(vpi(3))
    upViewport()
    pushViewport(vpi(5))
    upViewport()
    pushViewport(vpi(7))
    upViewport()
    pushViewport(vpi(9))
    upViewport(0)
    for (i in c(1, 3, 5, 7, 9)) {
        grid.roundrect(height=.5,
                       vp=paste("vplay::vp", i, sep=""),
                       name=paste("rr", i, sep=""),
                       gp=gpar(fill="white"))
    }
    grid.text("data", vp="vplay::vp1")
    grid.text("scale", vp="vplay::vp3")
    grid.text("stat", vp="vplay::vp5")
    grid.text("aesthetic", vp="vplay::vp7")
    grid.text("geom", vp="vplay::vp9")
    arr <- arrow(length=unit(3, "mm"), type="closed")
    grid.segments(grobX("rr1", 0), .5,
                  grobX("rr3", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr3", 0), .5,
                  grobX("rr5", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr5", 0), .5,
                  grobX("rr7", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr7", 0), .5,
                  grobX("rr9", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
}

figure5.8 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_bar(aes(x=trans))
    )

    update_geom_defaults("smooth", aes(color="black"))
    print(
        p + geom_smooth(aes(x=disp, y=mpg))
    )
}

figure5.9 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_point(aes(x=disp, y=mpg, shape=trans)) +
            scale_shape_manual(values=c(1, 3))
    
    )

    print(
        ggplot(mtcars2, aes(x=disp, y=mpg)) + 
            geom_point() +
            stat_smooth(aes(group=trans),
                        method="lm")
    )
}

figure5.10 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_bar(aes(x=trans, fill=factor(cyl)),
                     color="black") +
            scale_fill_manual(values=gray(1:3/3))
    
    )

    print(
        p + geom_bar(aes(x=trans, fill=factor(cyl)),
                     color="black",
                     position="dodge") +
            scale_fill_manual(values=gray(1:3/3))
    
    )

    print(
        p + geom_bar(aes(x=trans, fill=factor(cyl)),
                     color="black",
                     position="fill") +
            scale_fill_manual(values=gray(1:3/3))
    )
}

figure5.11 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_point(aes(x=disp, y=mpg)) + 
            scale_x_continuous(trans="log") +
            scale_y_continuous(trans="log") +
            geom_line(aes(x=disp, y=mpg), stat="smooth", 
                      method="lm")
    )

    print(
        p + geom_point(aes(x=disp, y=mpg)) + 
            scale_x_continuous(trans="log") +
            scale_y_continuous(trans="log") +
            geom_line(aes(x=disp, y=mpg), stat="smooth", 
                      method="lm") +
            coord_trans(xtrans="exp", ytrans="exp")
    )

    print(
        p + geom_bar(aes(x="", fill=trans)) +
            scale_fill_manual(values=gray(1:2/3))
    )

    print(
        p + geom_bar(aes(x="", fill=trans)) +
            scale_fill_manual(values=gray(1:2/3)) +
            coord_polar(theta="y") 
    )
}

figure5.12 <- function() {
    # grid.newpage()
    layvp <- viewport(layout=grid.layout(1, 11,
                                         heights=unit(1, "inch"),
                                         widths=unit(c(1, .5), "inch")),
                      name="vplay")
    vpi <- function(i) {
        viewport(layout.pos.col=i, name=paste("vp", i, sep=""))
    }
    pushViewport(layvp)
    pushViewport(viewport(layout.pos.col=1:11))
    grid.rect(width=1.1, gp=gpar(col=NA, fill="gray"))
    popViewport()
    pushViewport(vpi(1))
    upViewport()
    pushViewport(vpi(3))
    upViewport()
    pushViewport(vpi(5))
    upViewport()
    pushViewport(vpi(7))
    upViewport()
    pushViewport(vpi(9))
    upViewport()
    pushViewport(vpi(11))
    upViewport(0)
    for (i in c(1, 3, 5, 7, 9, 11)) {
        grid.roundrect(height=.5,
                       vp=paste("vplay::vp", i, sep=""),
                       name=paste("rr", i, sep=""),
                       gp=gpar(fill="white"))
    }
    grid.text("data", vp="vplay::vp1")
    grid.text("scale", vp="vplay::vp3")
    grid.text("stat", vp="vplay::vp5")
    grid.text("aesthetic", vp="vplay::vp7")
    grid.text("geom", vp="vplay::vp9")
    grid.text("coord", vp="vplay::vp11")
    arr <- arrow(length=unit(3, "mm"), type="closed")
    grid.segments(grobX("rr1", 0), .5,
                  grobX("rr3", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr3", 0), .5,
                  grobX("rr5", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr5", 0), .5,
                  grobX("rr7", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr7", 0), .5,
                  grobX("rr9", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
    grid.segments(grobX("rr9", 0), .5,
                  grobX("rr11", 180), .5,
                  arrow=arr, gp=gpar(fill="black"))
}

figure5.13 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)

    print(
        p + geom_point(aes(x=disp, y=mpg)) +
            facet_wrap(~ gear, nrow=1)
    
    )
}

figure5.14 <- function() {
    # mtcars2 <- mtcars
    # mtcars2$trans <- factor(mtcars$am, 
    #                         levels=0:1, 
    #                         labels=c("automatic", "manual"))
    # mtcars2$gear <- as.factor(mtcars$gear)
    # mtcars2$am <- NULL
    # mtcars2$vs <- NULL
    # mtcars2$drat <- NULL
    # mtcars2$carb <- NULL
    # mtcars2$wt <- NULL
    # mtcars2$hp <- NULL
    # mtcars2$qsec <- NULL
    # 
    # # To keep R CMD check happy
    # mpg <- mtcars2$mpg
    # 
    # 
    # 
    # p <- ggplot(mtcars2)
    # 
    # 
    # 
    # print(
    # p + geom_point(aes(x=disp, y=mpg)) +
    #     theme_bw()
    # 
    # )
    # 
    # 
    # print(
    # p + geom_point(aes(x=disp, y=mpg)) +
    #     opts(axis.title.y=theme_text(angle=0))
    # 
    # )
    # 
    # 
    # print(
    # p + geom_point(aes(x=disp, y=mpg)) +
    #     opts(axis.title.y=theme_blank())
    # 
    # )
    # 
    # 
    # print(
    # p + geom_point(aes(x=disp, y=mpg)) +
    #     opts(title="Vehicle Fuel Efficiency")
    # 
    # )
    # 
    # 
}

figure5.15 <- function() {
    mtcars2 <- mtcars
    mtcars2$trans <- factor(mtcars$am, 
                            levels=0:1, 
                            labels=c("automatic", "manual"))
    mtcars2$gear <- as.factor(mtcars$gear)
    mtcars2$am <- NULL
    mtcars2$vs <- NULL
    mtcars2$drat <- NULL
    mtcars2$carb <- NULL
    mtcars2$wt <- NULL
    mtcars2$hp <- NULL
    mtcars2$qsec <- NULL

    # To keep R CMD check happy
    mpg <- mtcars2$mpg

    p <- ggplot(mtcars2)
    print(
        p + geom_point(aes(x=disp, y=mpg)) +
            geom_hline(yintercept=29)
    )

    gcLimits <- 
        data.frame(category=c("2WD car",
                              "4WD car",
                              "2WD small pick-up truck",
                              "4WD small pick-up truck",
                              "2WD std pick-up truck",
                              "4WD std pick-up truck"),
                   limit=c(29, 24, 20, 18, 17, 16))

    print(
        p + geom_point(aes(x=disp, y=mpg)) +
            geom_hline(data=gcLimits, 
                       aes(yintercept=limit),
                       linetype="dotted") +
            geom_text(data=gcLimits,
                      aes(y=limit + .1, label=category),
                      x=70, hjust=0, vjust=0, size=3)
    
    )
}
