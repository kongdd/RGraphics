
figure13.1 <- function() {
    xmm <- read.table(file.path("XMM-Newton", "XMM-Newton.txt"),
                      header=TRUE)

    counts <- sort(table(RGraphics::xmm$Category))

    par(mfrow=c(1, 2), mar=c(3, 3, 2, 2))

    barplot(counts)
    dotchart(counts)

    print(barchart(counts), pos=c(0, 0, .5, 1),
          more=TRUE)
    print(dotplot(counts), pos=c(.5, 0, 1, 1))

    grid.newpage()
    catSort <- data.frame(Category=factor(RGraphics::xmm$Category, levels=names(counts)))
    pushViewport(viewport(x=0, width=.5, just="left"))
    print(ggplot(catSort) +
              geom_bar(aes(x=Category)), newpage=FALSE)
    popViewport()
    pushViewport(viewport(x=1, width=.5, just="right"))
    catCounts <- data.frame(Category=factor(names(counts), levels=names(counts)),
                            Count=counts)
    print(ggplot(catCounts) +
              geom_point(aes(y=Category, x=Count)),
          newpage=FALSE)
    popViewport()
}

figure13.2 <- function() {
    vcd::spine(Priority ~ Duration, RGraphics::xmm)

    durn <- RGraphics::xmm$Duration/1000
    vcd::cd_plot(Priority ~ durn, RGraphics::xmm, xlab="Duration (1000s)")
}

figure13.3 <- function() {
    trellis.par.set(theme = canonical.theme("postscript", color=FALSE))
    catTab <- table(RGraphics::xmm$Schedule, RGraphics::xmm$Priority)
    print(barchart(prop.table(catTab, margin=1), col=gray(1:3/4)),
          pos=c(0, 0, .5, 1), more=TRUE)
    print(barchart(prop.table(catTab, margin=1), col=gray(1:3/4), stack=FALSE),
          pos=c(.5, 0, 1, 1))

    grid.newpage()
    pushViewport(viewport(x=0, width=.5, just="left"))
    print(
        ggplot(as.data.frame(prop.table(catTab, margin=1))) +
            geom_bar(aes(x=Var1, fill=Var2, y=Freq), 
                     stat="identity", col="black") +
            scale_fill_manual(values=gray(1:3/3)),
        newpage=FALSE)
    popViewport()
    pushViewport(viewport(x=1, width=.5, just="right"))
    print(
        ggplot(as.data.frame(prop.table(catTab, margin=1))) +
            geom_bar(aes(x=Var1, fill=Var2, y=Freq), 
                     stat="identity", col="black", position="dodge") +
            scale_fill_manual(values=gray(1:3/3)),
        newpage=FALSE)
    popViewport()
}

figure13.4 <- function() {
    vcd::mosaic(Priority ~ Schedule, RGraphics::xmm)
    vcd::mosaic(nObs ~ Schedule + Priority, RGraphics::xmm,
                labeling_args=list(rot_labels=c(right=0), 
                                   offset_labels=c(right=-.5),
                                   just_labels=c(right="left")),
                margin=c(right=4))
}

figure13.5 <- function() {
    grid.rect(gp=gpar(col=NA, fill="gray"))
    vcd::tile(nObs ~ Schedule + Priority, RGraphics::xmm,
              tile_type="area",
              shade=TRUE, 
              gp=gpar(lwd=2, fill="white"), 
              pos_labels=c(left="left", top="left", right="left"), 
              just_labels=c(left="left", top="left", right="left"),
              pop=FALSE, newpage=FALSE)

    downViewport("cell:Schedule=fixed,Priority=C,nObs=multiple")
    grid.circle(0, 0, r=unit(1, "mm"))
    upViewport(0)

    downViewport("cell:Schedule=fixed,Priority=C,nObs=single")
    grid.circle(0, 0, r=unit(1, "mm"))
    upViewport(0)
    downViewport("cell:Schedule=free,Priority=A,nObs=multiple")
    grid.circle(0, 0, r=unit(1, "mm"))
    upViewport(0)
    downViewport("cell:Schedule=free,Priority=B,nObs=multiple")
    grid.circle(0, 0, r=unit(1, "mm"))
    upViewport(0)

    vcd::doubledecker(nObs ~ Schedule + Priority, RGraphics::xmm,
                      dep_varname=FALSE,
                      gp=gpar(fill=c("gray90", "gray")),
                      offset_labels=c(right=-.5),
                      margins=c(bottom=3, left=1, top=1, right=5))
}

figure13.6 <- function() {
    xmm <- read.table(file.path("XMM-Newton", "XMM-Newton.txt"),
                      header=TRUE)

    pairs(vcd::structable(nObs ~ Priority + Schedule, RGraphics::xmm),
          space=.15)
}

figure13.7 <- function() {
    vcd::cotabplot(~ Schedule + Priority | nObs, RGraphics::xmm)
}

figure13.8 <- function() {
    # Need the .1 cos the handling of zero cells seems off
    # Also need the custom shading to produce gray-scale
    vcdExtra::mosaic3d(vcd::structable(~ Priority + Schedule + nObs, RGraphics::xmm) + .1, 
                       shading=function(x) { "gray" })
}
