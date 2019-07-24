
figure15.1 <- function() {
    nodes <- c("grDevices", "graphics", "grid",
               "lattice", "ggplot2")
    edgeList <- 
        list(grDevices=list(edges=c("graphics", "grid")),
             graphics=list(),
             grid=list(edges=c("lattice", "ggplot2")),
             lattice=list(),
             ggplot2=list())
    simpleGNEL <- new("graphNEL",
                      nodes=nodes,
                      edgeL=edgeList,
                      edgemode="directed")

    # Weird stuff happening if don't pre-layout graph
    temp <- Rgraphviz::agopen(simpleGNEL, "")
    Rgraphviz::plot(temp)
}
# placeholder

figure15.3 <- function() {
    nodes <- c("grDevices", "graphics", "grid",
               "lattice", "ggplot2")
    edgeList <- 
        list(grDevices=list(edges=c("graphics", "grid")),
             graphics=list(),
             grid=list(edges=c("lattice", "ggplot2")),
             lattice=list(),
             ggplot2=list())
    simpleGNEL <- new("graphNEL",
                      nodes=nodes,
                      edgeL=edgeList,
                      edgemode="directed")

    # Weird stuff happening if don't pre-layout graph
    tempGraph <- Rgraphviz::agopen(simpleGNEL, "", layoutType="neato")
    Rgraphviz::plot(tempGraph)
}

figure15.4 <- function() {

    nodes <- c("grDevices", "graphics", "grid",
               "lattice", "ggplot2")
    edgeList <- 
        list(grDevices=list(edges=c("graphics", "grid")),
             graphics=list(),
             grid=list(edges=c("lattice", "ggplot2")),
             lattice=list(),
             ggplot2=list())
    simpleGNEL <- new("graphNEL",
                      nodes=nodes,
                      edgeL=edgeList,
                      edgemode="directed")

    Rgraphviz::plot(simpleGNEL, 
                    edgeAttrs=list(lty=c(`grDevices~graphics`="solid", 
                                         `grDevices~grid`="solid",
                                         `grid~lattice`="dashed", 
                                         `grid~ggplot2`="dashed")),
                    nodeAttrs=list(fillcolor=c(grDevices="white", 
                                               graphics="gray90", grid="gray90",
                                               lattice="gray60", ggplot2="gray60")))
}

figure15.5 <- function() {
    load(system.file("extra", "grd.rda", package="RGraphics"))
    grDraw <- function(layout) {
        ragrd <- Rgraphviz::agopen(grd, "", layoutType=layout)
        xy <- Rgraphviz::getNodeXY(ragrd)
        grid.newpage()
        pushViewport(viewport(width=1.1, height=1.1),
                     plotViewport(xscale=range(xy$x), yscale=range(xy$y)))
        grid.circle(xy$x, xy$y, default.units="native", 
                    r=unit(.25, "mm"), gp=gpar(fill="black"))
        grdNodes <- graph::nodes(grd)
        grdEdges <- graph::edges(grd)
        mapply(function(start, ends) {
            if (length(ends) > 0) {
                grid.segments(xy$x[grdNodes == start],
                              xy$y[grdNodes == start],
                              xy$x[grdNodes %in% ends],
                              xy$y[grdNodes %in% ends],
                              default.units="native",
                              gp=gpar(col=rgb(0,0,0,.5)))
            }
        },
        as.list(grdNodes),
        grdEdges)
        for (i in c("grDevices", "graphics", "grid", "lattice", "ggplot2")) {
            grid.rect(xy$x[grdNodes == i], xy$y[grdNodes == i],
                      width=stringWidth(i), height=unit(1, "lines"),
                      default.units="native",
                      gp=gpar(col=NA, fill=rgb(.5, .5, .5, .5)))
            grid.text(i, xy$x[grdNodes == i], xy$y[grdNodes == i],
                      default.units="native",
                      gp=gpar(col="white"))
        }
    }

    png("Figures/graph-pkgdep.png", width=1350, height=1350, res=300)
    grDraw("neato")
    dev.off()
    system("cp Figures/graph-pkgdep.png Web/")
}

figure15.6 <- function() {
    nodes <- c("grDevices", "graphics", "grid",
               "lattice", "ggplot2")
    edgeList <- 
        list(grDevices=list(edges=c("graphics", "grid")),
             graphics=list(),
             grid=list(edges=c("lattice", "ggplot2")),
             lattice=list(),
             ggplot2=list())
    simpleGNEL <- new("graphNEL",
                      nodes=nodes,
                      edgeL=edgeList,
                      edgemode="directed")

    Rgraphviz::toFile(Rgraphviz::agopen(simpleGNEL, ""), 
                      filename="Figures/graph-graphvizrender.ps", 
                      fileType="ps")
}

figure15.7 <- function() {
    dh <- hypergraph::DirectedHyperedge(c("A", "B"), c("C", "D"))
    hg <- hypergraph::Hypergraph(LETTERS[1:4], list(dh))
    getMethod("plot", "graphBPH")(hyperdraw::graphBPH(hg))
}

figure15.8 <- function() {
    treeIgraph <- igraph::graph.tree(10)
    fullIgraph <- igraph::graph.full(10)

    # See ?igraph.plotting for useful graph attributes
    treeIgraph <- igraph::set.vertex.attribute(treeIgraph, "color", value="black")
    treeIgraph <- igraph::set.edge.attribute(treeIgraph, "color", value="black")
    plot(treeIgraph, 
         layout=igraph::layout.reingold.tilford(treeIgraph, root=1, flip.y=FALSE))

    fullIgraph <- igraph::set.vertex.attribute(fullIgraph, "color", value="black")
    fullIgraph <- igraph::set.edge.attribute(fullIgraph, "color", value="black")
    plot(fullIgraph, layout=igraph::layout.circle)
}

figure15.9 <- function() {
    nodes <- c("grDevices", "graphics", "grid",
               "lattice", "ggplot2")
    edgeList <- 
        list(grDevices=list(edges=c("graphics", "grid")),
             graphics=list(),
             grid=list(edges=c("lattice", "ggplot2")),
             lattice=list(),
             ggplot2=list())
    simpleGNEL <- new("graphNEL",
                      nodes=nodes,
                      edgeL=edgeList,
                      edgemode="directed")

    simpleNetwork <- 
        network::network(rbind(c(1, 2),
                               c(1, 3),
                               c(3, 4),
                               c(3, 5)),
                         vertex.attr=list(vertex.names=nodes))

    par(mar=rep(2, 4), xpd=NA)
    set.seed(2500)
    plot(simpleNetwork, mode="fruchtermanreingold", 
         vertex.col=1, displaylabels=TRUE)
}

figure15.10 <- function() {
    par(mar=rep(1, 4))
    plot.new()

    nodePos <- diagram::coordinates(c(2, 2, 2, 2))

    diagram::straightarrow(nodePos[1, ], nodePos[3,])

    diagram::straightarrow(nodePos[3, ], nodePos[4,])
    diagram::straightarrow(nodePos[3, ], nodePos[5,])
    diagram::straightarrow(nodePos[5, ], nodePos[6,])
    diagram::straightarrow(nodePos[6, ], nodePos[4,])
    diagram::straightarrow(nodePos[5, ], nodePos[7,])
    diagram::straightarrow(nodePos[6, ], nodePos[8,])
    diagram::straightarrow(nodePos[7, ], nodePos[8,])

    diagram::textplain(nodePos[3, ] + c(.2, .02), lab="yes")
    diagram::textplain(nodePos[5, ] + c(.2, .02), lab="yes")
    diagram::textplain(nodePos[6, ] + c(.03, .15), lab="yes")
    diagram::textplain(nodePos[3, ] + c(.2, .02), lab="yes")
    diagram::textplain(nodePos[5, ] + c(-.03, .125), lab="no")
    diagram::textplain(nodePos[7, ] + c(-.03, .125), lab="no")
    diagram::textplain(nodePos[7, ] + c(.2, -.02), lab="no")
    diagram::textplain(nodePos[8, ] + c(-.03, .125), lab="no")

    diagram::textrect(nodePos[1, ], .05, .025, lab="start")

    diagram::textdiamond(nodePos[3, ], .15, .1)
    diagram::textplain(nodePos[3, ], .08,
                       lab=c("do you", "understand flow", "charts?"))
    diagram::textellipse(nodePos[4, ], .08, .08,
                         lab=c("let's go", "drink."))
    diagram::textdiamond(nodePos[5, ], .15, .1)
    diagram::textplain(nodePos[5, ], .08,
                       lab=c("you see", "the lines labeled", "'yes'?"))
    diagram::textdiamond(nodePos[6, ], .15, .1)
    diagram::textplain(nodePos[6, ], .08,
                       lab=c("you see", "the lines labeled", "'no'?"))
    diagram::textdiamond(nodePos[7, ], .15, .1)
    diagram::textplain(nodePos[7, ], .08,
                       lab=c("you see", "the lines labeled", "'no'?"))
    diagram::textellipse(nodePos[8, ], .07, .07,
                         lab=c("I hate", "you."))

}
