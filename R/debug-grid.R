# debug with [grid::showViewport]
figure8.26 <- function() {
    pdf("Figures/interactgrid-latticevps-%d.pdf", onefile=FALSE)
    print( xyplot(pressure ~ temperature, pressure) )
    showViewport(newpage=TRUE, 
                 col="black",
                 fill=rgb(.5, .5, .5, .2))
    dev.off()
    png("Web/interactgrid-latticevps%d.png")

    print(xyplot(pressure ~ temperature, pressure))
    showViewport(newpage=TRUE, col="black",
                 fill=rgb(.5, .5, .5, .2))
    dev.off()
    system("rm Web/interactgrid-latticevps1.png")
}

figure8.27 <- function() {
    pdf("Figures/interactgrid-latticeleaves-%d.pdf", onefile=FALSE)
    print(
        xyplot(pressure ~ temperature, pressure)
    )
    showViewport(newpage=TRUE, leaves=TRUE,
                 col="black",
                 fill=rgb(.5, .5, .5, .2))
    dev.off()

    
    png("Web/interactgrid-latticeleaves%d.png")
    print(
        xyplot(pressure ~ temperature, pressure)
    )
    showViewport(newpage=TRUE, leaves=TRUE,
                 col="black",
                 fill=rgb(.5, .5, .5, .2))
    dev.off()
    system("rm Web/interactgrid-latticeleaves1.png")
}
