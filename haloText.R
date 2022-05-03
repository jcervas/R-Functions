haloText <-  function(x, y=NULL, labels, col="black", hc="white", hw=0.1, ...) {
	
	xy <- coord(x)
	x <- xy[,1]
	y <- xy[,2]
	xo <- hw * strwidth("A")
	yo <- hw * strheight("A")	

	axo <- .5 * hw * strwidth("A")
	ayo <- .5 * hw * strheight("A")

	theta <- seq(pi/4, 2*pi, length.out=8*hw*10)
	alpha <- seq(pi/4, 2*pi, length.out=8*hw*10)

	for (i in theta) {
		text( x + cos(i)*xo, y + sin(i)*yo, labels, col=hc, ... )
	}	

	for (i in alpha) {
		text( x + cos(i)*(axo/2), y + sin(i)*(ayo/1), labels, col='#FFFFFF', ... )
	}

	text(x, y, labels, col=col, ...)
}
