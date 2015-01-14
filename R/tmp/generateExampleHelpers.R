library(BBmisc)
library(stats)




# following the paper comparing nystrom to random fourier features
# we generate 102 dimensional vectors, where the whole information is
# in the first two components, the rest is uniformly sampled.
generateSyntheticFS <- function (filePath = "./zsvLowBound.data", n = 50, seed = 42, eps = 0.01)
{
    set.seed(seed)
   
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting syntheticFS example with %d examples to %s", n, filePath)
    

    n = 1000
    xf = vector()
    yf = vector()
    lf = vector()

    # add positive examples
        xc = rnorm(n, mean = 1, sd = 0.2)
        yc = rnorm(n, mean = 1, sd = 2.0)
        lc = rep(1, n) 
        
    # add the point
        xf = append (xf, xc)
        yf = append (yf, yc)
        lf = append (lf, lc)

    # add positive examples
        xc = rnorm(n, mean = -1, sd = 0.2)
        yc = rnorm(n, mean = -1, sd = 2.0)
        lc = rep(-1, n) 
        
    # add the point
        xf = append (xf, xc)
        yf = append (yf, yc)
        lf = append (lf, lc)

    plot (xf, yf, col = lf+2)
    
    # just dump it without much thought
    for (i in 1:length(xf)) {
        # create dummy features
        cx = rndom
        
        writeLines( paste( lf[i], " 1:", toString(xf[i]), " 2:", toString(yf[i]), sep = ""), con)
    }
    
    # close connection
    close(con)
    
    return (filePath)
}



# generate 4 clusters representing XOR
# in each quadrand around (p,p) (-p,-p), (p,-p), (-p, p), p = 1 for now
# add big gaussian noise to fluff it up.
# n will be multiplied with 4!
generateZSVLowBound <- function (filePath = "./zsvLowBound.data", n = 1, seed = 42, eps = 0.01)
{
    set.seed(seed)
   
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting zsvLowBound example with %d examples to %s", n, filePath)
    
    n = ceil(1/eps)
    
    xf = vector()
    yf = vector()
    lf = vector()

    for (i in 1:n) {

        xf = rep(0,n+1)
#        xf[1] = sqrt(n)
  #      xf[i+1] = n
        yf = ((-1) * (i%%2))*2 + 1
        xf = yf * xf
        
        xf = append (xf, xc)
        yf = append (yf, yc)
        lf = append (lf, lc)
    }

#    plot (xf, yf, col = lf+2)
    
    # just dump it without much thought
    for (i in 1:length(xf)) {
        writeLines( paste( lf[i], " 1:", toString(xf[i]), " 2:", toString(yf[i]), sep = ""), con)
    }
    
    # close connection
    close(con)
    
    return (filePath)
}



# generate 4 clusters representing XOR
# in each quadrand around (p,p) (-p,-p), (p,-p), (-p, p), p = 1 for now
# add big gaussian noise to fluff it up.
# n will be multiplied with 4!
generateXORClusters <- function (filePath = "./xorcluster.data", n = 1, seed = 42, sd = 0.5)
{
    set.seed(seed)
    
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting linear 5 example with %d examples to %s", n, filePath)
    
    xf = vector()
    yf = vector()
    lf = vector()

    for (i in 0:3) {
        mx = 2*(i%%2) - 1
        my = 2*(i%%4>1) - 1
        lc = mx*my
        messagef("%d -- m: (%d, %d), l: %d", i,mx ,my, lc)
        
        xc = rnorm(n, mean = mx, sd = sd)
        yc = rnorm(n, mean = my, sd = sd)
        lc = rep(lc, n) 
        
        xf = append (xf, xc)
        yf = append (yf, yc)
        lf = append (lf, lc)
    }

#    plot (xf, yf, col = lf+2)
    
    # just dump it without much thought
    for (i in 1:length(xf)) {
        writeLines( paste( lf[i], " 1:", toString(xf[i]), " 2:", toString(yf[i]), sep = ""), con)
    }
    
    # close connection
    close(con)
    
    return (filePath)
}




# generate an example similar to the ones in manuscript 2621, 
# keerthi, lin: asymptotic behaviors of support vector machines with gaussian kernel
# we opt for a symmetrical setting though
# n denotes the number of layers or depth.

generate2621 <- function (filePath = "./2621.data", n = 3, seed = 42, asymmetric = TRUE)
{
    set.seed(seed)
    
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting 2621 example with %d examples to %s", n, filePath)


    xf = vector()
    yf = vector()
    lf = vector()

    # add lattice
    for (x in 0:n+1) {
        for (y in 0:n+1) {
            if ((asymmetric == FALSE) & (x == y))
            next
            xc = x/(n+2)
            yc = y/(n+2)
            if (x > y)
            lc = 1
            else
            lc = -1
            
        #   messagef("m: (%f, %f), l: %d", x, y, lc)
        #    messagef("m: (%f, %f), l: %d", xc, yc, lc)
            
            # add the point
            xf = append (xf, xc)
            yf = append (yf, yc)
            lf = append (lf, lc)
        }
    }

    # add the two noise dots
    xf = append (xf, 1.5/(n+2))
    yf = append (yf, (2*n+1)/(2*(n+2)))
    lf = append (lf, 1)
    xf = append (xf, (2*n+1)/(2*(n+2)))
    yf = append (yf, 1.5/(n+2))
    lf = append (lf, -1)

    # plot (xf, yf, col = lf+2)
    
    # just dump it without much thought
    for (i in 1:length(xf)) {
        writeLines( paste( lf[i], " 1:", toString(xf[i]), " 2:", toString(yf[i]), sep = ""), con)
    }
    
    # close connection
    close(con)
    
    return (filePath)
}



# these examples are from
# bi, zhang: support vector classification with input data uncertainty
#
generateRadial5 <- function (filePath = "./radial5.data", n = 1, seed = 42)
{
    set.seed(seed)
    
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting radial 5 example with %d examples to %s", n, filePath)
    
    # generate uniformly from -5..5
    xc = runif(n, min=-5, max=5)
    yc = runif(n, min=-5, max=5)
  
    # create their labels, depending on the location
    labels = sign(xc*xc + yc*yc - 9)
  
#    plot(xc,yc,col=2+labels)

    # add gaussian noise with mean 0 and covariance [0.1,0.8]
    gn = rnorm(n, mean = 0.0, sd = runif(n, min=0.1, max=0.8))
    xc = xc + gn
    gn = rnorm(n, mean = 0.0, sd = runif(n, min=0.1, max=0.8))
    yc = yc + gn

  #  plot(xc,yc,col=2+sign(xc-yc))

    # compute distance to target boundary x1 - x2 = 0
    distances = abs(sqrt( xc*xc + yc*yc) - 3 )
  
  #  plot(xc,yc,col=1+distances)

    # sort points by distance
    order = sort(distances, method="shell", index.return = TRUE)$ix
    xo = xc[order]
    yo = yc[order]
    labelso = labels[order]

    # choose 0.1*n from 0.2*n first examples
    n02 = floor(n*0.2)
    n01 = floor(n*0.1)
    p02 = sample (n02)[1:n01]
    xoutliers = xo[p02]
    youtliers = yo[p02]
  
#    plot(xoutliers, youtliers, col=3)

  
    # add more gaussian noise to outlier
    outliergn = rnorm(n01, mean = 0.0, sd = runif(n01, min=0.5, max=2))
    xoutliers = xoutliers + outliergn
    outliergn = rnorm(n01, mean = 0.0, sd = runif(n01, min=0.5, max=2))
    youtliers = youtliers + outliergn
    
#   plot(xoutliers, youtliers, col=3)
   
    # finally replace the outliers
    xo[p02] = xoutliers
    yo[p02] = youtliers
   # plot(xo, yo, col = 2 + labelso )
    
    
    # just dump it without much thought
    for (i in seq (1,n)) {
        writeLines( paste( labelso[i], " 1:", toString(xo[i]), " 2:", toString(yo[i]), sep = ""), con)
    }
    
    # close connection
    close(con)
    
    return (filePath)
}



# generate +/-1 examples on (+/- n)
generateLinear5 <- function (filePath = "./linear5.data", n = 1, seed = 42)
{
    set.seed(seed)
    
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting linear 5 example with %d examples to %s", n, filePath)
    
    # generate uniformly from -5..5
    xc = runif(n, min=-5, max=5)
    yc = runif(n, min=-5, max=5)
  
    # create their labels, depending on the location
    labels = sign(xc - yc)
  
#    plot(xc,yc,col=2+sign(xc-yc))

    # add gaussian noise with mean 0 and covariance [0.1,0.8]
    gn = rnorm(n, mean = 0.0, sd = runif(n, min=0.1, max=0.8))
    xc = xc + gn
    gn = rnorm(n, mean = 0.0, sd = runif(n, min=0.1, max=0.8))
    yc = yc + gn

  #  plot(xc,yc,col=2+sign(xc-yc))

    # compute distance to target boundary x1 - x2 = 0
    theta = -45/180*pi
    M = t(matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2))
    rotatedPts = M%*%(rbind(xc,yc))
    distances = abs(rotatedPts[2,])
  
#    plot(xc,yc,col=1+distances)

    # sort points by distance
    order = sort(distances, method="shell", index.return = TRUE)$ix
    xo = xc[order]
    yo = yc[order]
    labelso = labels[order]

    # choose 0.1*n from 0.2*n first examples
    n02 = floor(n*0.2)
    n01 = floor(n*0.1)
    p02 = sample (n02)[1:n01]
    xoutliers = xo[p02]
    youtliers = yo[p02]
  
#    plot(xoutliers, youtliers, col=3)

  
    # add more gaussian noise to outlier
    outliergn = rnorm(n01, mean = 0.0, sd = runif(n01, min=0.5, max=2))
    xoutliers = xoutliers + outliergn
    outliergn = rnorm(n01, mean = 0.0, sd = runif(n01, min=0.5, max=2))
    youtliers = youtliers + outliergn
    
#    plot(xoutliers, youtliers, col=3)
    
    # finally replace the outliers
    xo[p02] = xoutliers
    yo[p02] = youtliers
  #  plot(xo, yo, col = 2 + labelso )
    
    
    # just dump it without much thought
    for (i in seq (1,n)) {
        writeLines( paste( labelso[i], " 1:", toString(xo[i]), " 2:", toString(yo[i]), sep = ""), con)
    }
    
    # close connection
    close(con)
    
    return (filePath)
}




# generate +/-1 examples on (+/- n)
generateXORExample2D <- function (filePath = "./xorexample2D.data", n = 1)
{
  # open connection  
  filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
  con  <- file(filePath, open = "w")
  messagef ("\t\tWriting XOR example with %d examples to %s", n, filePath)
  
  # just dump it without much thought
  for (i in seq (1,n)) {
    writeLines( paste( "-1", " 1:", toString(i*1), " 2:", toString(i*1), sep = ""), con)
    writeLines( paste( "1", " 1:", toString(-i*1), " 2:", toString(i*1), sep = ""), con)
    writeLines( paste( "1", " 1:", toString(i*1), " 2:", toString(-i*1), sep = ""), con)
    writeLines( paste( "-1", " 1:", toString(-i*1), " 2:", toString(-i*1), sep = ""), con)
  }
 
  # close connection
  close(con)
  
  return (filePath)
}




# generate +/-1 examples on (+/- n)
generateSimpleExample1DChain <- function (filePath = "./easyexample1DChain.data", n = 1)
{
  # open connection  
  filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
  con  <- file(filePath, open = "w")
  messagef ("\t\tWriting easy example with %d examples to %s", n, filePath)
  
  # just dump it without much thought
  for (i in seq (1,n)) {
    writeLines( paste( "1 1:", toString(i), sep = ""), con)
    writeLines( paste( "-1 1:", toString(-i), sep = ""), con)
  }
 
  # close connection
  close(con)
  
  return (filePath)
}


# generate +/-1 examples on (0,+/-n)
generateSimpleExample2D <- function (filePath = "./easyexample2D.data", n = 1)
{
  # open connection  
  filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
  con  <- file(filePath, open = "w")
  messagef ("\t\tWriting easy example with %d examples to %s", n, filePath)
  
  # just dump it without much thought
  for (i in seq (0,n)) {
    writeLines( paste( "1 1:1 2:", toString(i), sep = ""), con)
    writeLines( paste( "-1 1:-1 2:", toString(i), sep = ""), con)
    
    if (i > 0) {
      writeLines( paste( "1 1:1 2:", toString(-i), sep = ""), con)
      writeLines( paste( "-1 1:-1 2:", toString(-i), sep = ""), con)
    }
  }
 
  # close connection
  close(con)
  
  return (filePath)
}



# generate +/-1 examples on (0,+/-1)
generateSimpleExample <- function (filePath = "./easyexample.data", n = 1, scale = FALSE)
{
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), "_", toString(n), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting easy example with %d examples to %s", n, filePath)

    correctionFactor = 1
    if (scale == TRUE)
        correctionFactor = 1/n
    
    xf = vector()
    lf = vector()

    for (i in 1:n) {
        xc = -i*correctionFactor
        lc = -1
  #      messagef("m: (%f), l: %d", xc, lc)
        
        xf = append (xf, xc)
        lf = append (lf, lc)
        
        xc = i*correctionFactor
        lc = 1
#        messagef("m: (%f), l: %d", xc, lc)
        
        xf = append (xf, xc)
        lf = append (lf, lc)
    }

   # plot (xf, rep(1,length(xf)), col = lf+2)
    
    # just dump it without much thought
    for (i in 1:length(xf)) {
        writeLines( paste( lf[i], " 1:", toString(xf[i]), sep = ""), con)
    }
  
 
  # close connection
  close(con)
  
  return (filePath)
}




# generate +/-1 examples on (0,+/-1)
generateSimpleThreePoint <- function (filePath = "./easyexample.data")
{
    # open connection  
    filePath = file.path(dirname(filePath), paste (basename(filePath), sep = ""))
    con  <- file(filePath, open = "w")
    messagef ("\t\tWriting easy example with 3 points to %s", filePath)
  
    # for now "hard-coded" example
    # just dump it without much thought
    writeLines("1 1:1", con)
    writeLines("-1 1:0", con)
    writeLines("1 1:-1", con)
 
    # close connection
    close(con)

    return (filePath)
}

