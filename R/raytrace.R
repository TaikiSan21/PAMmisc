#' @title Raytrace Through a Soundspeed Profile
#'
#' @description Traces the ray of a sound through a varying soundspeed profile
#'   for a fixed amount of time. Also plots the provided sound speed profile and
#'   all traces generated. All code here is based on MATLAB code originally
#'   written by Val Schmidt from the University of New Hampshire
#'   Val Schmidt (2021). raytrace
#'   https://www.mathworks.com/matlabcentral/fileexchange/26253-raytrace), MATLAB Central File Exchange. Retrieved June 29, 2021.
#'
#' @param x0 starting horizontal coordinate in meters
#' @param z0 starting vertical coordinate in meters
#' @param theta0 starting angle(s) of ray in degrees
#' @param tt max travel time of ray in seconds
#' @param zz vertical coordinates of sound speed profile (positive values are down)
#' @param cc sound speed measurements at \code{zz} locations, meters / second
#' @param plot logical flag to plot. Can be a vector of length two to individually
#'    select plotting one of the two plots generated
#' @param progress logical flag to show progress bar
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @return A list with four elements: \code{x}, the horizontal
#'  coordinates of ray path, \code{z} the vertical coordinates of ray path,
#'  \code{t} actual travel time of ray in seconds,
#'  and \code{d} the total distance the ray traveled. Each individual item
#'  in the output is a list with one entry for each \code{theta0} provided.
#'
#' @examples
#'
#' #  Setup the sound speed profile
#' zz <- seq(from=0, to=5000, by=1)
#' cc <- 1520 + zz * -.05
#' cc[751:length(cc)] <- cc[750] + (zz[751:length(zz)] - zz[750])*.014
#' rt <- raytrace(0, 0, 5, 120, zz, cc, TRUE)
#'
#' @importFrom graphics title lines legend
#' @export
#'
raytrace <- function(x0, z0, theta0, tt, zz, cc, plot=TRUE, progress=FALSE) {
    # extend sound speed profile to surface
    if(zz[1] != 0) {
        zz <- c(0, zz)
        cc <- c(cc[1], cc)
    }

    # initialize variables
    MAXBOUNCE <- 20
    nTheta <- length(theta0)
    xxf <- vector('list', length=nTheta)
    zzf <- xxf
    ddf <- xxf
    ttf <- xxf
    tttf <- xxf
    thetaf <- xxf
    for(m in seq_along(theta0)) {
        Nsvp <- length(zz)
        zzend <- zz[Nsvp]

        dz <- diff(zz)
        dc <- diff(cc)

        if(theta0[m] < 0 && z0 > 0) {
            z <- rev(zz)
            zzz <- c(zz[1], zz[1] + cumsum(rev(dz)))
            ccc <- c(cc[Nsvp], cc[Nsvp] - cumsum(rev(dc)))
            mult <- 1
        } else {
            z <- zz
            zzz <- c(zz[1], zz[1] + cumsum(dz))
            ccc <- c(cc[1], cc[1] + cumsum(dc))
            mult <- -1
        }
        dz0 <- dz
        while(abs(zzz[length(zzz)]) < MAXBOUNCE * zzend) {
            # browser()
            if(mult == -1) {
                if(theta0[m] < 0) {
                    zzz <- c(zzz, zzz[length(zzz)] + cumsum(rev(dz)))
                } else {
                    zzz <- c(zzz, zzz[length(zzz)] + cumsum(rev(dz)))
                }
                ccc <- c(ccc, ccc[length(ccc)] + mult * cumsum(rev(dc)))
            } else {
                zzz <- c(zzz, zzz[length(zzz)] + cumsum(dz))
                ccc <- c(ccc, ccc[length(ccc)] + mult * cumsum(dc))
            }
            # browser()
            z <- c(z, z[length(z)] + mult * cumsum(rev(dz0)))
            dz <- rev(dz)
            dz0 <- rev(dz0)
            mult <- mult * -1
            # cat(abs(zzz[Nsvp]) ,'\n')
        }
        # browser()
        # start with the standard defintions
        dz <- diff(zzz)                          # depth steps
        g <- diff(ccc) / dz                      # sound speed gradient

        # starting index (set to depth closest to z0
        # browser()
        idx <- which.min(abs(z[1:Nsvp]-z0))
        q <- idx[1]
        qstart <- q

        # init vars
        theta <- rep(NA, length(dz))  # ray angle
        theta[q] <- abs(theta0[m]) * pi / 180
        x <- rep(NA, Nsvp)    # x distance of ray path
        dx <- rep(NA, Nsvp)
        x[q] <- x0

        t <- 0   # cumulative travel time
        d <- 0   # cumulative travel distance
        ttt <- t
        if(progress) {
            pb <- txtProgressBar(min = 0, max=tt, style=3)
        }
        while(t < tt) {

            if(q > length(g)) {
                warning('CCOM:outofBounds Not enough bounces specified for',
                        'time requested. Increase MAXBOUNCES')
                break
            }

            # handle constant 0 gradient (no refraction)
            if(g[q] == 0) {
                theta[q+1] <- theta[q]
                if(theta[q] == 0) {
                    dx[q] <- abs(dz[q])
                } else {
                    dx[q] <- abs(dz[q] / tan(theta[q]))
                }

                dd <- sqrt(dx[q]^2 + dz[q]^2)
                dt <- dd / ccc[q]
            } else {
                # calculate radius of curvature
                Rc <- -1/g[q] * ccc[q] / cos(theta[q])
                # calculate angle Leaving depth step form angle enteringdepth
                # step and radius of curvature of ray-path
                tmpCos <- cos(theta[q]) - dz[q]/Rc
                # theta[q+1] <- suppressWarnings(acos(cos(theta[q]) - dz[q]/Rc))
                if(abs(tmpCos) > 1) {
                    #     browser()
                    # }
                    # # when we go through a caustic we get a complex angle, so we catch
                    # #and reflect the ray
                    # if(Im(theta[q+1]) != 0) {
                    theta[q+1] <- -theta[q]
                    #     % We need to rejig zzz,ccc, dz, z and g
                    #     % When you are going down and there's a caustic you start
                    # % to go back up. We need to find the index of the extended
                    # % synthetic sound speed profile that matches the sound
                    # % speed at the caustic so we can omit the portion of the
                    # % sound speed profile where we are to the bottom and back
                    # % up to our caustic locaiton. That's what this next find
                    #     % statement does.
                    #     %tmp = find(ccc((q+1):end) == ccc(q));
                    tmp <- tryCatch({
                        which.min(abs(z[(q+1):(q+2*length(cc))] - z[q]))
                    },
                    error = function(e) {
                        numeric(0)
                    })
                    if(length(tmp) == 0) {
                        warning('CCOM:outofBounds Not enough bounces specified for ',
                                'time requested. Increase MAXBOUNCES')
                        break
                    }

                    ccc <- c(ccc[1:q], ccc[(q + tmp[1]):length(ccc)])
                    zshift <- zzz[q + tmp[1] + 1] - zzz[q]
                    zzz <- c(zzz[1:q], zzz[(q+tmp[1]):length(zzz)] - zshift)
                    z <- c(z[1:q], z[(q+ tmp[1]):length(z)])
                    dz <- diff(zzz)
                    g <- diff(ccc)/dz
                    dx[q] <- 0
                    dt <- 0
                    dd <- 0
                } else {
                    theta[q+1] <- acos(tmpCos)
                    if(theta[q] > 0) {
                        dx[q] <- Rc * (sin(theta[q+1]) - sin(theta[q]))
                        dt <- -2/g[q] * (atanh(tan(theta[q+1]/2)) -
                                             atanh(tan(theta[q]/2)))
                    } else {
                        dx[q] <- abs(Rc * -1*(sin(theta[q+1]) - sin(theta[q])))
                        dt <- -2/g[q] * (atanh(tan(theta[q+1]/2)) -
                                             atanh(tan(theta[q]/2)))
                    }

                    dd <- dt * (ccc[q] + g[q]/2)
                }
            }

            t <- t + dt
            ttt[q+1] <- t
            x[q+1] <- x[q] + dx[q]
            d <- d + dd
            q <- q+1
            if(progress) {
                setTxtProgressBar(pb, value=t)
            }
        }

        x <- x[!is.na(x)]
        z <- z[qstart:(qstart + length(x) -1)]

        corrector <- (tt - (t-dt))/dt
        x[length(x)] <- x[length(x)-1] + (x[length(x)] - x[length(x)-1])*corrector
        z[length(z)] <- z[length(z)-1] + (z[length(z)] - z[length(z)-1])*corrector
        t <- (t-dt) + dt*corrector
        theta <- theta[!is.na(theta)]
        xxf[[m]] <- x
        zzf[[m]] <- z
        ddf[[m]] <- d
        ttf[[m]] <- t
        thetaf[[m]] <- theta * 180 / pi
        tttf[[m]] <- ttt
    }
    if(length(plot) == 1) {
        plot <- rep(plot, 2)
    }
    if(plot[1]) {
        plot(cc[1:Nsvp], zz, ylab='Depth, m', xlab='Sound Speed, m/s', type='l')
        title('Sound Speed Profile')
    }
    if(plot[2]) {
        xRange <- range(sapply(xxf, range))
        zRange <- range(sapply(zzf, range))
        cols <- viridis_pal()(32)
        colScale <- 31*((theta0 - min(theta0)) / max(theta0 - min(theta0))) + 1
        if(length(theta0) == 1) {
            colScale <- 1
        }
        # browser()
        for(i in seq_along(xxf)) {
            if(i == 1) {
                plot(xxf[[i]], -zzf[[i]], type='l', xlim=xRange, ylim=rev(-zRange), col=cols[colScale[i]],
                     xlab='Range (m)', ylab='Depth (m)', main='Ray Paths')
            } else {
                lines(xxf[[i]], -zzf[[i]], col=cols[colScale[i]])
            }
        }
        # browser()
        if(length(theta0) > 1) {
            leg <- rep(NA, 11)
            leg[c(1, 6, 11)] <- c(min(theta0), mean(range(theta0)), max(theta0))
            legend(x='topright',
                   legend= c(NA,leg),
                   fill=c(NA,viridis_pal()(11)),
                   border=NA,
                   y.intersp = .5,
                   cex=1,
                   text.font=1,
                   title='Start Angle'
            )
        }
    }
    list(x=xxf, z=zzf, t=ttf, d=ddf, tt=tttf)
}
