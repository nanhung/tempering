#setwd("C:/Users/lanhsieh/Documents/fitdist/linear2")

## function plotting the evolution of the temperature scale when adjusting it
plot.all.perks = function (all.perks, mylog="x") {
  par(mar=c(5, 5, 1, 1))
  N.iters = dim(all.perks$perks)[1]
  xlims  = c(1E-7,1)
  ylims  = range(all.perks$lnpi, na.rm=T)
  mycols = rainbow(N.iters)
  for (i in 1:N.iters) {
    j = which(!is.na(all.perks$perks[i,]))
    if (i == 1) {
      plot(all.perks$perks[i,j], all.perks$lnpi[i,j], type="b", las=1,
           log=mylog, xlab="Perk", ylab="", cex.lab=1.4,
           xlim=xlims, ylim=ylims, col=mycols[i], lwd=2)
      mtext("Log pseudo-prior", side=2, line=3.6, cex=1.4)      
    } else {
      plot(all.perks$perks[i,j], all.perks$lnpi[i,j], type="b", las=1,
           log=mylog,
           xlim=xlims, ylim=ylims, xaxt="n", yaxt="n", xlab="", ylab="",
           col=mycols[i], lwd=2)
    }
    par(new=T)
  }
  par(new=F)
}

## utility function to extract all information from a perk output
get.all.perks = function (filename) {
  ncols = max(count.fields(filename, sep = "\t"))
  tmp = read.delim(filename, col.names=1:ncols,
                   sep="\t", fill=T, header=F)
  index = which(tmp[,1] == "Perks:")
  perks.table = as.matrix(tmp[index,-1])
  index = which(tmp[,1] == "Counts:")
  counts.table = as.matrix(tmp[index,-1])
  index = which(tmp[,1] == "LnPi(i):")
  lnpi.table = as.matrix(tmp[index,-1])
  index = which(tmp[,1] == "Tried Jumps:")
  try.table = as.matrix(tmp[index,-1])
  index = which(tmp[,1] == "Accepted Jumps:")
  accept.table = as.matrix(tmp[index,-1])
  return(list("perks"  = perks.table,
              "counts" = counts.table,
              "lnpi"   = lnpi.table,
              "trials" = try.table,
              "accept" = accept.table))
}

## plot the evolution of the slopes of the temperature scale
plot.all.perks.slopes = function (all.perks, mylog="xy") {
  par(mar=c(5, 5, 1, 1))
  N.iters = dim(all.perks$perks)[1]
  xlims  = c(1E-7,1)
  ylims  = c(1,1E7)
  mycols = rainbow(N.iters)
  for (i in 1:N.iters) {
    j = which(!is.na(all.perks$perks[i,]))
    n.perks = length(j)
    slopes = rep(0, n.perks)
    k = 1
    slopes[k] = (all.perks$lnpi[i,k+1]  - all.perks$lnpi[i,k]) /
      (all.perks$perks[i,k+1] - all.perks$perks[i,k])
    if (n.perks > 2)
      for (k in 2:(n.perks-1)) {
        slopes[k] = ((all.perks$lnpi[i,k+1]  - all.perks$lnpi[i,k]) /
                       (all.perks$perks[i,k+1] - all.perks$perks[i,k]) +
                       (all.perks$lnpi[i,k]  - all.perks$lnpi[i,k-1]) /
                       (all.perks$perks[i,k] - all.perks$perks[i,k-1])) * 0.5
      }
    k = n.perks
    slopes[k] = (all.perks$lnpi[i,k]  - all.perks$lnpi[i,k-1]) /
      (all.perks$perks[i,k] - all.perks$perks[i,k-1])
    if (i == 1) {
      plot(all.perks$perks[i,j], slopes, type="b", las=1, log=mylog,
           xlab="Perk", ylab="", cex.lab=1.4,
           xlim=xlims, ylim=ylims, col=mycols[i])
      mtext("Log pseudo-prior slope", side=2, line=3.8, cex=1.4)
    }
    else
      plot(all.perks$perks[i,j], slopes, type="b", las=1, log=mylog,
           xlim=xlims, ylim=ylims, xaxt="n", yaxt="n", xlab="", ylab="",
           col=mycols[i])    
    par(new=T)
  }
  par(new=F)
} # end plot.all.perks.slopes()

## utility function to extract last perks from a perk output
get.final.perks = function (filename) {
  ncols = max(count.fields(filename, sep = "\t"))
  perks = read.delim(filename, col.names=1:ncols,
                     sep="\t", fill=T, header=F)
  ## dim(perks)
  ## head(perks, 10)
  ## lines are grouped by 5
  n.blocks = dim(perks)[1] / 5
  perks = perks[5 * (n.blocks - 1) + 1,-1]
  return(perks[which(perks <= 1)])
}

## function to plot the temperatures visits
plot.perks.visits = function(mcmc, n_temper) {
  par(mar=c(5, 5, 1, 1))
  plot(mcmc$iter, mcmc$IndexT, type='l', ylim=c(0,n_temper-1), las=1,
       xlab="Iteration", ylab="Perk index", cex.lab=1.5)
}

## function to plot Robbins-Monro pseudo-priors updating 
plot.Robbins_Monro = function (mcmc, n_temper, nrows=7, ncols=7) {
  ## array indices for the pseudos priors
  i_start_pseudos = pmatch("LnPseudoPrior.1.",names(mcmc))
  i_end_pseudos = i_start_pseudos + n_temper - 1
  irange.top = i_start_pseudos:i_end_pseudos
  par(mfrow=c(nrows,ncols), mar=c(3,3,1,1))
  for (i in (irange.top)) {
    plot(mcmc$iter, mcmc[,i], type='l', las=1) 
  }
}

## function to plot linear2 posterior in 2D
plot.lin2.2D = function(mcmc) {
  plot(mcmc$SD_z, mcmc$B.1., pch=".", las=1, xlab="SD_z", ylab="B")
}

## function to plot linear2 posterior in 3D
plot.lin2.3D = function(mcmc) {
  library(rgl)
  plot3d(x=mcmc$SD_y, y=mcmc$SD_z, z=mcmc$B.1., type="s", size=0.25,
         xlab="SD_y", ylab="SD_z", zlab="B", col="red")
}
## function to plot posterior histograms for a parameter at a time
plot.post.histograms = function (mcmc, n_temper, parm=1, a=6, b=5) {
  par(mfrow=c(a,b), mar=c(4,4,1.5,1))
  i = parm+1
  xlims = range(mcmc[,i])
  for (j in 0:(n_temper-1)) {
    index = which(mcmc$IndexT==j)
    hist(mcmc[index,i], main=perks[j+1], xlab="", ylab="", xlim=xlims, axes=F,
         breaks=20)
    ##text(min(xlims), length(index)/4, paste0("N=",length(mcmc[index,i])),
    ##     cex=0.7)
    axis(1)
  }
}

## function to compute effective sample sizes
compute_neff = function(mcmc, n_temper, index_parms) {
  ## index_parms is a vector of column numbers (parameters) for which n_eff
  ## should be calculated
  library(rstan)
  index  = which(mcmc$IndexT == n_temper-1)
  n.samples = length(index)
  n.chains  = 1
  n.parms   = length(index_parms)
  sims = array(0, c(n.samples, n.chains, n.parms))
  sims[,1,] = as.matrix(mcmc[index, index_parms])
  mysummary = monitor(sims, warmup=0, probs=NA, digit=4)
  return(c(mysummary[1,"n_eff"], n.samples))
}

# ====================================================== #
all.perks = get.all.perks("linear2.LTMCMC.out.perks")

## plot the evolution of the temperature scale
pdf("Perk scale evolution.pdf")
plot.all.perks(all.perks, mylog="")
dev.off()

## plot the evolution of the slopes of the temperature scale
plot.all.perks.slopes(all.perks, mylog="y")

## read tempered mcmc output
mcmc = read.delim("linear2.LTMCMC.out")

## check
head(mcmc[grep("LnPseudoPrior.",names(mcmc))])

perks = get.final.perks("linear2.LTMCMC.out.perks")
n_temper = length(perks)
n_temper

## plot last perks and pseudo-priors
par(mar=c(5, 5, 2, 1))
lnpseudos = as.numeric(all.perks$lnpi[dim(all.perks$lnpi)[1],])
lnpseudos = lnpseudos - min(lnpseudos, na.rm=T) + 1
lnpseudos = lnpseudos[1:which(perks==1)]
plot(as.numeric(perks), lnpseudos, las=1,
     xlab="Perks", ylab="Log pseudo-prior", cex.lab=1.4,
     type="b", lwd=2, xlim=c(1E-7,1), log="xy")

## plot the temperatures visits
pdf("Temperature visits.pdf")
plot.perks.visits(mcmc, n_temper)
dev.off()
table(mcmc$IndexT)

## plot Robbins-Monro pseudo-priors updating 
plot.Robbins_Monro(mcmc, n_temper, 4, 4)

## plot the data (remember, for the model y = z)
pdf("Data and predictions.pdf")
par(mar=c(5, 5, 7, 7), xpd=F)
x = seq(1, 10, 1);
y = c(-1.006879, -2.001636, -2.993538, -3.994545, -5.008485, -6.006281,
      -6.977793, -8.004303, -8.999835, -9.992217);
z = c(+1.006879, +2.001636, +2.993538, +3.994545, +5.008485, +6.006281,
      +6.977793, +8.004303, +8.999835, +9.992217);
## dev.new()
xlims = c(0, 10)
ylims = range(c(y, z))
plot(x, y, xlim=xlims, ylim=ylims, type="n", pch=16, col="red", las=1,
     cex.lab=1.5, bty="n")
## abline(a=0, b=1) # true line, but it's obvious
## add some random simulations at target
## mycols = rev(rainbow(n_temper))
mycols = rev(heat.colors(n_temper))
for (j in 1:n_temper) {
  index = which(mcmc$IndexT == j - 1)
  for (i in 1:20) {
    abline(0, mcmc$B.1.[sample(index, 1)], col=mycols[j])
  }
}
points(x, y, pch=16)
points(x, z, pch=16)
box()
par(xpd=T)
legend("topright", inset=c(-0.27,0), legend=perks, lty=1, col=mycols,
       title="Perks")
dev.off()

## plot linear2 posterior in 2D
#dev.new()
plot.lin2.2D(mcmc)

## plot linear2 posterior in 3D
## plot.lin2.3D(mcmc) # Have problem in rendering
library(rgl)
open3d()
par3d(windowRect=c(20, 60, 770, 810))
## good.par = par3d()
## good.par$userMatrix
my.userMatrix = matrix(
  c( 0.7632413, -0.6460999, 0.004215047,    0,
     0.1214922,  0.1499206, 0.981205046,    0,
     -0.6345884, -0.7483841, 0.192921668,    0,
     0.0000000,  0.0000000, 0.000000000,    1), byrow=T, 4, 4)
view3d(fov=30, interactive=F, userMatrix=my.userMatrix)
index = which(mcmc$IndexT == n_temper - 1)
plot3d(x=mcmc$SD_y[index], y=mcmc$SD_z[index], z=mcmc$B.1.[index],
       type="s", size=0.25, zlim=c(-2.3,2.3),
       xlab="", ylab="", zlab="", col="red")
## set to True if you do not want the next commands would change the plot size
par3d(ignoreExtent=T) 
## add text, such as axes labels
text3d(x=+15, y=-7,  z=-2.5, "SD2", family="sans", cex=1.5, col="black")
text3d(x=-12, y=+15, z=-2.5, "SD1", family="sans", cex=1.5, col="black")
text3d(x=-6,  y=30,  z=1.7, "Slope",    family="sans", cex=1.5, col="black")
#rgl.snapshot("Joint posterior sample at target - 3D.png", fmt = "png", top =F)

## Show posterior histograms for a parameter at a time
pdf("Posterior histograms slope.pdf")
plot.post.histograms(mcmc, n_temper, parm=1, 3, 4)
dev.off()
pdf("Posterior histograms SDy.pdf")
plot.post.histograms(mcmc, n_temper, parm=2, 3, 4)
dev.off()
pdf("Posterior histograms SDz.pdf")
plot.post.histograms(mcmc, n_temper, parm=3, 3, 4)
dev.off()

## an histogram for the slope at target
index = which(mcmc$IndexT == n_temper - 1)
hist(mcmc$B.1.[index], breaks=2000, xlim=c(-1.5,1.5))

## nice plot of the posterior versus B values
pdf("Slope posterior smooth.pdf")
i2 = order(mcmc$B.1.[index])
x = (mcmc$B.1.[index])[i2]
y = exp((mcmc$LnPosterior[index])[i2])
par(mfrow=c(1,2))
## first panel
par(mar=c(4, 2, 2, 0))
xlims = c(-1.015, -0.985)
## plot(x, y, type="p", bty="n", yaxt="n",
##      pch=16, xlim=xlims, xlab="Slope", cex=0.25, ylab="")
## par(new=T)
m1 = mean(x[x > xlims[1] & x < xlims[2]])
s1 = sd(x[x > xlims[1] & x < xlims[2]])
x2 = seq(xlims[1], xlims[2], (xlims[2] - xlims[1])/300)
plot(x2, dnorm(x2, m1, s1), xlim=xlims, type="l", xaxt="n", yaxt="n",
     xlab="", ylab="", bty="n", col="red", lwd=2)
axis(1, at=c(xlims[1], -1.01, -1, -0.99, xlims[2]),
     labels=c("", "-1.01", "-1", "-0.99", ""), line=0.2)
mtext("...", side=1, at=c(-0.9828, 0))
mtext("Slope", side=1, line=2.5, at=-0.9828, cex=2)
par(xpd=NA)
lines(c(-0.9835, -0.9825), c(-10, 10)) # break marks
lines(c(-0.9825, -0.9815), c(-10, 10)) # break marks
par(xpd=F)
## second panel
par(mar=c(4, 1, 2, 2))
xlims = rev(-c(-1.015, -0.985))
## plot(x, y, type="p", bty="n", yaxt="n",
##      pch=16, xlim=xlims, xlab="Slope", cex=0.25, ylab="")
## par(new=T)
m1 = mean(x[x > xlims[1] & x < xlims[2]])
s1 = sd(x[x > xlims[1] & x < xlims[2]])
x2 = seq(xlims[1], xlims[2], (xlims[2] - xlims[1])/300)
plot(x2, dnorm(x2, m1, s1), xlim=xlims, type="l", xaxt="n", yaxt="n",
     xlab="", ylab="", bty="n", col="red", lwd=2)
axis(1, at=c(xlims[1], rev(-c(-1.01, -1, -0.99)), xlims[2]),
     labels=c("", "0.99", "1", "1.01", ""), line=0.2)
dev.off()

## alternative nice plot of the posterior versus B values
pdf("Slope posterior sample.pdf")
i2 = order(mcmc$B.1.[index])
x = (mcmc$B.1.[index])[i2]
y = exp((mcmc$LnPosterior[index])[i2])
par(mfrow=c(1,2))
## first panel
par(mar=c(4, 2, 2, 0))
xlims = c(-1.015, -0.985)
plot(x, y, type="p", bty="n", xaxt="n", yaxt="n",
     pch=16, xlim=xlims, xlab="", cex=0.3, ylab="")
axis(1, at=c(xlims[1], -1.01, -1, -0.99, xlims[2]),
     labels=c("", "-1.01", "-1", "-0.99", ""), line=0.2)
mtext("...", side=1, at=c(-0.9828, 0))
mtext("Slope", side=1, line=2.5, at=-0.9828, cex=2)
## second panel
par(mar=c(4, 1, 2, 2))
xlims = rev(-c(-1.015, -0.985))
plot(x, y, type="p", bty="n", xaxt="n", yaxt="n",
     pch=16, xlim=xlims, xlab="", cex=0.3, ylab="")
axis(1, at=c(xlims[1], rev(-c(-1.01, -1, -0.99)), xlims[2]),
     labels=c("", "0.99", "1", "1.01", ""), line=0.2)
dev.off()

## plot trajectory
plot(mcmc$B.1.[index], type="l", pch=".")
plot(mcmc$B.1.[index], type="p", pch=".")

## computed effective number of samples at target
compute_neff(mcmc, n_temper, 2:4)

## miscellaneous calculations

## volume of the prior parameter space:
V = 20 * (100 - 0.001)^2
## = 199996
## volume of the prior parameter space show on Figure 4 of the paper:
Vf = 5 * 30 * 30
## = 4500
## Vf is 100 * Vf / V = 2.25% of V  
## approximate volume of the envelope of the posterior parameter sample
## (two cones of base diameter 3E-3 and height 25)
v = 2 * (3E-3 / 2)^2 * 25 * pi / 3
## = 0.000118
## v is 100 * v / V = 6E-8% of V

## distance between the two peaks for slope
pnorm(2, 0, 1E-3, log.p=T)


