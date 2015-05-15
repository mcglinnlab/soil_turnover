library(vario)

# import demography data
demo = read.csv('./data/demo_root_rhiz_myco_avg_across_frames.csv', 
                colClasses = c(rep('numeric', 17), 'Date'))
                
time = demo$fulldate - min(demo$fulldate)

# explore geographic patterns --------------------------------------------

# for time being adjust spatial coords to allow for ring differences

plot(demo$tube.east, demo$tube.north, xlim=c(-100, 300))
for(i in seq_along(unique(demo$block))) {
    points(demo$tube.east[demo$block == i], 
           demo$tube.north[demo$block == i], col=i, pch=19)
}

# so from a spatial perspective you've really got 3 levels
# tube < ring < block 

gdist = dist(cbind(demo$tube.east, demo$tube.north))
summary(gdist)
plot(density(gdist))
plot(density(log10(gdist+1)))

tdist = dist(demo$fulldate)
summary(tdist)
plot(density(tdist))

# vario analysis -------------------------------------------------------

# this needs to be a comparison at 4 different spatial grains
# 1) within tubes
# 2) between tubes within an N treatment
# 2) between N treatments within a ring
# 3) between rings within a block (i.e., CO2 compar)
# 4) between blocks within the site
# need to create distance matrices that reflect these structures
# possibly the easiest way would be to multiply the calculated
# distance matrix by a distance matrix of dummy matrices

# approach to only comparing objects defined by a grouping variable
n = 5
y = runif(2*n)
g = rep(1:n, 2)
gd = as.dist((as.matrix(dist(g)) ==  0)*1)
round(dist(y), 2) * gd

breaks = c(1, 30, 60, 120, 240, 480)
vt_root = vario(demo$root.len.prod, coord = time, hmin=20, hmax=480, 
                breaks=breaks, log=F)
vt_rhizo = vario(demo$rhiz.len.prod, coord = time, hmax=365)
vt_myco = vario(demo$myc.prod, coord=time, hmax=365)

pdf('./figs/vt_tube.pdf', width=7, height=7*3)
plot(vt_root$vario$Dist, vt_root$vario$exp.var, ylim=c(0, 10))
lines(lowess(vt_root$vario$Dist, vt_root$vario$exp.var, f=.05),
      col='red', lwd=2)
plot(vt_root$vario$Dist, vt_root$vario$exp.var, ylim=c(0, 10))
lines(lowess(vt_root$vario$Dist, vt_root$vario$exp.var, f=.05),
      col='red', lwd=2)
plot(vt_root$vario$Dist, vt_root$vario$exp.var, ylim=c(0, 10))
lines(lowess(vt_root$vario$Dist, vt_root$vario$exp.var, f=.05),
      col='red', lwd=2)
dev.off()

root_mso = mso(rda(demo$root.len.prod), demo$fulldate)
msoplot(root_mso)

root_d = dist(demo$root.len.prod)
pdf('./figs/root_time.pdf')
plot(tdist, root_d, type='n')
lines(lowess(tdist, root_d), col='red', lwd=3)
dev.off()