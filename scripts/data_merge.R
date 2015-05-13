
# import data objects from rut-system repo -----------------------------------

setwd('./rut-system/src/')
source('./build-all.R')
source('./database_help.R')
setwd('../../')

# examine productivity datasets at the frame scale --------------------------

head(productivity)
head(rhizo_productivity)
head(myc_productivity)

# merge the root, rhizo, and myco databases ---------------------------------

# first rename the fields that we do not wish to merge:
samp_fields = c("session", "fuid", "tube", "frame", "fulldate", "new_session",
                "julian1990", "ring", "block", "co2treat", "ntreat", "interval")
prod_fields = c('len.prod', 'len.mort', 'len.stand')

clean_dat = function(df, type) {
    samp_fields2 = samp_fields[samp_fields %in% names(df)]
    samp = df[ , samp_fields2]
    prod = df[ , prod_fields]
    names(prod) = paste(type, prod_fields, sep='.')
    out = cbind(samp, prod)
    return(out)
}

root = clean_dat(productivity, 'root')
rhiz = clean_dat(rhizo_productivity, 'rhiz')
myc_fields = c('tube', 'frame', 'session', 'fulldate', 'julian1990', 'ring',
               'block', 'co2treat', 'ntreat', 'fuid', 'myc.prod', 'myc.mort',
               'myc.stand')
myc = myc_productivity[ , myc_fields]

prod = merge(root, rhiz, all=T)
prod = merge(prod, myc, all=T)
head(prod)

# convert fulldate to a date string
prod$fulldate = as.Date(prod$fulldate, format="%m/%d/%Y")

# convert tube to a unique number
tubecrds$tube = (tubecrds$ring - 1)*12 + tubecrds$tube
# drop dead tube 15 and unneeded ring column
tubecrds = tubecrds[tubecrds$tube != 15, c('tube','tube.x','tube.y')]
head(tubecrds)

# merge tube coords with productivity ------------------------------------------

prod = merge(prod, tubecrds, by.x = c('tube'), by.y = c('tube'), all=TRUE)
head(prod)
dim(prod)

# average productivity at the tube scale --------------------------------------

prod_avg = aggregate(prod[ , c('root.len.prod', 'root.len.mort', 'root.len.stand',
                               'rhiz.len.prod', 'rhiz.len.mort', 'rhiz.len.stand',
                               'myc.prod', 'myc.mort', 'myc.stand', 'tube.x', 'tube.y')], 
                     by=list(tube=prod$tube, session=prod$session, 
                             ring=prod$ring, block=prod$block,
                             fulldate=prod$fulldate),
                     mean, na.rm=T)

head(prod_avg)
dim(prod_avg)

# drop records without spatial coordinates
prod_avg = subset(prod_avg, subset=!is.na(tube.x))

# explore geographic patterns --------------------------------------------

# for time being adjust spatial coords to allow for ring differences

x = (prod_avg$ring - 1) * 1000 + prod_avg$tube.x
y = (prod_avg$ring - 1) * 1000 + prod_avg$tube.y

gdist = dist(cbind(x, y))
gdist = ifelse(gdist > 25, 25, gdist)
plot(density(gdist), ylim=c(0,.02))

tdist = dist(prod_avg$fulldate)
plot(density(tdist))

root_mso = mso(rda(prod_avg$root.len.prod), prod_avg$fulldate)
msoplot(root_mso)

root_d = dist(prod_avg$root.len.prod)
pdf('./figs/root_time.pdf')
plot(tdist, root_d, type='n')
lines(lowess(tdist, root_d), col='red', lwd=3)
dev.off()
