
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
tubecrds = tubecrds[tubecrds$tube != 15, c('tube','tube.east','tube.north')]
head(tubecrds)

# merge tube coords with productivity ------------------------------------------

prod = merge(prod, tubecrds, by.x = c('tube'), by.y = c('tube'), all=TRUE)
head(prod)
dim(prod)

# average productivity at the tube scale --------------------------------------

prod_agg = aggregate(prod[ , c('root.len.prod', 'root.len.mort', 'root.len.stand',
                               'rhiz.len.prod', 'rhiz.len.mort', 'rhiz.len.stand',
                               'myc.prod', 'myc.mort', 'myc.stand', 'tube.east',
                               'tube.north')], 
                     by=list(tube=prod$tube, session=prod$session),
                     mean, na.rm=T)
# add back the metadata pertaining to the experiment
prod_agg = data.frame(prod_agg, ring=NA, block=NA, co2treat=NA, ntreat=NA)
for(i in 1:nrow(prod_agg)) {
    prod_agg$ring[i] = prod$ring[prod$tube == prod_agg$tube[i] &
                                 prod$session == prod_agg$session[i]][1]
    prod_agg$ntreat[i] = prod$ntreat[prod$tube == prod_agg$tube[i] &
                                     prod$session == prod_agg$session[i]][1]
    prod_agg$co2treat[i] = prod$co2treat[prod$tube == prod_agg$tube[i] &
                                         prod$session == prod_agg$session[i]][1]
    prod_agg$block[i] = prod$block[prod$tube == prod_agg$tube[i] &
                                   prod$session == prod_agg$session[i]][1]
}


head(prod_agg)
dim(prod_agg)

prod = prod_agg

# drop records without spatial coordinates
prod = subset(prod, subset=!is.na(tube.east))

# output data products --------------------------------------------------------
write.csv(prod, file='./data/prod.csv', row.names=F)
save(prod, file='./data/prod.Rdata')
