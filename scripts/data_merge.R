
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

demo = merge(root, rhiz, all=T)
demo = merge(demo, myc, all=T)
head(demo)

# convert fulldate to a date string
demo$fulldate = as.Date(demo$fulldate, format="%m/%d/%Y")

# convert tube to a unique number
tubecrds$tube = (tubecrds$ring - 1)*12 + tubecrds$tube
# drop dead tube 15 and unneeded ring column
tubecrds = tubecrds[tubecrds$tube != 15, c('tube','tube.east','tube.north')]
head(tubecrds)

# merge tube coords with demography ------------------------------------------

demo = merge(demo, tubecrds, by.x = c('tube'), by.y = c('tube'), all=TRUE)
head(demo)
dim(demo)

# average demography at the tube scale --------------------------------------

demo_agg = aggregate(demo[ , c('root.len.prod', 'root.len.mort', 'root.len.stand',
                               'rhiz.len.prod', 'rhiz.len.mort', 'rhiz.len.stand',
                               'myc.prod', 'myc.mort', 'myc.stand', 'tube.east',
                               'tube.north')], 
                     by=list(tube=demo$tube, session=demo$session),
                     mean, na.rm=T)
# add back the metadata pertaining to the experiment
fields = c('ring', 'block', 'co2treat', 'ntreat', 'fulldate')
nfields = length(fields)
col_index = ncol(demo_agg) + 1
demo_agg = data.frame(demo_agg, matrix(NA, nrow=nrow(demo_agg), ncol=nfields))
names(demo_agg) = c(names(demo_agg)[1:(col_index - 1)], fields)
class(demo_agg$fulldate) = "Date"

for(i in 1:nrow(demo_agg)) {
    row_index = which(demo$tube == demo_agg$tube[i] & 
                      demo$session == demo_agg$session[i])[1]
    demo_agg[i, col_index:ncol(demo_agg)] = demo[row_index, fields]
}


head(demo_agg)
dim(demo_agg)

demo = demo_agg

# output data products --------------------------------------------------------
# convert co2treat to 0 = ambient ; 1 = elevated
demo$co2treat = ifelse(demo$co2treat == 'AMBIENT', 0, 1) 
write.csv(demo, file='./data/demo_root_rhiz_myco_avg_across_frames.csv', row.names=F)
