## ------------------------------------------------------------------------
library(cleanEHR)
data.path <- paste0(find.package("cleanEHR"), "/doc/sample_ccd.RData")
load(data.path)

## ------------------------------------------------------------------------
library(pander)
print(head(ccd@infotb))

## ------------------------------------------------------------------------
# quickly check how many episodes are there in the dataset.
ccd@nepisodes

## ---- fig.width=10, fig.height=11, out.width='700px', results='hide', message=FALSE, warning=FALSE----
# check the heart rate, bilirubin, fluid balance, and drugs of episode_id = 7. 
# NOTE: due to anonymisation reason, some episodes data cannot be displayed
# properly. 
episode.graph(ccd, 7, c("h_rate",  "bilirubin", "fluid_balance_d"))

## ---- fig.width=10, fig.height=6, out.width='700px', results='hide', message=FALSE, warning=FALSE----
# contains all the 1D fields i.e. non-longitudinal
tb1 <- sql.demographic.table(ccd)

# filter out all dead patient. (All patients are dead in the dataset.)
tb1 <- tb1[DIS=="D"]

# subset variables we want (ARSD = Advanced respiratory support days,
# apache_prob = APACHE II probability)
tb <- tb1[, c("SEX", "ARSD", "apache_prob"), with=F]
tb <- tb[!is.na(apache_prob)]

# plot
library(ggplot2)
ggplot(tb, aes(x=apache_prob, y=ARSD, color=SEX)) + geom_point()


## ------------------------------------------------------------------------
# To prepare a YAML configuration file like this. You write the following text
# in a YAML file. 
conf <- "
NIHR_HIC_ICU_0108:
  shortName: hrate
NIHR_HIC_ICU_0112:
  shortName: bp_sys_a
  dataItem: Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure
NIHR_HIC_ICU_0093:
   shortName: sex
"
library(yaml)
tb <- create.cctable(ccd, yaml.load(conf), freq=1)

# a lazy way to do that. 
tb <- create.cctable(ccd, list(NIHR_HIC_ICU_0108=list(), 
                         NIHR_HIC_ICU_0112=list(), 
                         NIHR_HIC_ICU_0093=list()), 
                     freq=1)
print(tb$tclean)

## ------------------------------------------------------------------------
tb$tclean[, mean(NIHR_HIC_ICU_0108, na.rm=T), by=c("site", "episode_id")]

## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
conf <-"
NIHR_HIC_ICU_0108:
  shortName: hrate
  dataItem: Heart rate
  distribution: normal
  decimal_places: 0
  range:
    labels:
      red: (0, 300)
      amber: (11, 150)
    apply: drop_entry
  missingness: # remove episode if missingness is higher than 70% in any 24 hours interval 
    labels:
      yellow: 24
    accept_2d:
      yellow: 70 
    apply: drop_episode
"

ctb <- create.cctable(ccd, yaml.load(conf), freq=1)
ctb$filter.ranges("amber") # apply range filters
ctb$filter.missingness()
ctb$apply.filters()

cptb <- rbind(cbind(ctb$torigin, data="origin"), 
              cbind(ctb$tclean, data="clean"))


ggplot(cptb, aes(x=time, y=NIHR_HIC_ICU_0108, color=data)) + 
  geom_point(size=1.5) + facet_wrap(~episode_id, scales="free_x")


