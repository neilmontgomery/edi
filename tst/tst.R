library(tidyverse)
library(readxl)
library(lubridate)


## Load the functions
library(edi)

curr_file <- "tst/20200430.xlsx"

pts <- read_excel(curr_file, sheet="pts", col_names=FALSE)
pts <- pts[!duplicated(pts$...3), ]
names(pts)[c(1,2,3,6,5)] <- c("Last.Name", "First.Name", "PHN", "Home.Phone","Sex")
hcn=sprintf("%-12s", pts[[3]])
dob=format(as.Date(pts[[4]]), "%Y%m%d")
ln=sprintf("%-9s", substr(gsub("[^A-Z]", "", toupper(pts[[1]])), 1, 9))
fn=sprintf("%-5s", substr(gsub("[^A-Z]", "", toupper(pts[[2]])), 1, 5))
sx=ifelse(pts[[5]]=="M", 1, 2)

pl <- data.frame(hcn, dob, ln, fn, sx, stringsAsFactors = FALSE)

ref <- read_excel(curr_file, sheet="ref", col_names =  FALSE) %>% pull(...1)
refer <- tibble(refs = gsub("  "," ",sub(" *([0-9]{5,6}).*",",\\1",ref)))

refer <- refer %>%
  filter(grepl("[0-9]{5,6}", refs))%>%
  separate(refs, c("name", "number"), ",") %>%
  distinct(name,number)

curr_tab <- "phone"
ph <- read_excel(curr_file, sheet = curr_tab, col_names = FALSE)

ph <- ph %>% select(hcn = ...3, s.date = ...9, type = ...14) %>%
  mutate(s.date = as.Date(s.date)) %>%
  filter(s.date >= ymd(20200301), s.date <= ymd(20200430)) %>%
  mutate(type = str_replace_na(type, ""),
         type = str_replace(type, "^[Pp]h.*", "ph"),
         type = str_replace(type, "^(?!ph).*", ""))

curr_tab <- "todo"
enc_orig <- read_excel(curr_file, sheet=curr_tab, col_names=FALSE)
enc <- enc_orig[,c(3, 4, 22, 23)]
names(enc) <- c("hcn","s.date","ref","cd")

enc <- enc %>% mutate(ref = gsub(",","",ref))

## Get rid of referral names when not needed
enc[!grepl("a135", enc$cd),"ref"]  <- NA
## Replace referral names with numbers
enc <- merge(enc, refer, by.x="ref", by.y="name", all.x=T)
enc %>% filter(is.na(number) | number == "000000", grepl("a135", cd))
enc <- select(enc, hcn, s.date, ref=number, cd)

enc$cd <- gsub("(\\.|;)", ",", enc$cd)
enc$hcn <- sprintf("%-12s", enc$hcn)
enc$s.date <- as.Date(enc$s.date, format="%Y-%b-%d")
enc$ref <- sapply(as.numeric(enc$ref), function(x) ifelse(!is.na(x), sprintf("%06g", x), spaces(6)))
enc = enc[order(enc$s.date),]

# Append "ph" for phone visits
enc <- left_join(enc, ph) %>%
  mutate(type = str_replace_na(type, ""),
         cd = str_c(cd, type, sep=","),
         cd = str_replace(cd, ",$","")) %>%
  select(-type)

# # infusions
# enc <- read_excel("infusions.xlsx", sheet = "Sheet1")
# enc$cd <- "g381,714"

### Make the H file

## Load all the pfiles

allp.flat <- unlist(claims(dir("../pfiles/", full.names=T)),F,F)
allp.flat <- claims("tst/PF021202.020")

## Remove invalids
allp.flat <- lapply(allp.flat, function(x) x[!substring(x, 45, 46) %in% c("MR", "35")])
allp.flat <- allp.flat[-which(sapply(allp.flat, length)==1)]
allp.flat <- allp.flat[-which(!grepl("[AK]48.A", sapply(allp.flat, function(x) x[2], simplify = TRUE)))]

genH(hname(directory = "tst"), enc, allp.flat, pl, directory = "tst")

