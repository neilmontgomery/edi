## May 1, 2014 procedure amounts
amt <- function(pcode, newSOB = FALSE) {
    return(switch(pcode, "A480A"=89.95 + newSOB * 2.25,
                  "A481A"=70.90 + newSOB * 1.75,
                  "A483A"=79.85 + newSOB * 1.85,
                  "A484A"=61.25 + newSOB * 1.35,
                  "A485A"=157.00 + newSOB * 13.10,
                  "A486A"=105.25 + newSOB * 4.10,
                  "A488A"=38.05 + newSOB * 1.05,
                  "A765A"=165.50,
                  "E542A"=11.15 + newSOB * 0.40,
                  "G328A"=39.80,
                  "G329A"=20.25,
                  "G345A"=75.00,
                  "G370A"=20.25,
                  "G371A"=19.90,
                  "G372A"=3.89,
                  "G381A"=54.25,
                  "G382A"=13.30 + newSOB * 0.50,
                  "K050A"=100.00 + newSOB * 3.55,
                  "K481A"=75.00,
                  "K036A"=10.25,
                  "K054A"=25.00 + newSOB * 0.90,
                  "L810A"=22.05 + newSOB * 1.35))
}

## Determine batchID based on this day's files already there in
## "directory". Format: last digit of year, two digits month, 0..9
batchID <- function(directory="hfiles") {
    ## Today's date
    today <- format(Sys.Date(), "%Y%m%d")
    ## Get the H files excluding emacs ~ files
    hfn <- grep('^H.*[^~]$', dir(directory), value=T)
    ## If there aren't any...
    if(!length(hfn)) {
        bID <- "0000"
    } else {
        ## Get all the batch IDs
        bid <- substring(sapply(paste(directory, "//", hfn, sep=""),
                                function(x) readLines(x, 1)), 8, 19)
        ## Find today's
        tbid <- grep(today, bid, value=T)
        ## If there aren't any
        if(!length(tbid)) {
            bID <- "0000"
        } else {
            ## Find the largest and increment it
            bID <- substring(as.character(as.numeric(max(tbid)) + 1), 9, 12)
        }
    }
    return(bID)
}

## Find the "largest" accounting number used so far this month
lastAN <- function(directory="hfiles") {
    today <- Sys.Date()
    m.alpha <- LETTERS[as.numeric(format(today, "%m"))]
    tmh.regexp <- paste("^H", m.alpha, ".*[^~]$", sep="")
    ## List of H files for this month
    hfn <- grep(tmh.regexp, dir(directory), value=T)
    ## If there aren't any, make it yymm0001
    if(!length(hfn)) {
        an <- as.numeric(paste(format(today, "%y%m"), "0000", sep=""))
    } else {
        ## If there are, find the biggest, which has to be the last
        ## one in the last file
        lf <- max(hfn)
        ln <- substring(tail(grep("HEH",
                                  readLines(paste(directory,"//",lf,sep="")),
                                  value=T),
                             1),
                        24, 31)
        an <- as.numeric(ln)
    }
    return(an)
}

## Returns the "next" H filename to us
hname <- function(directory="hfiles", bn="021202") {
    today <- Sys.Date()
    m.alpha <- LETTERS[as.numeric(format(today, "%m"))]
    tmh.regexp <- paste("^H", m.alpha, ".*[^~]$", sep="")
    ## List of H files for this month
    hfn <- grep(tmh.regexp, dir(directory), value=T)
    ## If none
    if(!length(hfn)) {
       suf <- "001"
    } else {
        suf <- sprintf("%03g", as.numeric(substring(max(hfn), 10, 12)) + 1)

    }
    hn <- paste("H", m.alpha, bn, ".", suf, sep="")
    return(hn)
}

## Current referral base list (using old Excel file for now)
## visit.type <- function(pn, da, dx, visits) {
visit.type <- function(pn, da, dx, visits) {
    require(lubridate)
    re <- c("A133A", "A134A", "A131A",
            "A483A", "A484A", "A481A")

    vpdx <- c("710", "714", "720", "721")

    pn <- substr(pn, 1, 10)
    vpn <- visits$HCN

    since <- da - years(1)
    v <- visits[vpn==pn & visits$Date > since,]$Code

    n.re <- sum(v %in% re)
    n.0 <- sum(v %in% "A480A")
    is.133 <- sum(v %in% c("A133A", "A483A"))

    if(dx %in% vpdx) {
        if(!is.133) {
            vt <- "A483A"
        } else if(n.re < 4) {
            vt <- "A481A"
        } else if(n.0 < 6) {
            vt <- "A480A"
        } else {
            vt <- "A488A"
        }
    } else {
        if(!is.133) {
            vt <- "A483A"
        } else if(n.re < 2) {
            vt <- "A484A"
        } else {
            vt <- "A488A"
        }
    }
    return(vt)
}

## Prioritize 714, then any premium-eligibles, then take the first
dx.priority <- function(x) {
    x <- gsub("433","443",x)
    other <- c("710", "720", "721")
    if("714" %in% x) {
        return("714")
    } else if(sum(other %in% x)) {
        return(other[which(other %in% x)[1]])
    } else {
        return(x[1])
    }
}

## Produce n spaces
spaces <- function(n) {
    return(paste(rep(" ", n), collapse=""))
}

## Make the H file

genH <- function(fname, enc, pclaims, pl, directory="hfiles", debug = FALSE) {
    ## Find the batchID if more than one file made "today"
    id <- batchID(directory)
    ## Find the "next" accounting number
    an <- lastAN(directory)

    ## Initialize
    fl <- NULL

    nonG381 <- sapply(pclaims, function(x) !grepl("G381A", x[[2]]))
    pclaims <- pclaims[nonG381]
    HCN <- sapply(pclaims,
        function(x) as.numeric(substring(grep("^HR4", x, value=T), 53, 62)))
    Date <- as.Date(unlist(sapply(pclaims,
        function(x) substring(grep("[AK]48.A", x, value=T), 16, 23))), "%Y%m%d")
    Code <- as.character(sapply(pclaims,
        function(x) substring(grep("[AK]48.A", x, value=T), 26, 30)))

    visits <- data.frame(HCN, Date, Code)

    ## Header line
    fl <- c(fl, paste("HEBV03E",
                      format(Sys.Date(),"%Y%m%d"),
                      id,
                      spaces(6),
                      "0000",
                      "02120248",
                      spaces(42),
                      sep=""))

    ## Go through the encounters
    for(i in 1:nrow(enc)) {
        RMB <- grepl("RMB", enc$cd[i])

        pl_ix <- grep(substr(enc$hcn[i], 1, 10), pl$hcn)
        ref <- enc$ref[i]
        if(debug) { print(pl$hcn[pl_ix]) }
        ## Claim header
        fl <- c(fl, paste("HEH",
                          ifelse(!RMB, pl$hcn[pl_ix], spaces(12)),   # HCN
                          pl$dob[pl_ix],  # DOB
                          an + i,
                          ifelse(grepl("ph", enc$cd[i]), "PHON",
                                 paste(ifelse(RMB, "RMB", "HCP"), "P",
                                       sep="")),
                          ref,
                          spaces(38),
                          sep=""))

        ## Item record
        ## Service date (still in date format)
        da <- enc$s.date[i]

        newSOB <- da >= as.Date("2020-04-01")
        ## Split the code
        cd <- strsplit(as.character(enc$cd[i]),'[;/,.]')[[1]]
        cd <- gsub("^\\s+|\\s+$", "", cd)
        ## Pick the diagnosis
        dx <- dx.priority(grep("^\\d{3}", cd, value=T))

        if(RMB) {
          fl <- c(fl, paste("HER",
                            pl$hcn[pl_ix],
                            pl$ln[pl_ix],
                            pl$fn[pl_ix],
                            pl$sx[pl_ix],
                            substr(grep("RMB", cd, value=T), 4,5),
                            spaces(47),
                            sep=""))
        }


        ## Visit type
        vt <- grep("^[agk][0-9]{3}", cd, perl=T, value=T)

        if(any(grepl("[aA]135", vt))) {
            vta <- "A485A"
        } else if(any(grepl("[aA]765", vt))) {
            vta <- "A765A"
        } else if(any(grepl("[aA]138", vt))) {
            vta <- "A488A"
        } else if (any(grepl("[gG]381", vt))) {
            vta <- "G381A"
        } else if (any(grepl("[gG]345", vt))) {
            vta <- "G345A"
        } else if (any(grepl("[kK]481", vt))) {
          vta <- "K481A"
        } else {
            vta <- visit.type(enc$hcn[i], da, dx, visits)
        }
        ## Number of services is always 1 for visit type
        ns <- 1
        fl <- c(fl, paste("HET",
                          vta,
                          spaces(2),
                          sprintf("%06g", ns*amt(vta, newSOB)*100),
                          sprintf("%02g", ns),
                          format(da, "%Y%m%d"),
                          dx,
                          spaces(50),
                          sep=""))

        ## Visit premium?
        vpdx = c("710", "714", "720", "721")
        if(sum(dx %in% vpdx) && !(vta %in% c("A485A", "A480A", "G345A", "G381A", "K481A"))) {
            fl <- c(fl, paste("HET",
                              "E078A",
                              spaces(2),
                              sprintf("%06g", floor(0.51 + amt(vta, newSOB)*50)),
                              sprintf("%02g", ns),
                              format(da, "%Y%m%d"),
                              dx,
                              spaces(50),
                              sep=""))
        }

        ## Aspirations

        ## First, determine number of aspirations
        first.asp = grep("370", cd)
        subs.asp = grep("371", cd)
        subs.x = grep("371[xX]", cd, value=T)
        n.asp = 0
        if(length(first.asp)) {
            n.asp = 1
        }
        if(length(subs.x)>0) {
            n.asp = n.asp + as.numeric(substr(subs.x,6,6))
        } else if(length(subs.asp)>0) {
            n.asp = n.asp + length(subs.asp)
        }

        ## If there are any, process them
        if(n.asp > 0) {
            ## Complicated?
            f.asp <- ifelse(length(grep("comp", cd)) > 0, "G328A", "G370A")
            ns=1
            fl <- c(fl, paste("HET",
                              f.asp,
                              spaces(2),
                              sprintf("%06g", ns*amt(f.asp, newSOB)*100),
                              sprintf("%02g", ns),
                              format(da, "%Y%m%d"),
                              dx,
                              spaces(50),
                              sep=""))
            ## Add the tray fee
            fl <- c(fl, paste("HET",
                              "E542A",
                              spaces(2),
                              sprintf("%06g", ns*amt("E542A", newSOB)*100),
                              sprintf("%02g", ns),
                              format(da, "%Y%m%d"),
                              dx,
                              spaces(50),
                              sep=""))
            ## Compute remaining aspirations
            n.asp <- n.asp - 1

            ## IF THERE ARE ANY SUBSEQUENT...
            ## If the first is G328, process the next two as G329...
            if(n.asp > 0) {
                if(grepl("328", f.asp)) {
                    n.add.compl <- min(n.asp, 2)
                    ns <- n.add.compl
                    fl <- c(fl, paste("HET",
                                      "G329A",
                                      spaces(2),
                                      sprintf("%06g", ns*amt("G329A", newSOB)*100),
                                      sprintf("%02g", ns),
                                      format(da, "%Y%m%d"),
                                      dx,
                                      spaces(50),
                                      sep=""))
                    n.asp <- n.asp - n.add.compl
                    ## ...any more...process NEXT as G370
                    if(n.asp > 0) {
                        ns <- 1
                        fl <- c(fl, paste("HET",
                                          "G370A",
                                          spaces(2),
                                          sprintf("%06g", ns*amt("G370A", newSOB)*100),
                                          sprintf("%02g", ns),
                                          format(da, "%Y%m%d"),
                                          dx,
                                          spaces(50),
                                          sep=""))
                        n.asp <- n.asp - 1
                    }
                }
                ## Again see if n.asp > 0, which might not be the case
                ## if the previous "if" was followed
                if(n.asp > 0) {
                    ns <- min(n.asp, 5)
                    fl <- c(fl, paste("HET",
                                      "G371A",
                                      spaces(2),
                                      sprintf("%06g", ns*amt("G371A", newSOB)*100),
                                      sprintf("%02g", ns),
                                      format(da, "%Y%m%d"),
                                      dx,
                                      spaces(50),
                                      sep=""))
                }
            }
        }

        ## Chemo CHECK TO SEE IF 6 DONE IN PAST YEAR?
        if(length(grep("382", cd)) > 0) {
            ns <- 1
            fl <- c(fl, paste("HET",
                              "G382A",
                              spaces(2),
                              sprintf("%06g", ns*amt("G382A", newSOB)*100),
                              sprintf("%02g", ns),
                              format(da, "%Y%m%d"),
                              dx,
                              spaces(50),
                              sep=""))
        }

        ## Looked at
        if(length(grep("look", cd)) > 0) {
            ns <- 1
            fl <- c(fl, paste("HET",
                              "L810A",
                              spaces(2),
                              sprintf("%06g", ns*amt("L810A", newSOB)*100),
                              sprintf("%02g", ns),
                              format(da, "%Y%m%d"),
                              dx,
                              spaces(50),
                              sep=""))
        }

        ## IM
        if(length(grep("[iI][mM]", cd)) > 0) {
            ns <- 1
            fl <- c(fl, paste("HET",
                              "G372A",
                              spaces(2),
                              sprintf("%06g", ns*amt("G372A", newSOB)*100),
                              sprintf("%02g", ns),
                              format(da, "%Y%m%d"),
                              dx,
                              spaces(50),
                              sep=""))
        }
        if(length(grep("[kK]036", cd)) > 0) {
          ns <- 1
          fl <- c(fl, paste("HET",
                            "K036A",
                            spaces(2),
                            sprintf("%06g", ns*amt("K036A", newSOB)*100),
                            sprintf("%02g", ns),
                            format(da, "%Y%m%d"),
                            dx,
                            spaces(50),
                            sep=""))
        }
        if(length(grep("[kK]054", cd)) > 0) {
          ns <- 1
          fl <- c(fl, paste("HET",
                            "K054A",
                            spaces(2),
                            sprintf("%06g", ns*amt("K054A", newSOB)*100),
                            sprintf("%02g", ns),
                            format(da, "%Y%m%d"),
                            dx,
                            spaces(50),
                            sep=""))
        }
    }

    ## RMB manual additions
    ## fl <- c(fl,
    ##         sprintf("%-79s", "HEH            1964070414080200RMBP"),
    ##         sprintf("%-79s", "HER7111278102  REHAN    FOUZI2NS"),
    ##         sprintf("%-79s", "HETA481A  0070900120140801720"),
    ##         sprintf("%-79s", "HETE078A  0035450120140801720"))


    ## Trailer
    n_HEH <- length(grep("HEH", fl)) - length(grep("HER", fl))
    n_HER <- length(grep("HER", fl))
    n_HET <- length(grep("HET", fl))


    fl <- c(fl, paste("HEE",
                      sprintf("%04g", n_HEH),
                      sprintf("%04g", n_HER),
                      sprintf("%05g", n_HET),
                      spaces(63),
                      sep=""))

    fc <- file(paste(directory, "//", fname, sep=""))
    writeLines(fl, fc, sep="\r")
    close(fc)
}

total <- function(hfc) {
    ln <- readLines(hfc)
    fh.rx <- "^(HEB|HX1|HR1)"
    type <- regmatches(ln[1], regexpr(fh.rx, ln[1]))
    if(type=="HEB") {
        start <- 11; end <- 16; rx <- "HET"
    } else if(type=="HX1") {
        start <- 11; end <- 16; rx <- "HXT"
    } else {
        start <- 38; end <- 43; rx <- "HR5"
    }
    sum(as.numeric(substring(grep(rx, ln, value=T), start, end))/100)
}

totals <- Vectorize(total)

checklines<- function(hfc) {
    clms <- claims(hfc)
    return(clms[which(sapply(clms, function(x) any(nchar(x) != 79)))])
}

## Returns a list of character vectors - one claim per vector - names
## are accounting codes (if all present)
claims.raw <- function(hfc) {
    ln <- readLines(hfc)
    ## Strip irrelevant lines
    ln <- grep("^(HEH|HET|HXH|HXT|HR4|HR5)", ln, value=T)
    ## Find find claim indices
    idx <- cumsum(grepl('^(HEH|HXH|HR4)', ln))
    ## Split lines by claim index
    lnl <- split(ln, idx)
    ## Unique accounting codes?
    acc <- substring(grep('^(HEH|HXH|HR4)', ln, value=T), 24, 31)
    n.claims <- length(lnl)
    n.acc <- nrow(table(acc))
    if(n.claims==n.acc) {
        ## Then rename by accounting codes
        names(lnl) <- acc
    }
    return(lnl)
}

claims <- Vectorize(claims.raw)



e.to.email <- function(efile, pts, ecodes) {
    ## Separate into claims
    e.list <- claims(efile)
    err.table <- NULL
    for(claim in e.list) {
        ## Interpret errors
        errors <- unique(grep(" ", substring(claim, 65, 67), value=T, invert=T))
        full.error <- ecodes[which(ecodes$Code %in% errors),2]

        ## Get service date
        this.date <- as.Date(substring(claim[2], 19, 26), "%Y%m%d")

        ## Get pt hcn
        this.hcn <- substring(claim[1], 4, 13)
        this.vc <- substring(claim[1], 14, 15)
        which.pt <- grep(this.hcn, pts$PHN)
        ## pt details
        this.pt <- pts[which.pt, c("Last.Name",
                                   "First.Name",
                                   "Home.Phone")]

        this.pt <- this.pt[rep(seq_len(nrow(this.pt)), each=length(full.error)),]

        this.pt <- tibble::tibble(Date = this.date,
                                  Error = full.error,
                                  Last = this.pt$Last.Name,
                                  First = this.pt$First.Name,
                                  Phone = this.pt$Home.Phone,
                                  this.hcn,
                                  this.vc,
                                  claim = list(claim))

        err.table <- rbind(err.table, this.pt)
    }

    return(err.table)
}

oname <- function(directory="ofiles", response="E") {
    return(paste0(directory, "/", "OBEC", response,
                  sprintf("%03g", as.numeric(format(Sys.Date(),"%W"))),
                  ".TXT"))
}

genOBEC <- function(ofc, hcns) {
    fl <- paste0("OBEC01",
                 sprintf("%-12s", hcns))
    writeLines(fl, ofc, sep="\r")
}

interpretOBEC <- function(outfileXLSX, obec, response, codes="oCodes.csv") {
    require(XLConnect)
    ## Load the codes
    oCodes <- read.csv(codes, stringsAsFactors=F)
    ## Load the appointments cut and paste from Accuro - unfortunately
    ## columns hardcoded. Remove whitespace and format HCNs
    appts <- readWorksheetFromFile(obec, "Sheet1", header=F)[,c(1,2,3,6,9,10,12)]
    names(appts) <- c("last","first","hcn","phone","appt","type","time")
    appts$first <- gsub("^\\s+|\\s+$", "", appts$first)
    appts$hcn <- sprintf("%-12s", appts$hcn)

    ## Get the OBEC response
    resp <- readLines(response)
    ## Process into hcn and relevant codes
    hcn.code <- lapply(resp, function(x) substring(x, c(1,13), c(12,14)))
    invals <- hcn.code[!sapply(hcn.code, function(x) grepl("^5", x[2]))]
    warns <- hcn.code[sapply(hcn.code, function(x) grepl("^5[2-9]", x[2]))]

    ## Make the output table
    if(length(invals)) {
        out <- data.frame(appts[which(appts$hcn %in%
                                        sapply(invals, function(x) x[1])),],
                          reason=oCodes$Reason[sapply(sapply(
                              c(invals,warns), function(x) x[2]),
                              function(x) grep(x, oCodes$Code))])
        ## Write the worksheet
        of <- loadWorkbook(outfileXLSX, create=TRUE)
        sn <- "Upcoming Health Cards"
        createSheet(of, sn)
        out$appt <- as.character(out$appt)
        writeWorksheet(of, out, sn)
        ## Colour the invals pink
        if(length(invals)) {
            inval.rows <- createCellStyle(of)
            setFillForegroundColor(inval.rows, color=XLC$COLOR.ROSE)
            setFillPattern(inval.rows, fill=XLC$FILL.SOLID_FOREGROUND)
            setCellStyle(of, formula=paste0(sn, "!A2:H", length(invals)+1),
                         cellstyle=inval.rows)
        }
        ## Colour the warns light blue
        if(length(warns)) {
            warn.rows <- createCellStyle(of)
            setFillForegroundColor(warn.rows, color=XLC$COLOR.LIGHT_YELLOW)
            setFillPattern(warn.rows, fill=XLC$FILL.SOLID_FOREGROUND)
            setCellStyle(of, formula=paste0(sn, "!A",length(invals)+2,":H",
                                 length(invals)+length(warns)+1), cellstyle=warn.rows)
        }

        invisible(sapply(1:8, function(x) { setColumnWidth(of, sn, x) }))
        saveWorkbook(of)
        return(out)
    } else {
        return(data.frame())
    }
}

# Count 714

p_tbl <- function(pcl) {
  hcn <- sapply(pcl, function(x) substring(x[1], 53, 62))
  vc <- sapply(pcl, function(x) substring(x[1], 65, 66))
  dt <- ymd(sapply(pcl, function(x) substring(x[2], 16, 23)))
  vt <- sapply(pcl, function(x) substring(x[2], 26, 29))
  return(data_frame(hcn, vc, dt, vt))
}

h_tbl <- function(hcl) {
  hcn <- sapply(hcl, function(x) substring(x[1], 4, 13))
  vc <- sapply(hcl, function(x) substring(x[1], 14, 15))
  dt <- ymd(sapply(hcl, function(x) substring(x[2], 19, 26)))
  dob <- sapply(hcl, function(x) substring(x[1], 16, 23))
  dx <- sapply(hcl, function(x) substring(x[2], 27, 29))
  return(data_frame(hcn, vc, dt, dob, dx))
}

ecl_to_enc <- function(x) {
  hc_num <- substr(x[1], 4, 13)
  hc_vc <- substr(x[1], 14, 15)
  s.date <- lubridate::ymd(substr(x[2], 19, 26))
  ref <- substr(x[1], 36, 41)
  codes <- sub("a48", "a13", tolower(substr(x[-1],4,7)))
  n_s <- strtoi(substr(x[-1], 17, 18))

  # Encounter
  cd <- grep("a13", codes, value = TRUE)
  if(!str_detect(cd, "a13[58]")) {
    cd <- "a133"
  }

  # Top-up?
  cd <- c(cd, grep("e078", codes, value = TRUE))

  # Monitoring?
  cd <- c(cd, grep("g382", codes, value = TRUE))

  # Injections?
  complic <- grep("g32[89]", codes)
  complic_n <- sum(n_s[complic])
  regular <- grep("g37[01]", codes)
  regular_n <- sum(n_s[regular])
  n_inj <- sum(c(complic_n, regular_n))

  if (length(n_inj)) {
    if (n_inj) {
      cd <- c(cd, "g370")
    }

    if (n_inj == 2) {
      cd <- c(cd, "g371")
    } else if (n_inj > 2) {
      cd <- c(cd, paste0("g371X", n_inj-1))
    }


    if (complic_n) {
      cd <- c(cd, paste0("complicX", complic_n))
    }
  }

  # Looked at?
  looked <- grep("810", codes)
  n_looked <- n_s[looked]

  if(length(n_looked)) {
    cd <- c(cd, paste0("lookX", n_looked))
  }

  # dx
  dx <- substr(x[2], 27, 29)

  data.frame(hc_num, hc_vc, s.date, ref, cd = paste(cd, collapse = ","), dx, stringsAsFactors = FALSE)
}


to_phone <- function(hfile, directory = "hfiles") {
  hfname <- strsplit(basename(hfile), "\\.")[[1]]
  fname <- paste(hfname[1], ".", 1000 - as.numeric(hfname[2]), sep="")

  fl <- readLines(hfile, 1) # copy first line

  cls <- claims.raw(hfile)

  for (claim in cls) {
    fl <- c(fl, sub("PHON[0-9 ]{6}", "HCPP      ", claim[1]))
    if (grepl("PHON", claim[1])) {
      da <- substr(claim[2], 19, 26)
      dx <- substr(claim[2], 27, 29)
      ns <- round(sum(as.numeric(substr(claim[-1], 11, 16)) / 100) / 5)
      amount <- 5 * ns
      fl <- c(fl, paste(
        "HET",
        "K083A",
        spaces(2),
        sprintf("%06g", amount * 100),
        sprintf("%02g", ns),
        da,
        dx,
        spaces(50),
        sep = ""
      ))
    } else {
      fl <- c(fl, claim[-1])
    }
  }

  fl <- c(fl, paste("HEE",
                    sprintf("%04g", length(grep("HEH", fl))),
                    sprintf("%04g", length(grep("HER", fl))),
                    sprintf("%05g", sum(stringi::stri_count(fl, regex="HET"))),
                    spaces(63),
                    sep=""))

  fc <- file(paste(directory, "//", fname, sep=""))
  writeLines(fl, fc, sep="\r")
  close(fc)
  fl
}
