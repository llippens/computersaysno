# Setup
source(file.path(here::here(), "code", "csn_setup.R"))


# Analyses
## Base models
lnr.base.m <- lm(paste0(lnr.frml, "can_minority"),
                 data = data.csn)
lnr.base.i <- lm(paste0(lnr.frml, "can_identity"),
                 data = data.csn)
lnr.base.sex <- lm(paste0(lnr.frml, "can_sex"),
                   data = data.csn)

lnr <- list(minority = lnr.base.m,
            identity = lnr.base.i,
            sex = lnr.base.sex)

## Linear models
list.het.job <- list()

for(base in c("minority", "identity")){
  list.het.job[[base]] <- lnr[[base]] %>%
    update(as.formula(
      paste(model.link,
            paste0(gpt.vars.c, collapse = " + "),
            paste0(vac.vars.c, collapse = " + "),
            paste0("can_", base, "*can_sex"),
            paste0("can_", base, "*can_occupation"),
            paste0("can_", base, "*vac_experience_requested"),
            paste0(str_c("can_", base, " * ", vac.vars.jt), collapse = " + "),
            paste0(str_c("can_", base, " * ", vac.vars.lang), collapse = " + "),
            sep = " + ")
    ))
}

list.het.job[["sex"]] <- lnr[["sex"]] %>%
  update(as.formula(
    paste(model.link,
          paste0(gpt.vars.c, collapse = " + "),
          paste0(vac.vars.c, collapse = " + "),
          paste0("can_sex", "*can_minority"),
          paste0("can_sex", "*can_occupation"),
          paste0("can_sex", "*vac_experience_requested"),
          paste0(str_c("can_sex", " * ", vac.vars.jt), collapse = " + "),
          paste0(str_c("can_sex", " * ", vac.vars.lang), collapse = " + "),
          sep = " + ")
  ))

list.het.job[["sex_female_occ"]] <- lnr[["sex"]] %>%
  update(as.formula(
    paste(model.link,
          paste0(gpt.vars.c, collapse = " + "),
          paste0(vac.vars.c, collapse = " + "),
          paste0("can_sex", "*can_minority"),
          paste0("can_sex", "*can_isco08_female"),
          paste0("can_sex", "*vac_experience_requested"),
          paste0(str_c("can_sex", " * ", vac.vars.jt), collapse = " + "),
          paste0(str_c("can_sex", " * ", vac.vars.lang), collapse = " + "),
          sep = " + ")
  ))
