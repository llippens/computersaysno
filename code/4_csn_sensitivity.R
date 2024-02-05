# Setup
source(file.path(here::here(), "code", "csn_setup.R"))


## Creating cutoff variables
for (i in 0:100) {
  data.csn <- data.csn %>%
    mutate(
      !!paste0("gpt_score_", i) :=
        case_when(
          gpt_score > i ~ 1,
          gpt_score <= i ~ 0
        )
    )
}


# Sensitivity Analysis
## Logit models
lgt.c <- list()
predictions <- list()
predictions.t <- tibble(
  base = character(),
  cutoff = numeric(),
  identity = character(),
  predicted = numeric(),
  conf.low = numeric(),
  conf.high = numeric()
)

for(cutoff in c(1:100)){
  cutoff.var <- paste0("c", cutoff)
  
  lgt.frml <- paste0("gpt_score_", cutoff, " ~ ")
  
  lgt.base.m <- glm(paste0(lgt.frml, "can_minority"),
                    data = data.csn,
                    family = binomial(link = "logit"),
                    method = "brglmFit")
  lgt.base.i <- glm(paste0(lgt.frml, "can_identity"),
                    data = data.csn,
                    family = binomial(link = "logit"),
                    method = "brglmFit")
  lgt <- list(minority = lgt.base.m,
              identity = lgt.base.i)
  
  for(base in names(lgt)){
    m.id <- str_c("can_", base)
    
    lgt.c[[base]][[cutoff.var]] <- lgt[[base]] %>%
      update(as.formula(
        paste(model.link,
              paste0(gpt.vars.c, collapse = " + "),
              sep = " + ")
      ))
    
    predictions[[base]][[cutoff.var]] <-
      ggpredict(model = lgt.c[[base]][[cutoff.var]],
                terms = str_c("can_", base),
                vcov.type = "HC1",
                vcov.args = list(cluster = ~vac_uid))
    
    if(base == "minority"){
      for(i in c(1:2)){
        predictions.t <- predictions.t %>%
          add_row(
            base = base,
            cutoff = cutoff,
            identity = predictions[[base]][[cutoff.var]]$x[[i]],
            predicted = predictions[[base]][[cutoff.var]]$predicted[[i]],
            conf.low = predictions[[base]][[cutoff.var]]$conf.low[[i]],
            conf.high = predictions[[base]][[cutoff.var]]$conf.high[[i]]
          )
      }
    } else if(base == "identity"){
      for(i in c(1:9)){
        predictions.t <- predictions.t %>%
          add_row(
            base = base,
            cutoff = cutoff,
            identity = predictions[[base]][[cutoff.var]]$x[[i]],
            predicted = predictions[[base]][[cutoff.var]]$predicted[[i]],
            conf.low = predictions[[base]][[cutoff.var]]$conf.low[[i]],
            conf.high = predictions[[base]][[cutoff.var]]$conf.high[[i]]
          )
      }
    }
  }
}

predictions.t <- predictions.t %>%
  mutate(label = case_when(identity == "Yes" ~ "Other",
                           identity == "No" ~ "Dutch",
                           TRUE ~ identity))
