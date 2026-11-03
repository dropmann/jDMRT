# Duncan's multiple range test as an addon to jamovi's ANOVA (jmv::anova). The
# ANOVA's options (self$parent$options) give the dependent variable, factors and
# model terms, and its own table the F-tests; this adds Duncan's group letters
# for the chosen terms, and the Mean +/- S.D. report table, to its results.
# See R/duncanCompute.R
anovaDuncanClass <- R6::R6Class(
  "anovaDuncanClass",
  inherit = anovaDuncanBase,
  private = list(

    .factors = function() {
      as.character(unlist(self$parent$options$factors))
    },
    .sortTerm = function(term) {
      fn <- private$.factors()
      fn[sort(match(term, fn))]
    },
    # the ANOVA's model terms (a full factorial when it has none)
    .modelTerms = function() {
      fn <- private$.factors()
      terms <- Filter(function(t) length(t) > 0 && all(t %in% fn), self$parent$options$modelTerms)
      if (!length(terms)) terms <- .duncanFullFactorial(fn)
      terms
    },
    .init = function() {
      # the results go in the ANOVA's results, after its own
      self$parent$results$add(self$results$duncanTables)
      self$parent$results$add(self$results$duncanReport)

      fn <- private$.factors()
      for (term in self$options$duncanTerms) {
        if (!all(term %in% fn)) next
        ft <- private$.sortTerm(term)
        tb <- self$results$duncanTables$get(key = term)
        tb$setTitle(paste0("Duncan's Multiple Range Test: ", paste(ft, collapse = " × ")))
        for (i in seq_along(ft))
          tb$addColumn(name = paste0("f", i), title = ft[i], type = "text", index = i)
      }
    },

    .run = function() {
      opts  <- self$options
      popts <- self$parent$options
      fn    <- private$.factors()
      if (is.null(popts$dep) || length(fn) == 0) return()
      if (!length(opts$duncanTerms) && !opts$reportTable) return()

      d <- self$data
      facs <- lapply(fn, function(v) as.factor(d[[v]]))
      names(facs) <- fn
      terms <- private$.modelTerms()
      lab <- function(t) .duncanTermLabel(t, fn)
      termLabs <- vapply(terms, lab, "")
      inModel <- function(t) length(t) > 0 && all(t %in% fn) && lab(t) %in% termLabs
      keyOf <- function(lst) if (length(lst)) vapply(lst, lab, "") else character()
      uniq  <- function(lst) lst[!duplicated(keyOf(lst))]

      ph <- Filter(inModel, opts$duncanTerms)
      repTerms <- if (opts$reportTable) (if (length(ph)) ph else terms) else list()
      describe <- uniq(c(ph, repTerms))

      # an error is shown in this addon's results, rather than the ANOVA's
      res <- tryCatch(
        .duncanAnova(jmvcore::toNumeric(d[[popts$dep]]), facs,
                     NULL, terms = terms, posthoc = uniq(ph), describe = describe,
                     alpha = opts$alpha, adjusted = identical(opts$meanType, "adjusted")),
        error = function(e) jmvcore::reject(conditionMessage(e), code = ""))

      # the p of a term's F-test as the ANOVA shows it (so following its choice
      # of sums of squares), or the Type III one computed here
      pOf <- function(t) {
        p <- tryCatch(self$parent$results$main$getCell(rowKey = t, col = "p")$value,
                      error = function(e) NULL)
        if (is.numeric(p) && length(p) == 1 && is.finite(p)) p
        else unname(res$term_p[lab(t)])
      }
      termP <- stats::setNames(vapply(terms, pOf, 0), termLabs)

      # letters for a term: "ns" when its own F-test is not significant (if requested)
      lettersOf <- function(g, p) {
        gated <- opts$requireSignificantF && is.finite(p) && p > opts$alpha
        out <- g$groups
        if (g$posthoc && gated) out$letter <- "ns"
        list(groups = out, gated = gated)
      }
      tl <- lapply(terms, function(t) fn[sort(match(t, fn))])

      # Duncan tables ----------------------------------------------------------------
      info <- sprintf("alpha = %s, error df = %d, MSE = %s (full model).",
                      format(res$alpha), res$dfe, format(signif(res$mse, 5)))
      unb <- if (res$unbalanced) {
        if (res$adjusted) "Unbalanced design: least-squares means; Duncan-Kramer pairwise ranges."
        else "Unbalanced design: raw means; harmonic mean of group sizes (agricolae method)."
      }
      for (t in ph) {
        g  <- res$groups[[lab(t)]]
        lt <- lettersOf(g, termP[[lab(t)]])
        tb <- self$results$duncanTables$get(key = t)
        tb$getColumn("mean")$setTitle(if (res$adjusted) "Adjusted mean" else "Mean")
        gr <- lt$groups
        nf <- length(g$factors)
        for (i in seq_len(nrow(gr))) {
          vals <- list(n = gr$n[i], sd = gr$sd[i], mean = gr$mean[i], letter = gr$letter[i])
          for (j in seq_len(nf)) vals[[paste0("f", j)]] <- gr[[paste0("f", j)]][i]
          tb$addRow(rowKey = i, values = vals)
        }
        if (lt$gated)
          tb$setNote("gate", "The F-test for this term is not significant; group letters are suppressed (ns).")
        else
          tb$setNote("share", "Means sharing a letter are not significantly different (Duncan's test).")
        # a significant higher-order term containing this one makes the comparison questionable
        S <- g$factors
        hi <- termLabs[vapply(tl, function(u) length(u) > length(S) && all(S %in% u), TRUE)]
        hi <- hi[is.finite(termP[hi]) & termP[hi] <= opts$alpha]
        if (length(hi))
          tb$setNote("int", paste0("Significant interaction (", paste(hi, collapse = ", "),
                                   "): these marginal comparisons may be misleading."))
        if (!is.null(unb)) tb$setNote("unb", unb)
        tb$setNote("info", info)
      }

      # Report table -----------------------------------------------------------------
      if (opts$reportTable) {
        dg <- if (identical(opts$decimals, "auto")) "auto" else as.integer(opts$decimals)
        rt <- self$results$duncanReport
        i <- 0
        for (t in uniq(repTerms)) {
          g  <- res$groups[[lab(t)]]
          gr <- lettersOf(g, termP[[lab(t)]])$groups
          gr <- gr[order(gr$pos), , drop = FALSE]
          txt <- .duncanReport(gr$rawMean, gr$sd, gr$n, gr$letter, dg)
          for (r in seq_len(nrow(gr))) {
            i <- i + 1
            rt$addRow(rowKey = i, values = list(part = lab(t), level = gr$level[r], n = gr$n[r],
                                                value = txt[r]))
          }
        }
        rt$setNote("raw", "Raw (unadjusted) group means ± standard deviation.")
        if (length(ph))
          rt$setNote("share", "Within a term, means sharing a letter are not significantly different (Duncan's test).")
        if (length(ph) && res$unbalanced && res$adjusted)
          rt$setNote("unb", "Unbalanced design: letters are based on least-squares means, the table shows raw means.")
      }
    })
)
