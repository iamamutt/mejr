#' Render an Rmarkdown document
#'
#' Wrapper for formatting options from rmarkdown::*_document() functions
#'
#' @param filename character input specifying markdown file name
#' @param format character name of format function from the \code{rmarkdown} package,
#'   e.g., \code{c('html_document', 'html_document')}
#' @param ... additional options passed to rmarkdown::render
#'
#' @return NULL
#' @export
#'
#' @examples
#' generate_doc('input.Rmd', 'pdf_document')
generate_doc <- function(filename, format = 'html_document', ...) {
    require_pkg("knitr")
    require_pkg("rmarkdown")

    # store default hook to restore later
    default_error_hook <- knitr::knit_hooks$get("error")

    # set custom options and hooks
    update_opts_rmd()

    # yaml data options
    yaml_header <- list(
        toc = TRUE,
        toc_depth = 4,
        df_print = 'default',
        fig_height = 4.5,
        fig_width = 5.25,
        dev = 'png',
        highlight = 'default'
    )

    # update options that are not listed, such as some knitr options
    r_chunk_opts <- list(
        knitr = list(
            opts_chunk = list(
                echo = TRUE,
                message = FALSE,
                warning = FALSE,
                include = TRUE,
                comment = "",
                dpi = 150,
                fig.align = "center",
                out.width = '50%',
                out.height = '50%',
                results = "markup", # asis, hold, hide
                strip.white = FALSE
            )
        )
    )

    # change format specific options from defaults
    if (format == 'html_document') {

        # html specific metadata options
        yaml_header <- modifyList(
            yaml_header,
            list(
                df_print = 'paged',
                toc_float = list(
                    collapsed = FALSE,
                    smooth_scroll = TRUE,
                    print = TRUE
                ),
                code_folding = 'none',
                code_download = TRUE,
                theme = 'journal',
                dev = 'svg',
                keep_md = FALSE
            )
        )
    } else if (format == 'pdf_document') {

        # pdf specific metadata options
        yaml_header <- modifyList(
            yaml_header,
            list(
                dev = 'pdf',
                keep_tex = FALSE)
        )

        # pdf specific knitr options
        r_chunk_opts <- modifyList(
            r_chunk_opts,
            list(
                knitr = list(
                    opts_chunk = list(
                        out.width = '4.2in',
                        out.height = '3.6in')
                )
            )
        )
    } else if (format == "md_document") {
        # markdown specific metadata options
        yaml_header <- modifyList(
            yaml_header,
            list(
                variant = "markdown",
                dev = 'png',
                highlight=NULL)
        )
        r_chunk_opts <- modifyList(
            r_chunk_opts,
            list(
                knitr = list(
                    opts_chunk = list(
                        out.width = '4.2in',
                        out.height = '3.6in')
                )
            )
        )
    }

    # get list of output format specific options
    opts <- do.call(eval(parse(text=paste0("rmarkdown::", format))), yaml_header)

    # update list with some knitr options
    opts <- modifyList(opts, r_chunk_opts)

    # use options list for rendering file
    rmarkdown::render(filename, opts, ...)

    # restore
    knitr::knit_hooks$set(error=default_error_hook)
    return(invisible())
}

#' Update R Markdown options
#'
#' @return list of old options
#' @export
#'
#' @examples
#' update_opts_rmd()
update_opts_rmd <- function() {
    require_pkg("knitr")

    # change basic R options
    mejr::update_opts(
        width = non_null(getOption('mejr.width'), 100),
        max.print = non_null(getOption('mejr.print'), 500),
        knitr.kable.NA = '',
        xtable.comment = FALSE)

    # custom default knitr options
    knitr::opts_chunk$set(
        math.matrix = NULL,
        pretty.data = NULL,
        pretty.data.opts = list(
            round=getOption('mejr.round'),
            code=FALSE)
    )

    # knitr chunk options sets
    knitr::opts_template$set(math.matrix = list(results = 'asis', echo = FALSE))

    # hooks for chunks
    knitr::opts_hooks$set(
        pretty.data = function(options) {
            if (!is.null(options$pretty.data)) {

                # pretty.data=TRUE in chunk
                if (is.logical(options$pretty.data) && options$pretty.data) {
                    options$pretty.data <- list()
                }

                # update chunk options sent to show_data
                options$pretty.data <- modifyList(
                    options$pretty.data.opts,
                    options$pretty.data
                )

                # output options
                options <- modifyList(
                    options,
                    list(
                        eval = FALSE, # will be evaluated in hook
                        echo = FALSE, # data should be a one-line chunk
                        results = 'asis', # no additional knitr formatting
                        # needs to be returned from hook
                        pretty.data = list(print = FALSE)
                    ))
            }
            return(options)
        },
        math.matrix = function(options) {
            if (!is.null(options$math.matrix)) {
                if (is.logical(options$math.matrix) && options$math.matrix) {
                    options$echo <- FALSE
                    options$results <- 'asis'
                }
                return(options)
            }
        }
    )

    # custom knitr hooks
    knitr::knit_hooks$set(
        # supress error messages
        error = function(x, options) {},
        # evaluate source code for pretty printing
        pretty.data = function(before, options, envir) {

            # text object is returned and printed after the chunk, which is never evaluated
            if (!before) {
                call <- options$pretty.data

                # switch functions depending on output option
                if (call$code) {
                    fn <- code_block_print
                } else {
                    fn <- show_data
                }

                # erase unused options passed to fn
                call$code <- NULL

                # TODO: check multiline code statements
                call$x <- eval(parse(text=options$code), envir)

                # call printing function
                return(do.call(fn, call))
            }
        }
    )
}


#' Pretty print a data object or atomic vector
#'
#' @param x data object, such as data.frame, data.table, numeric or character vector
#' @param nrows number of rows to print
#' @param caption table caption text
#' @param print print immediately or return string
#' @param ... other args passed to pander::pandoc.table
#'
#' @return formatted data text
#' @export
#'
#' @examples
#' x <- array(rnorm(100), c(25,5))
#' colnames(x) <- c("(Intercept)", paste0("V", 1:4))
#' show_data(x)
#' show_data(as.data.frame(x))
show_data <- function(x,
                      nrows = NULL,
                      caption = NULL,
                      print = TRUE,
                      ...) {
    require_pkg("pander")
    # update default args if not specified in ...
    pan_args <- mejr::append_args(
        list(...),
        list(
            round = non_null(getOption('mejr.round'), 2),
            style = 'multiline',
            emphasize.rownames = FALSE,
            emphasize.colnames = FALSE,
            missing = '',
            split.cells = 20,
            split.tables = 110
        ))

    digits <- non_null(getOption('mejr.digits'), 6)
    pan_args$digits <- as.integer(digits + pan_args$round)
    old <- update_pander_opts(
        list(
            table.alignment.default = 'right',
            table.alignment.rownames = 'left',
            table.continues = "\n\n$$\\ldots$$",
            table.caption.prefix = ""
        )
    )

    if (is.matrix(x)) {
        colnames(x) <- rm_colon(colnames(x))
        rownames(x) <- rm_colon(rownames(x))

    } else if (is.vector(x)) {
        x <- format_vector(x, nrows, round = pan_args$round)
        nrows <- nrow(x)
        pan_args <- mejr::append_args(pan_args, list(justify = 'left'))

        old <- modifyList(
            old,
            update_pander_opts(
                list(
                    table.continues = "\n\n<br />\n",
                    table.caption.prefix = ""
                )))
    }

    # cut table rows
    x <- head(x, non_null(nrows, 50))
    rnames <- rownames(x)
    cnames <- colnames(x)
    pan_args$t <- x

    # emphasize rownames if present
    if (!is.null(rnames)) {
        pan_args <- mejr::append_args(
            pan_args,
            list(row.names = pander::pandoc.emphasis.return(rnames)))
    }

    # emphasize colnames if present
    if (!is.null(cnames)) {
        pan_args <- mejr::append_args(
            pan_args,
            list(col.names = pander::pandoc.strong.return(cnames)))
    }

    pander::pandoc.strong.return

    if (is.null(caption)) {
        caption <- '\n<br />\n\n'
    } else {
        caption <- paste0('Table: ', caption, ' \n\n\n<br />\n\n')
    }


    # print immediately or return text object
    if (print) {
        printed <- NULL
        do.call(pander::pandoc.table, pan_args)
        cat(caption)
    } else {
        printed <- do.call(pander::pandoc.table.return, pan_args)
        printed <- paste0(printed, caption)
    }

    # return pander::panderOptions to previous state
    update_pander_opts(old)
    return(invisible(printed))
}


#' Print a LaTeX math matrix from an R matrix object
#'
#' @param sym escaped LaTeX math expression to print before the matrix
#' @param x matrix object
#' @param rows n rows to print (defaults to all)
#' @param cols n cols to print (defaults to all)
#' @param round n significant digits to print (defaults to 2)
#' @param print immediately print or return text object
#'
#' @return character
#' @export
#'
#' @examples
#' x <- matrix(rnorm(25), 5)
#' math_matrix('\\bar{x}=', x, 4, 4, 2)
math_matrix <- function(sym = '',
                        x,
                        rows = NULL,
                        cols = NULL,
                        round = getOption('mejr.round'),
                        print = TRUE) {
    require_pkg("xtable")
    round <- non_null(round, 2)

    if (!is.matrix(x)) x <- as.matrix(x)

    x <- na_slices(x, rows, 1)
    x <- na_slices(x, cols, 2)

    nr <- nrow(x)
    nc <- ncol(x)
    na_vals <- is.na(x)

    x[!na_vals] <- sprintf(print_fmt(round), x[!na_vals])

    all_na_c <- apply(na_vals, 2, all)
    all_na_r <- apply(na_vals, 1, all)

    x[, all_na_c] <- '\\ldots'
    x[all_na_r,] <- '\\vdots'

    for (j in 1:ncol(x)) {
        for (i in 1:nrow(x)) {
            if (na_vals[i, j]) {
                is_na_row <- all_na_r[i]
                is_na_col <- all_na_c[j]
                is_na_row_col <- is_na_row & is_na_col
                is_diag <- i == j
                is_end <- i == nr & j == nc
                make_ddots <- FALSE
                if (is_end) {
                    if (is_na_row_col) {
                        make_ddots <- TRUE
                    }
                } else {
                    if (is_diag | is_na_row_col) {
                        make_ddots <- TRUE
                    }
                }
                if (make_ddots) {
                    x[i, j] <- '\\ddots'
                }
            }
        }
    }

    options(xtable.comment = FALSE)
    xtab <- xtable::xtable(x, align = rep("r", ncol(x) + 1))
    xtab_txt <- print(xtab,
                      floating = FALSE,
                      tabular.environment = "array",
                      hline.after = NULL,
                      include.rownames = FALSE,
                      include.colnames = FALSE,
                      print.results = FALSE,
                      sanitize.text.function = identity
    )

    math_mode_txt <- sprintf('\n\n$$\n%s\\left[\n%s\\right]\n$$\n\n', sym, xtab_txt)

    if (print) {
        cat(math_mode_txt)
        return(invisible())
    } else {
        return(math_mode_txt)
    }
}



# pretty print numeric vector as markdown code block
code_block_print <- function(x,
                             round=getOption('mejr.round'),
                             width=getOption('mejr.width'),
                             print=TRUE) {

    round <- non_null(round, 2)
    width <- non_null(width, 88)

    if (!is.atomic(x)) {
        stop('x must be an atomic object')
    }

    if (is.numeric(x)) {
        x <- sprintf(print_fmt(round, format='f'), x)
    }

    splitted <- Reduce(function(current_val, next_val) {
        concat <- c(current_val, next_val)
        new_lines <- concat == '\n'
        vec_length <- length(concat)

        if (any(new_lines)) {
            concat <- concat[min(vec_length, max(which(new_lines)) + 1):vec_length]
            vec_length <- length(concat)
        }

        fits_on_line <- cumsum(nchar(concat)) <= width

        if (all(fits_on_line)) {
            c(current_val, ', ', next_val)
        } else {
            c(current_val, '\n', next_val)
        }
    },
    x = x, accumulate = FALSE)

    print_fmt <- paste(c('\n\n```\n', splitted, '\n```\n\n'), collapse='')

    if (print) {
        cat(print_fmt)
        return(invisible())
    } else {
        return(print_fmt)
    }
}

#' Pretty formatting for a numeric vector
#'
#' @param x vector of integers or floating point values
#' @param nrows split vector into this many rows instead of a single row vector
#' @param round round to n significant digits
#'
#' @return a formatted numeric vector as string
#' @export
#'
#' @examples
#' format_vector(rnorm(10), 2, 3)
format_vector <- function(x, nrows=NULL, round=getOption('mejr.round')) {

    round <- non_null(round, 3)
    size <- length(x)

    if (!is.vector(x) | size < 1)
        return(x)

    if (is.null(nrows)) {
        nrows <- 1
    } else if (nrows < 0) {
        nrows <- size
    }

    vnames <- names(x)
    cols <- ceiling(size / nrows)

    if (cols < 2)
        nrows <- size

    if (is.numeric(x))
        x <- sprintf(print_fmt(round), x)

    x <- sapply(x, function(v) paste0('`', v, '`'))
    y <- array(NA, c(nrows, cols))
    y[1:size] <- x

    if (!is.null(vnames)) {
        vnames <- rm_colon(vnames)
        vnames <- sapply(vnames, function(i) paste0('*', i, '*'))
        z <- array(NA, c(nrows, cols))
        z[1:size] <- vnames
        y <- interleave(z, y)
    }

    return(y)
}


#' Adds rows or columns of NAs to a matrix
#'
#' @param x matrix object
#' @param head number of rows or columns to keep, or index of which to keep
#' @param d dimension. 1 = rows, 2 = columns
#'
#' @return matrix
#' @export
#'
#' @examples
#' x <- diag(5)
#' na_slices(x, 4)
#' na_slices(x, 4, 2)
na_slices <- function(x, head=NULL, d=1) {
    if (is.null(head)) {
        return(as.matrix(x))
    }

    if (length(head) == 1) {
        head <- 1:head
    }

    n <- dim(x)[d]
    head <- head[head <= n]
    max_i <- max(head)
    slice_r <- ifelse(d==1, TRUE, FALSE)
    vec <- rep(NA, n)
    vec[head] <- head

    if (max_i < n) {
        max_i <- max_i + 1
    }

    vec <- vec[1:max_i]
    which_nas <- is.na(vec)
    rep_na <- rle(which_nas)
    single_nas <- unlist(sapply(seq_along(rep_na$values), function(i) {
        y <- rep(FALSE, rep_na$lengths[i])
        if (rep_na$values[i]) {
            y[1] <- TRUE
        }
        return(y)
    }))

    vec <- sort(c(which(!which_nas), which(single_nas)))

    if (slice_r) {
        x[single_nas, ] <- NA
        x <- x[vec, ]
    } else {
        x[, single_nas] <- NA
        x <- x[, vec]
    }
}


# helper functions ----------------------------------------------------------------


#' check if all values are NULL or possibly all NAs, return a single value if so.
non_null <- function(x, val, check_na = FALSE) {
    x[is.null(x)] <- val
    if (check_na) {
        x[is.na(x)] <- val
    }
    return(x)
}

#' sprintf format string
print_fmt <- function(round = getOption('mejr.round'),
                      digits = getOption('mejr.digits'),
                      format = 'g',
                      leading = TRUE) {
    paste0('%',
           ifelse(leading, ' ', ''),
           as.integer(non_null(digits, 6)),
           '.',
           as.integer(non_null(round, 2)),
           format)
}

#' interleave vectors or matrices by alternating sequential values
interleave <- function(..., dim=2) {
    x <- list(...)
    if (dim == 1) {
        y <- do.call(rbind, x)
        return(y[order(sequence(sapply(x, nrow))), ])
    } else {
        y <- do.call(cbind, x)
        return(y[, order(sequence(sapply(x, ncol)))])
    }
}

#' remove a colon from a string
rm_colon <- function(x, s=' ') {
    stringr::str_replace_all(x, ':', s)
}

#' update options from pander package from named list
update_pander_opts <- function(opts=NULL) {
    if (is.null(opts))
        return(invisible())

    opt_names <- names(opts)

    old <- lapply(names(opts), function(o) {
        pander::panderOptions(o)
    })
    names(old) <- opt_names

    # set
    sapply(seq_along(opt_names), function(o) {
        key <- opt_names[o]
        val <- opts[[o]]
        if (!(is.na(val) | is.null(val)))
            pander::panderOptions(key, val)
        return(invisible())
    })

    return(old)
}


#' print lme4 model
#'
#' @param merMod lmer or glmer object
#' @param top_level Number of #'s, defaults to 3
#' @param digits significant digits to print
#' @param aov  print ANOVA, logical
#'
#' @return NULL
#' @export
#'
#' @examples
#' knit_lme(lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy))
knit_lme <- function(merMod,
                     top_level = 4,
                     round = getOption('mejr.round'),
                     aov = TRUE,
                     title = NULL,
                     df = "Satterthwaite") {
    require_pkg("lme4")
    if (class(merMod) == "merModLmerTest") {
        modsum <- lmerTest::summary(merMod, ddf = df)
    } else {
        modsum <- summary(merMod)
    }

    header <- function(x, adj = 0) {
        paste0(
            "\n\n",
            paste0(rep("#", top_level + adj), collapse = ""),
            " ",
            paste0(x, collapse = ""),
            "\n\n",
            collapse = "")
    }

    if (modsum %?n% "methTitle" && is.null(title)) {
        cat(header(sub("\\n", "\n\n", modsum$methTitle)))
        if (is.null(modsum$family)) {
            cat("\n- Family: Gaussian\n- Link: Identity")
        } else {
            cat(sprintf("\n\n- Family: %s\n- Link: %s", modsum$family, modsum$link))
        }
    } else {
        cat(header(title))
    }

    if (modsum %?n% "call") {
        cat(sprintf(
            "\n- Data: `%s`\n- Formula: `%s`",
            Reduce(paste0, deparse(modsum$call$data)),
            Reduce(paste0, deparse(modsum$call$formula))
        ),
        sep = "")
    }

    if (modsum %?n% "ngrps") {
        cat("\n- Group N:")
        ngrps <- modsum$ngrps
        cat(paste0("\n\t- ", names(ngrps), ": ", ngrps), sep = "")
    }

    if (modsum %?n% "residuals") {
        cat("\n- Total N:", length(modsum$residuals))
    }

    cat('\n\n')

    if (modsum %?n% "AICtab") {
        cat(header("Information Criteria", 1))
        show_data(as.matrix(modsum$AICtab), round=round, style='simple')
    }

    if (modsum %?n% "sigma") {
        cat(header("Residual error", 1))
        show_data(c(`$\\sigma$ residuals` = modsum$sigma), round=round+1)
    }

    if (modsum %?n% "varcor") {
        vcov <- varcov(merMod, cov = FALSE)
        lnames <- names(vcov)
        for (i in 1:length(lnames)) {
            cat(header(c("Combined SD & Correlation Matrix: ", lnames[i]), 1))
            vcov[[i]][upper.tri(vcov[[i]])] <- NA
            if (nrow(vcov[[i]]) == 1) vcov[[i]] <- cbind(vcov[[i]], `  ` = NA)
            show_data(vcov[[i]], round=round, style='simple')
        }

        if (class(merMod) == "merModLmerTest") {
            cat(header("Analysis of Random Effects Table:", 1))
            ran_tab <- rand(merMod)$rand.table
            ran_tab <- data.table::data.table(` ` = row.names(ran_tab), ran_tab)
            show_data(ran_tab, round=round)
        }

    }

    if (modsum %?n% "coefficients") {
        cf_tab <- modsum$coefficients
        cf_tab <- cbind(Effect = rm_colon(rownames(cf_tab)), data.table::as.data.table(cf_tab))
        if (class(merMod) == "merModLmerTest") {
            pvals <- cf_tab[, unlist(lapply(names(cf_tab), function(i) {
                grepl("Pr", i)
            })), with = F]
            cf_tab <- cbind(cf_tab, ` ` = pval_format(pvals[[1]])[, 2])
        }
        cat(header("Fixed Regression Coefficients", 1))
        show_data(cf_tab, round=round, row.names=FALSE, style='simple')
    }

    if (aov) {
        if (class(merMod) == "merModLmerTest") {
            aov_tab <- anova(merMod, ddf = df)
        } else {
            aov_tab <- anova(merMod)
        }
        aov_cap <- sub("\\n", "", attr(aov_tab, "heading"))
        aov_fac <- rm_colon(rownames(aov_tab))
        aov_tab <-
            cbind(Factor = aov_fac, data.table::as.data.table(aov_tab))
        if (class(merMod) == "merModLmerTest") {
            pvals <-
                aov_tab[, unlist(lapply(names(aov_tab), function(i)
                    grepl("Pr", i))), with = F]
            aov_tab <- cbind(aov_tab, ` ` = pval_format(pvals[[1]])[, 2])
        }
        if (aov_tab %?n% "F.value")
            data.table::setnames(aov_tab, "F.value", "F value")
        cat(header(aov_cap, 1))
        show_data(aov_tab, round=round, row.names=FALSE, style='simple')
    }

    cat("\n\n")
    return(invisible(NULL))
}
