
#' produce plotly-capable manhattanplot based on ggmanh
#' @import ggmanh
#' @import rlang
#' @param indf data.frame as produced by `variants_from_study`
#' @param pos.colname character(1) defaults to "CHR_POS"
#' @param chr.colname character(1) defaults to "CHR_ID"
#' @param chromosome character(1) or NULL, should be removed for gwasCatSearch
#' @param rescale logical(1)
#' @param rescale.ratio.threshold numeric(1)
#' @param signif.rel.pos numeric(1)
#' @param color.by.highlight logical(1)
#' @param pval.colname character(1)
#' @param label.colname character(1) obligatory for phenotype labeling
#' @param x.label character(1)
#' @param y.label character(1)
#' @param point.size numeric(1)
#' @param label.font.size numeric(1)
#' @param max.overlaps numeric(1)
#' @param plot.title like ggplot2::waiver() or character(1)
#' @param plot.subtitle like plot.title
#' @param plot.width numeric(1)
#' @param plot.height numeric(1)
#' @note Association records with missing p-values are eliminated before
#' plotting.  Note also that the position column in `indf` is coerced to numeric mode.
#' @examples
#' con = gwasCatSearch_dbconn()
#' toprecs = DBI::dbGetQuery(con, "select * from gwascatalog_associations limit 20")
#' toprecs$CHR_POS = as.numeric(toprecs$CHR_POS)
#' ww = simple_ggmanh(toprecs, y.label="-log10 p", 
#'        label.colname = "MAPPED_TRAIT", pval.colname="P-VALUE")
#' plotly::ggplotly(ww, tooltip="text")
#' @export
simple_ggmanh <- function(
  indf, pos.colname="CHR_POS", chr.colname = "CHR_ID", chromosome = NULL, 
  rescale = TRUE, 
  rescale.ratio.threshold = 5, signif.rel.pos = 0.4, color.by.highlight = FALSE,
  label.colname = NULL, pval.colname="P-VALUE", x.label = "Chromosome", y.label = "-log10 p",
  point.size = 0.75, label.font.size = 2, max.overlaps = 20,
  plot.title = ggplot2::waiver(), plot.subtitle = ggplot2::waiver(),
  plot.width = 10, plot.height = 5) {
#
# this code is extracted from ggmanh:::manhattan_plot.MPdata, to support
# use of ggplotly with the output.  it excludes the ggrepel capability
# and avoids issue with y.label as expression
#
  indf[[pos.colname]] = as.numeric(indf[[pos.colname]])
  x = ggmanh::manhattan_data_preprocess(indf, chr.colname=chr.colname,
    pos.colname=pos.colname, pval.colname=pval.colname, chr.order=c(1:22, "X", "Y"))
  stopifnot(is(label.colname, "character"), length(label.colname)==1L,
    label.colname %in% names(x$data))
  if (all(!is.null(label.colname), !is.na(label.colname), na.rm = TRUE)) {
    if (!(label.colname %in% colnames(x$data))) stop("label.colname not a valid column name for the data.")
#    if ((sum(!is.na(x$data[[label.colname]]) & !(x$data[[label.colname]] == ""))) > 20) warning("The plot will generate > 20 labels. The plot may be cluttered & may take a longer to generate.")
  }

  # if chromosome is specified, subset the data
  if (!is.null(chromosome)) {
    valid_chr(x$data, chromosome, x$chr.colname)
    x$data <- x$data[x$data[[x$chr.colname]] == chromosome,]
  }

  # decide if the resulting plot will be single chromosome, or multiple
  single.chr <- length(unique(x$data[[x$chr.colname]])) == 1

  # create transformation object; if rescaling is required, create appropriate transformation
  trans <- list("trans" = "identity", "breaks" = ggplot2::waiver())
  mustdrop = which(is.na(x$data[[x$pval.colname]]))
  if (length(mustdrop)>0)
      x$data = x$data[-mustdrop,]
  if (requireNamespace("shiny")) {
  shiny::validate(shiny::need(nrow(x$data)>0, "no non-missing p-values, please select additional studies"))
  }
  if (rescale) {
    jump <- ggmanh:::get_transform_jump(-log10(x$signif))
    if ((ceiling(max(x$data[[x$pval.colname]],na.rm=TRUE)/5)*5)/jump > rescale.ratio.threshold) {
      trans <- ggmanh:::get_transform(x$data, jump, x$pval.colname, jump.rel.pos = signif.rel.pos)
    }
  }

  ylimit <- c(0, ifelse(identical(trans$trans, "identity"), NA, max(trans$breaks)))

  # plot parameters depend on single.chr
  if (single.chr) {
    pos <- x$true.pos.colname
    x.break <- ggplot2::waiver()
    x.break.label <- ggplot2::waiver()
    x.limits <- NULL
  } else {
    pos <- x$pos.colname
    x.break <- x$center_pos
    x.break.label <- x$chr.labels
    x.limits <- c(min(x$start_pos), max(x$end_pos))
  }

  # choose whether to use highlight.colname or chr.colname
  if (!is.null(x$highlight.colname) && !is.null(x$highlight.col) && color.by.highlight) {
    point.color <- x$highlight.colname
    point.color.map <- x$highlight.col
  } else {
    point.color <- x$chr.colname
    point.color.map <- x$chr.col
  }

  # set up hovertext
  x$data$newtext = sprintf("trait: %s<br>SNP: rs%s<br>gene: %s<br>chr %s:%s",
          x$data$MAPPED_TRAIT, x$data$SNP_ID_CURRENT,
            x$data$MAPPED_GENE, x$data$CHR_ID, x$data$CHR_POS)

  # manhattan plot
  p <- ggplot2::ggplot(x$data, ggplot2::aes(x = !!rlang::sym(pos), y = !!rlang::sym(x$pval.colname), 
      color = !!rlang::sym(point.color), 
      text=newtext)) + # !!rlang::sym(label.colname))) +  # label.colname is obligatory
    ggplot2::geom_point(size = point.size, pch = 16) +
    ggplot2::scale_discrete_manual(aesthetics = "color", values = point.color.map) +
    ggplot2::scale_y_continuous(
      trans = trans$trans,
      breaks = trans$breaks,
      expand = c(0.02, 0.01),
      limits = ylimit
    ) +
    ggplot2::scale_x_continuous(
      name = x.label,
      breaks = x.break,
      labels = x.break.label,
      expand = c(0.01, 0.01), limits = x.limits
    )  +
    ggplot2::geom_hline(
      yintercept = -log10(x$signif),
      linetype = 'dashed',
      color = x$signif.col
    ) 
  if (rescale & !identical(trans$trans, "identity")) {
    # if the plot is rescaled, change the tick at the "jump" to double line
    jump.tick.size <- 3.5

    p <- p +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_line(linetype = trans$y_axis_linetype)) +
      ggplot2::annotate(geom = "point", shape = "=", x = -Inf, y = trans$jump, size = jump.tick.size) +
      ggplot2::coord_cartesian(clip = "off")
  }


p
}   # maybe some of the following will be used in revisions
# +
#    ggplot2::theme_bw() +
#    ggplot2::theme(
#      panel.grid.major = ggplot2::element_blank(),
#      panel.grid.minor = ggplot2::element_blank(),
#      legend.position = "none"
#    ) +
#    ggplot2::ylab(y.label) +
#    ggplot2::ggtitle(label = plot.title, subtitle = plot.subtitle)
##
##  if (all(!is.null(label.colname), !is.na(label.colname), na.rm = TRUE)) {
##    p <- p + ggrepel::geom_label_repel(
##      ggplot2::aes(x = !!rlang::sym(pos), y = !!rlang::sym(x$pval.colname), label = !!rlang::sym(label.colname)),
##      size = label.font.size,
##      label.padding = 0.15,
##      segment.size = 0.2,
##      min.segment.length = 0,
##      max.overlaps = max.overlaps,
##      ...
##    )
##  }
##
##  if (rescale & !identical(trans$trans, "identity")) {
##    # if the plot is rescaled, change the tick at the "jump" to double line
##    jump.tick.size <- 3.5
##
##    p <- p +
##      ggplot2::theme(
##        axis.ticks.y = ggplot2::element_line(linetype = trans$y_axis_linetype)) +
##      ggplot2::annotate(geom = "point", shape = "=", x = -Inf, y = trans$jump, size = jump.tick.size) +
##      ggplot2::coord_cartesian(clip = "off")
##  }
##
##  if (!is.null(outfn)) {
##    ggplot2::ggsave(outfn, plot=p, width=plot.width, height=plot.height, units = "in")
##    invisible()
##  } else {
##    return(p)
##  }
