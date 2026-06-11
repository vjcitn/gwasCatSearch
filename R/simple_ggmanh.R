
#' produce plotly-capable Manhattan plot using ggplot2
#' @import rlang
#' @param indf data.frame as produced by `variants_from_study`
#' @param pos.colname character(1) defaults to "CHR_POS"
#' @param chr.colname character(1) defaults to "CHR_ID"
#' @param pval.colname character(1) defaults to "P-VALUE"
#' @param label.colname character(1) column used for hover tooltip labeling
#' @param x.label character(1) x-axis label
#' @param y.label character(1) y-axis label
#' @param point.size numeric(1) point size passed to geom_point
#' @param signif numeric(1) genome-wide significance threshold, default 5e-8
#' @note Association records with missing p-values or positions are dropped before
#' plotting. The position column is coerced to numeric. No external Manhattan
#' plot package is required.
#' @examples
#' con = gwasCatSearch_dbconn()
#' toprecs = DBI::dbGetQuery(con, "select * from gwascatalog_associations limit 200")
#' toprecs$CHR_POS = as.numeric(toprecs$CHR_POS)
#' ww = simple_ggmanh(toprecs, y.label="-log10 p",
#'        label.colname = "MAPPED_TRAIT", pval.colname="P-VALUE")
#' plotly::ggplotly(ww, tooltip="text")
#' @export
simple_ggmanh <- function(
    indf, pos.colname = "CHR_POS", chr.colname = "CHR_ID",
    pval.colname = "P-VALUE", label.colname = NULL,
    x.label = "Chromosome", y.label = "-log10 p",
    point.size = 0.75, signif = 5e-8) {

  if (requireNamespace("shiny", quietly = TRUE))
    shiny::validate(shiny::need(nrow(indf) > 0, "no association data for selected studies"))

  # coerce and drop incomplete rows
  indf[[pos.colname]] <- as.numeric(indf[[pos.colname]])
  indf <- indf[!is.na(indf[[pos.colname]]) & !is.na(indf[[pval.colname]]), ]

  if (requireNamespace("shiny", quietly = TRUE))
    shiny::validate(shiny::need(nrow(indf) > 0, "no non-missing p-values, please select additional studies"))

  indf$mlogp <- -log10(indf[[pval.colname]])

  # order chromosomes canonically
  chr_order <- c(as.character(1:22), "X", "Y")
  indf[[chr.colname]] <- factor(indf[[chr.colname]], levels = chr_order)
  indf <- indf[!is.na(indf[[chr.colname]]), ]
  indf <- indf[order(indf[[chr.colname]], indf[[pos.colname]]), ]

  # cumulative x positions with a small gap between chromosomes
  chr_present <- levels(droplevels(indf[[chr.colname]]))
  chr_max <- tapply(indf[[pos.colname]], indf[[chr.colname]], max, na.rm = TRUE)
  gap <- max(chr_max, na.rm = TRUE) * 0.02
  offsets <- c(0, cumsum(as.numeric(chr_max[chr_present]) + gap))
  names(offsets) <- c(chr_present, "")
  offsets <- offsets[chr_present]

  indf$pos_cumul <- indf[[pos.colname]] +
    offsets[as.character(indf[[chr.colname]])]

  # x-axis tick at midpoint of each chromosome
  axis_pos <- tapply(indf$pos_cumul, indf[[chr.colname]], mean, na.rm = TRUE)

  # alternating blue shades
  chr_colors <- setNames(
    rep_len(c("#4393C3", "#92C5DE"), length(chr_present)),
    chr_present
  )

  # hover tooltip
  indf$newtext <- sprintf(
    "trait: %s<br>SNP: rs%s<br>gene: %s<br>chr %s:%s",
    indf$MAPPED_TRAIT, indf$SNP_ID_CURRENT, indf$MAPPED_GENE,
    as.character(indf[[chr.colname]]), indf[[pos.colname]]
  )

  ggplot2::ggplot(
    indf,
    ggplot2::aes(
      x     = .data$pos_cumul,
      y     = .data$mlogp,
      color = as.character(.data[[chr.colname]]),
      text  = .data$newtext
    )
  ) +
    ggplot2::geom_point(size = point.size, pch = 16) +
    ggplot2::scale_color_manual(values = chr_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      name   = x.label,
      breaks = axis_pos,
      labels = names(axis_pos),
      expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      name   = y.label,
      expand = c(0.02, 0.01)
    ) +
    ggplot2::geom_hline(
      yintercept = -log10(signif),
      linetype   = "dashed",
      color      = "red"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "none"
    )
}
