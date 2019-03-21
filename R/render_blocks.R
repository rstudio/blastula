render_blocks <- function(blocks,
                          context = "body") {

  for (i in seq_along(blocks)) {

    if (inherits(blocks[[i]], "block_title")) {
      blocks[[i]] <- render_block_title(blocks[[i]], context = context)
    }

    if (inherits(blocks[[i]], "block_text")) {
      blocks[[i]] <- render_block_text(blocks[[i]], context = context)
    }

    if (inherits(blocks[[i]], "block_spacer")) {
      blocks[[i]] <- render_block_spacer(blocks[[i]], context = context)
    }

    if (inherits(blocks[[i]], "block_articles")) {
      blocks[[i]] <- render_block_articles(blocks[[i]])
    }

    if (inherits(blocks[[i]], "block_social_links")) {
      blocks[[i]] <- render_block_social_links(blocks[[i]])
    }
  }

  blocks
}
