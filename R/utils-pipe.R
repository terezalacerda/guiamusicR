#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


utils::globalVariables(c("album_album_type", "album_artists", "artists",
                         "artists_id", "external_urls.spotify", "followers.href",
                         "followers.total", "href", "id", "images", "name",
                         "popularity", "type", "uri"))
