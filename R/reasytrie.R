#' This is a conceptual class representation of a single node in a trie.
#'
#' @slot is_complete_word A logical marks if the path to this node forms a complete word.
#' @slot children An environment (hash map) that maps from the next alphabet to the corresponding sub-tree.
#'
#' @name trie.node
#' @rdname trie.node
#' @export
setClass("trie.node", slots=list(is_complete_word="logical", children="environment"))

#' This is a conceptual class representation of a Trie.
#'
#' @slot root A trie node that is the root of the trie.
#'
#' @name trie
#' @rdname trie
#' @export
setClass("trie", slots=list(root="trie.node"))

#' Create an empty trie.
#'
#' @return A trie.
#' @export
#'
#' @examples
#' trie <- trie_create()
trie_create <- function() {
  new("trie", root = new("trie.node", is_complete_word=FALSE, children=new.env()))
}

#' Deletes a single word from the trie.
#'
#' @param trie A trie.
#' @param word_to_delete The word to be deleted from the trie.
#'
#' @return a logical indicating that if the deletion is successful or not.
#'   Returns FALSE if the word to delete is not in the trie.
#' @export
#'
#' @examples
#' trie <- trie_create()
#' trie_delete(trie, "test")
trie_delete <- function(trie, word_to_delete) {
  stop("The function is not yet implemented")
}

#' Search if a word is present in the trie.
#'
#' @param trie A trie.
#' @param word The word to be searched in the trie.
#'
#' @return a logical indicating that if the word is present or not.
#'   Returns TRUE if the word is in the trie.
#'   Returns FALSE if the word is not in the trie.
#' @export
#'
#' @examples
#' trie <- trie_create()
#' trie_contain(trie, "test")
trie_contain <- function(trie, word) {
  stop("The function is not yet implemented")
}