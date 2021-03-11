#' This is a conceptual class representation of a single node in a trie.
#'
#' @slot is_complete_word A logical marks if the path to this node forms a complete word.
#' @slot children An environment (hash map) that maps from the next alphabet to the corresponding sub-tree.
#'
#' @name trie.node
#' @rdname trie.node
#' @export
setClass("trie.node",
         slots = list(is_complete_word = "logical", children = "environment"))

#' This is a conceptual class representation of a Trie.
#'
#' @slot root A trie node that is the root of the trie.
#'
#' @name trie
#' @rdname trie
#' @export
setClass("trie", slots = list(root = "trie.node"))

#' Create an empty trie.
#'
#' @return A trie.
#' @export
#'
#' @examples
#' trie <- trie_create()
trie_create <- function() {
  new(
    "trie",
    root = new(
      "trie.node",
      is_complete_word = FALSE,
      children = rlang::new_environment()
    )
  )
}

.trie_delete <- function(cur, word_to_delete) {
  # print(word_to_delete)
  if (word_to_delete == "") {
    if (cur@is_complete_word) {
      cur@is_complete_word <- FALSE
      if (length(cur@children) == 0) {
        return(list(delete_success = TRUE, remove_link = TRUE))
      } else {
        return(list(delete_success = TRUE, remove_link = FALSE))
      }
    } else {
      return(list(delete_success = FALSE, remove_link = FALSE))
    }
  }

  if (!exists(substr(word_to_delete, 1, 1), envir = cur@children)) {
    return(list(delete_success = FALSE, remove_link = FALSE))
  }

  result <-
    .trie_delete(cur@children[[substr(word_to_delete, 1, 1)]],
                 substr(word_to_delete, 2, nchar(word_to_delete)))

  if (!result$delete_success) {
    return(list(delete_success = FALSE, remove_link = FALSE))
  }

  if (result$remove_link) {
    rm(list = substr(word_to_delete, 1, 1),
       envir = cur@children)
  }

  if (length(cur@children) == 0 && (!cur@is_complete_word)) {
    return(list(delete_success = TRUE, remove_link = TRUE))
  }

  list(delete_success = TRUE, remove_link = FALSE)
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
  if (!class(trie) == "trie") {
    stop("trie must be an instance of the trie class")
  }

  if (!is.character(word_to_delete) || length(word_to_delete) < 1) {
    stop("word_to_delete must be a non-empty string")
  }

  result = .trie_delete(trie@root, word_to_delete)

  result$delete_success
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


#' Adds a single word to the trie.
#'
#' @param trie A trie.
#' @param word_to_add The word to be added in the trie.
#'
#' @return a logical indicating that if the addition is successful or not.
#'   Returns TRUE if word is added successfully
#'   Returns FALSE if the word  already exists in the trie.
#' @export
#'
#' @examples
#' trie <- trie_create()
#' trie_add(trie, "test")
trie_add <- function(trie, word_to_add) {
  stop("The function is not yet implemented")
}


#' Finds all words that match the prefix in the trie.
#'
#' @param trie A trie.
#' @param prefix The prefix of the words to search for.
#'
#' @return List of words matching the prefix
#' @export
#'
#' @examples
#' trie <- trie_create()
#' trie_find_prefix(trie, "he")
trie_find_prefix <- function(trie, prefix) {
  stop("The function is not yet implemented")
}
