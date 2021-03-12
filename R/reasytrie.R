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
  new("trie",
      root = new(
        "trie.node",
        is_complete_word = FALSE,
        children = rlang::new_environment())
  )
}

.trie_delete <- function(cur, word_to_delete) {
  # The next character to process
  next_char <- substr(word_to_delete, 1, 1)
  # Rest of the strings excluding the next character
  rest_string <- substr(word_to_delete, 2, nchar(word_to_delete))

  if (!exists(next_char, envir = cur@children)) {
    # The next character is not in the Trie, deletion failed.
    return(FALSE)
  }

  if (nchar(word_to_delete) == 1) {
    # Reached the target node correspond to the last character
    if (!cur@children[[word_to_delete]]@is_complete_word) {
      # The path to this node does not form a complete word, deletion failed.
      return(FALSE)
    }

    cur@children[[word_to_delete]]@is_complete_word <- FALSE
    if (length(cur@children[[word_to_delete]]@children) == 0) {
      # This node does not have any child, so we want to remove it from
      #   our Trie entirely.
      rm(list = word_to_delete, envir = cur@children)
    }

    return(TRUE)
  }

  result <- .trie_delete(cur@children[[next_char]], rest_string)

  if (length(cur@children[[next_char]]@children) == 0 &&
      (!(cur@children[[next_char]]@is_complete_word))) {
    # The node is not a complete word while not having any child, remove it.
    rm(list = next_char, envir = cur@children)
  }

  result
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

  if (!is.character(word_to_delete) || nchar(word_to_delete) < 1) {
    stop("word_to_delete must be a non-empty string")
  }

  .trie_delete(trie@root, word_to_delete)
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
  if (!class(trie) == "trie") {
    stop("Input trie must be an instance of the trie class")
  }

  if (!is.character(word) || !grepl("^[A-Za-z]+$", word)) {
    stop("Input word must be a valid string contains letters only")
  }

  char_list <- as.list((strsplit(tolower(word), "")[[1]]))
  cur <- trie@root

  for (char in char_list) {
    if (!exists(char, envir = cur@children)) {
      return(FALSE)
    }

    cur <- cur@children[[char]]
  }

  if (!cur@is_complete_word) {
    return(FALSE)
  } else {return(TRUE)}
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

  if (!class(trie) == "trie") {
    stop("Input trie must be an instance of the trie class")
  }

  if (!is.character(word_to_add) || !grepl("^[A-Za-z]+$", word_to_add)) {
    stop("Input word must be a valid string contains letters only")
  }

  char_list <-  as.list(strsplit(word_to_add, "")[[1]])
  cur <- trie@root

  for (i in seq_along(char_list)) {
    char <- char_list[[i]]


    if (exists(char, envir = cur@children)) {
      if (i == length(char_list)) {
        cur@children[[char]] <- TRUE
      }
      cur <- cur@children[[char]]
    }

    else {

      if (i == length(char_list)) {
        is_complete_word <- TRUE
      } else {is_complete_word <- FALSE}

      cur@children[[char]] <- new("trie.node", is_complete_word = is_complete_word, children = rlang::new_environment())
      cur <- cur@children[[char]]
    }

  }

  if (!cur@is_complete_word) {
    return(FALSE)
  } else {
    return(TRUE)
  }
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
