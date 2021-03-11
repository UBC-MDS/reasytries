test_that("trie_delete: not delete anything from an empty trie", {
  trie <- tr(list())
  #   root

  is_deleted <- trie_delete(trie, "ad")
  # Deletion should fail

  expect_false(is_deleted)
  expect_length(trie@root@children, 0)
})

test_that("trie_delete: correctly delete existing word", {
  trie <- tr(list(
    a = tn(FALSE, list(
      d = tn(TRUE, list())
    ))))
  #   root
  #    |
  #    a
  #   /
  #  d

  is_deleted <- trie_delete(trie, "ad")
  # After removal, should be:
  #   root

  expect_true(is_deleted)
  expect_length(trie@root@children, 0)
})

test_that("trie_delete: not delete non-existing word", {
  trie <- tr(list(
    a = tn(FALSE, list(
      d = tn(TRUE, list())
    ))))
  #   root
  #    |
  #    a
  #   /
  #  d

  is_deleted <- trie_delete(trie, "a")
  # Deletion should fail, and nothing should be changed

  expect_false(is_deleted)
  expect_true(exists("a", envir = trie@root@children))
  expect_true(exists("d", envir = trie@root@children$a@children))
  expect_true(trie@root@children$a@children$d@is_complete_word)
})

test_that("trie_delete: not delete non-existing word with existing prefix", {
  trie <- tr(list(
    a = tn(FALSE, list(
      d = tn(TRUE, list())
    ))))
  #   root
  #    |
  #    a
  #   /
  #  d

  is_deleted <- trie_delete(trie, "ax")
  # Deletion should fail, and nothing should be changed

  expect_false(is_deleted)
  expect_true(exists("a", envir = trie@root@children))
  expect_true(exists("d", envir = trie@root@children$a@children))
  expect_true(trie@root@children$a@children$d@is_complete_word)
  expect_false(exists("x", envir = trie@root@children$a@children))
})

test_that("trie_delete: correctly delete sub-tree", {
  trie <- tr(list(
    a = tn(FALSE, list(
      d = tn(TRUE, list()),
      c = tn(TRUE, list())
    ))))
  #   root
  #    |
  #    a
  #   / \
  #  d   c

  is_deleted <- trie_delete(trie, "ac")
  # After removal, should be
  #   root
  #    |
  #    a
  #   /
  #  d

  expect_true(is_deleted)
  expect_true(exists("a", envir = trie@root@children))
  expect_true(exists("d", envir = trie@root@children$a@children))
  expect_true(trie@root@children$a@children$d@is_complete_word)
  expect_false(exists("c", envir = trie@root@children$a@children))
})

test_that("trie_delete: correctly delete nested word", {
  trie <- tr(list(
    b = tn(FALSE, list(
      e = tn(TRUE, list(
        d = tn(TRUE, list())
      ))))))
  #   root
  #    |
  #    b
  #    \
  #     e (complete word)
  #      \
  #       d (complete word)

  # browser()
  is_deleted <- trie_delete(trie, "be")
  #   root
  #    |
  #    b
  #    \
  #     e
  #      \
  #       d (complete word)

  expect_true(is_deleted)
  expect_true(exists("b", envir = trie@root@children))
  expect_false(trie@root@children$b@is_complete_word)
  expect_true(exists("e", envir = trie@root@children$b@children))
  expect_false(trie@root@children$b@children$e@is_complete_word)
  expect_true(exists("d", envir = trie@root@children$b@children$e@children))
  expect_true(trie@root@children$b@children$e@children$d@is_complete_word)
})

test_that("trie_delete: report error on invalid argument type", {
  trie <- tr(list())

  # Invalid argument type for word_to_delete
  expect_error(trie_delete(trie, 5678))

  # Invalid argument type for trie
  expect_error(trie_delete("a string", "abc"))
})

test_that("trie_delete: report error on empty word_to_delete", {
  trie <- tr(list())

  expect_error(trie_delete(trie, ""))
})
