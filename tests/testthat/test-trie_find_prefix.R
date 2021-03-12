test_that("trie_find_prefix: no words should be found with prefix that doesn't occur in the Trie", {
  trie <- tr(list(
    b = tn(FALSE, list(
      e = tn(TRUE, list(
        e = tn(TRUE, list()))),
      a = tn(FALSE, list(
        d = tn(TRUE, list())))
    ))))
  #     root
  #      |
  #      b
  #     / \
  #    a   e
  #   /     \
  #  d       e

  prefix_not_found <- trie_find_prefix(trie, "ca")
  # Finding a prefix that's not in the Trie, should return:
  # an empty vector

  expect_length(prefix_not_found, 0)
  expect_true(exists("b", envir = trie@root@children))
  expect_true(exists("a", envir = trie@root@children$b@children))
  expect_true(trie@root@children$b@children$a@children$d@is_complete_word)
})

test_that("trie_find_prefix: at the minimum the prefix should be returned when user passes in a complete word as prefix", {
  trie <- tr(list(
    b = tn(FALSE, list(
      e = tn(TRUE, list(
        e = tn(TRUE, list()))),
      a = tn(FALSE, list(
        d = tn(TRUE, list())))
    ))))
  #     root
  #      |
  #      b
  #     / \
  #    a   e
  #   /     \
  #  d       e

  prefix_is_whole_word <- trie_find_prefix(trie, "be")
  # Finding a prefix that's already a complete word in the Trie, should return:
  # at the minimum the prefix itself and plus another other complete word(s)

  expect_equal(prefix_is_whole_word, c("be", "bee"))
  expect_true(exists("b", envir = trie@root@children))
  expect_true(exists("e", envir = trie@root@children$b@children))
  expect_true(trie@root@children$b@children$e@children$e@is_complete_word)
})

test_that("trie_find_prefix: report error on invalid argument type", {
  trie <- tr(list())

  # Invalid argument type for prefix
  expect_error(trie_find_prefix(trie, 5678))

  # Invalid argument type for trie
  expect_error(trie_find_prefix("a string", "be"))
})

test_that("trie_find_prefix: report error on empty prefix", {
  trie <- tr(list())

  expect_error(trie_find_prefix(trie, ""))
})
