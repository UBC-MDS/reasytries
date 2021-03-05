
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reasytries

<!-- badges: start -->

<!-- badges: end -->

The pyeasytries package contains classes and functions that efficiently
store and search words passed by the user based on a trie data
structure.

This package was developed as a project for UBC MDS-Vancouver program
DSCI 524 course.

## Installation

You can install the released version of reasytries from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("reasytries")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/reasytries")
```

## Overview

Storing and searching words can be expensive in terms of time and
computing: for example, using a balanced binary search tree, the time
complexity for searching a word is \(O(mlogn)\), where \(m\) is the
length of the word and \(n\) is the number of keys in the tree. However,
with the trie data structure, the time complexity for searching can be
reduced to \(O(m)\). The `reasytries` package is a simple tool to aid
word insertion, deletion, and searching in the trie data structure.
Users can pass any words to be stored for later-on searching or printing
with certain prefix, and even modify the words in storage.

## Features 

**Classes**

  - `Class TrieNode`: conceptual class representation of a single node in a trie
  - `Class Trie`: a conceptual class representation of a trie
    
**Functions**

  - `trie_create()` function initializes an empty trie
  
  - `trie_add(trie, word_to_add)` function takes in a trie and a word_to_add to store each letter of the new word in the trie data structure.
  
  - `trie_contain(trie, word)` function takes in a trie and a word to search through the trie structure and check if the word has been stored in the trie already.

  - `trie_find_prefix(trie, prefix)` function takes in a trie and a prefix to search through the trie structure and return a list of all word stored with the given prefix.

  - `trie_delete(trie, word_to_delete)` function takes in a trie and a word_to_delete to remove the letters of the word if they are contained in the trie already. The function returns `FALSE` when the trie does not contain the word_to_delete.

## R Ecosystem (TO BE MODIFIED BY Jordan)

This `pyeasytries` package aims to simplify and speed up the process of
searching through a fixed dictionary. In addition to time complexity
benefits, a tries data structure also allows to search for given words
inside the fixed dictionary when given a prefix. Users who are building
a search/filter bar from a fixed dictionary will find this package
useful\! As trie is a famous data structure, there are currently some
similar packages in Python such as
[pygtrie](https://pypi.org/project/pygtrie/).