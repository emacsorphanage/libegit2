(ert-deftest message-prettify ()
  (should (string= "Just header\n"
                   (libgit-message-prettify "Just header    ")))
  (should (string= "  Leading space\n"
                   (libgit-message-prettify "  Leading space")))
  (should (string= "Title\n\nExtra lines\n"
                   (libgit-message-prettify "Title  \n \nExtra lines")))
  (should (string= "Comments\n# Here's one\n"
                   (libgit-message-prettify "Comments\n# Here's one")))
  (should (string= "Comments\n"
                   (libgit-message-prettify "Comments\n# Here's one" ?#)))
  (should (string= "Comments\n# Here's one\n"
                   (libgit-message-prettify "Comments\n# Here's one" ?.)))
  (should (string= "Comments\n"
                   (libgit-message-prettify "Comments\n. Here's one" ?.))))

(ert-deftest message-trailers ()
  (should (equal '(("Planet" . "Earth") ("Species" . "Humanity"))
                 (libgit-message-trailers
                  "Title\n\nPlanet: Earth\nSpecies: Humanity\n")))
  (should (equal nil (libgit-message-trailers "Nothing here"))))
