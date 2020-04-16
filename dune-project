(lang dune 2.4)

(generate_opam_files true)

(source (github Willenbrink/PaperTerminal))
(authors "Sebastian Willenbrink sebastian.willenbrink@gmail.com")
(maintainers "Sebastian Willenbrink sebastian.willenbrink@gmail.com")

(package
  (name IT8951)
  (synopsis "Interact with the IT8951 driver board via SPI and display images on a EInk display.")
  (depends
    (ctypes (>= 0))
    (ctypes.foreign (>= 0))
    (pcf-parser (>= 0.0.4))
  ))
