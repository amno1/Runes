;;;; decompress.asd
(defsystem "decompress"
  :description "Brotli (RFC 7932) and deflate/zlib (RFC 1950/1951) decoders for
                SBCL, with AVX-512 copies when sb-simd provides them.  Decodes
                WOFF2 and WOFF 1.0 font data, Content-Encoding: br and deflate.
                The Brotli static dictionary is vendored."
  :version "1.0.0"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "tables")
                             (:file "reader")
                             (:file "huffman")
                             (:file "decoder")
                             (:file "inflate-vop")
                             (:file "inflate"))))
  :in-order-to ((test-op (test-op "decompress/tests"))))

(defsystem "decompress/tests"
  :description "Tests against libbrotli and libz (through SB-ALIEN) as oracles."
  :depends-on ("decompress")
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (o c) (uiop:symbol-call :decompress/tests :run)))
