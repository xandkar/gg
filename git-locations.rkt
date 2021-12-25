#lang racket

(require racket/os
         racket/serialize
         xml)

(module+ test
  (require rackunit))

; TODO serializable-struct/contract
; TODO serializable-struct/versions/contract

; TODO Check if addr is actually a local path and mark:
;      - whether it exists
;      - whether it is _a_ git repo at all
;        - whether it _the_ correct git repo (shares root)
; TODO Maybe optionally check the actual remote addresses as well.
; TODO Check remote status with: git ls-remote --heads (<remote-name>|<remote-addr>)
; TODO Mark the above ^^^ stuff in this struct or some table?

(serializable-struct Remote (name addr) #:transparent)
(set! Remote?
      (struct/dc Remote
                 [name string?]
                 [addr string?]))

(serializable-struct Local (hostname path description remotes) #:transparent)
(set! Local?
      (struct/dc Local
                 [hostname    string?]
                 [path        path-string?]
                 [description (or/c #f string?)]
                 [remotes     (listof Remote?)]))

;; TODO locals should be a custom set keyed on hostname+path
(define locals? (listof Local?))

(serializable-struct Repo (root locals) #:transparent)
(set! Repo?
      (struct/dc Repo
                 [root   string?]
                 [locals locals?]))

(define out-format? (or/c 'table 'graph 'serial))
(define out-dst? (or/c (cons/c 'file path-string?) 'stdout))

(struct/contract Search
                 ([exclude-prefix (listof path-string?)]
                  [exclude-regexp (listof pregexp?)]))

(define data-source? (or/c Search? 'merge 'merge-tables))

(define/contract (exe cmd)
  (-> string? (listof string?))
  ; TODO Switch from (process command ...) to (process* executable args ...)
  (match-define
    (list stdout stdin _pid stderr ctrl)
    (process cmd))
  (define lines (port->lines stdout))
  (ctrl 'wait)
  (match (ctrl 'status)
    ['done-ok (void)]
    ['done-error
     (copy-port stderr (current-error-port))
     (exit 1)])
  (close-output-port stdin)
  (close-input-port stdout)
  (close-input-port stderr)
  lines)

(define/contract (git-dir->remotes git-dir-path)
  (-> path-string? (listof Remote?))
  ; TODO Replace piping to unix filters with Racket-written filters.
  (define cmd
    (string-append
      "git --git-dir=" git-dir-path " remote -v | awk '{print $1, $2}' | sort -u"))
  (map (λ (line)
          (match-define (list name addr) (string-split line))
          (Remote name addr))
       (exe cmd)))

(define/contract (git-dir->root git-dir-path)
  (-> path-string? (or/c #f string?))
  ; TODO Replace piping to unix filters with Racket-written filters.
  (define cmd
    (string-append
      "git --git-dir=" git-dir-path " rev-list --reverse HEAD | head -1"))
  (match (exe cmd)
    ['() #f]
    [(list root) root]
    [_ (assert-unreachable)]))

(define/contract (find-git-dirs search-paths)
  (-> (listof path-string?) (listof path-string?))
  (define (find search-path)
    ; TODO Check OS and maybe dispatch the (albeit slower) Racket version of find.
    (exe (string-append "find " search-path " -type d -name .git")))
  (append* (map find search-paths)))

(define uniq
  (compose set->list list->set))

(define/contract (local-dir->description path)
  (-> path-string? (or/c #f string?))
  (define path-description (build-path path "description"))
  (if (file-exists? path-description)
      (match (file->lines path-description)
        ['() #f]
        [(list* line _)
         (if (string-prefix? line "Unnamed repository;")
             #f
             line)])
      #f))

(define/contract (find-git-repos hostname search-paths exclude-prefix exclude-regexp)
  (-> string? (listof path-string?) (listof path-string?) (listof pregexp?) (listof Repo?))
  ; All roots are the same in a group, so root of first repo is as good as any:
  (define (group->root group) (first (first group)))
  (define (group->local-dirs group) (map second group))
  (define (group->remotes group) (uniq (append* (map third group))))
  (map (λ (group)
          (define root (group->root group))
          (define locals
            (map (λ (local-dir)
                    (define description (local-dir->description local-dir))
                    (define remotes (group->remotes group))
                    (Local hostname local-dir description remotes))
                 (group->local-dirs group)))
          (Repo root locals))
       (group-by first
                 (foldl (λ (dir repos)
                           ; TODO git lookups can be done concurrently
                           (match (git-dir->root dir)
                             [#f repos]
                             [root
                               (define remotes (git-dir->remotes dir))
                               (define repo (list root dir remotes))
                               (cons repo repos)]))
                        '()
                        (filter
                          (λ (path)
                             (and (not (ormap (curry string-prefix? path)
                                              exclude-prefix))
                                  (not (ormap (λ (px) (regexp-match? px path))
                                              exclude-regexp))))
                          (find-git-dirs search-paths))))))

(define/contract (output-table repos)
  (-> (listof Repo?) void?)
  (define (row . columns) (displayln (string-join columns " ")))
  (for* ([r repos]
         [l (Repo-locals r)]
         [m (if (null? (Local-remotes l))
                `(,(Remote "" ""))
                (Local-remotes l))])
        (row (Repo-root r)
             (Local-hostname l)
             (Local-path l)
             (Remote-name m)
             (Remote-addr m))))

(define row?
  (or/c (list/c string? string? string? string? string?)
        (list/c string? string? string?)))

(define/contract (file->rows path)
  (-> path-string? (listof row?))
  (map string-split (file->lines path)))

(define/contract (rows->repos rows)
  (-> (listof row?) (listof Repo?))
  ; <root> <host> <path> [<name> <address>]
  (define row-root first)
  (define row-host second)
  (define row-path third)

  ; Single local per repo, so may have duplicate-root repos in the list:
  (define root-per-local
    (for*/list
      ([rows-sharing-root           (group-by row-root rows)]
       [rows-sharing-root-host      (group-by row-host rows-sharing-root)]
       [rows-sharing-root-host-path (group-by row-path rows-sharing-root-host)])

      (define remotes
        (append* (map (λ (row)
                         (match row
                           [(list _ _ _ name addr) (list (Remote name addr))]
                           [(list _ _ _)           (list)]))
                      rows-sharing-root-host-path)))

      ; XXX First is as good as any, since these fields are shared:
      (define root (row-root (first rows-sharing-root-host-path)))
      (define host (row-host (first rows-sharing-root-host-path)))
      (define path (row-path (first rows-sharing-root-host-path)))

      (define locals (list (Local host path #f remotes)))
      (Repo root locals)))

  (define merged-locals-by-root
    (foldl
      (λ (repos-sharing-root repos)
         ; First is as good as any other:
         (define root (Repo-root (first repos-sharing-root)))
         (define locals (append* (map Repo-locals repos-sharing-root)))
         (define repo (Repo root locals))
         (cons repo repos))
      '()
      (group-by Repo-root root-per-local)))

  merged-locals-by-root)

(define (file->repos path)
  (rows->repos (file->rows path)))

(define/contract (input-table paths)
  (-> (listof path-string?) (listof Repo?))
  (append* (map file->repos paths)))

(module+ test
  ; TODO [x] Test : rows -> repos
  ; TODO [ ] Test : repos -> file -> repos
  ; TODO [x] Test :   single file -> repos
  ; TODO [ ] Test : multiple files -> repos
  ; TODO [ ] Test : single-host-filesystem -> repos -> file -> repos
  ; TODO [ ] Test :  multi-host-filesystem -> repos -> file -> repos

  (let* ([test-file (make-temporary-file)]
         [test-lines '("root host dir name addr")])
    (display-lines-to-file test-lines test-file #:exists 'replace)
    (check-equal?
      (file->rows test-file)
      '(("root" "host" "dir" "name" "addr")))
    (check-equal?
      (file->repos test-file)
      (list (Repo "root" (list (Local "host" "dir" #f (list (Remote "name" "addr")))))))
    (delete-file test-file))

  (check-equal?
    (rows->repos '(("r" "h" "d" "n" "a")))
    (list (Repo "r" (list (Local "h" "d" #f (list (Remote "n" "a")))))))

  (check-equal?
    (rows->repos
      '(("r" "h" "d" "n1" "a1")
        ("r" "h" "d" "n2" "a2")))
    (list (Repo "r" (list (Local "h" "d" #f (list (Remote "n1" "a1")
                                                  (Remote "n2" "a2")))))))

  (let* ([given-rows '(("r" "h" "d1" "n1" "a1")
                       ("r" "h" "d1" "n2" "a2")
                       ("r" "h" "d2" "n2" "a2"))]
         [actual-repos (rows->repos given-rows)]
         [expected-repos
           (list (Repo "r" (list (Local "h" "d1" #f (list (Remote "n1" "a1")
                                                          (Remote "n2" "a2")))
                                 (Local "h" "d2" #f (list (Remote "n2" "a2"))))))])
    (check-equal? 1 (length actual-repos))
    (check-equal? actual-repos expected-repos))

  (let* ([given-rows '(("r" "h1" "d1" "n1" "a1")
                       ("r" "h1" "d1" "n2" "a2")
                       ("r" "h1" "d2" "n2" "a2")
                       ("r" "h2" "d2" "n2" "a2"))]
         [actual-repos (rows->repos given-rows)]
         [expected-repos
           (list (Repo "r" (list (Local "h1" "d1" #f (list (Remote "n1" "a1")
                                                           (Remote "n2" "a2")))
                                 (Local "h1" "d2" #f (list (Remote "n2" "a2")))
                                 (Local "h2" "d2" #f (list (Remote "n2" "a2"))))))])
    (check-equal? 1 (length actual-repos))
    (check-equal? actual-repos expected-repos))

  (let* ([given-rows '(("r1" "h1" "d1" "n1" "a1")
                       ("r1" "h1" "d1" "n2" "a2")
                       ("r1" "h1" "d2" "n2" "a2")
                       ("r1" "h2" "d2" "n2" "a2")
                       ("r2" "h2" "d3" "n1" "a3"))]
         [actual-repos (rows->repos given-rows)]
         [expected-repos
           (list (Repo "r2" (list (Local "h2" "d3" #f (list (Remote "n1" "a3")))))
                 (Repo "r1" (list (Local "h1" "d1" #f (list (Remote "n1" "a1")
                                                            (Remote "n2" "a2")))
                                  (Local "h1" "d2" #f (list (Remote "n2" "a2")))
                                  (Local "h2" "d2" #f (list (Remote "n2" "a2"))))))])
    (check-equal? 2 (length actual-repos))
    (check-equal? actual-repos expected-repos)))

;; At least 2 locals.
(define/contract (multi-homed? repo)
  (-> Repo? boolean?)
  (match (Repo-locals repo)
    [(list* _ _ _) #t]
    [(list* _) #f]))

(define/contract (output-graph repos)
  (-> (listof Repo?) void?)
  (define all-roots (mutable-set))
  (define all-locals (mutable-set))
  (define all-remotes (mutable-set))

  (define (edge-label-root2local _r _l) "\"\"")
  (define (edge-label-local2remote _l r) (format "~v" (Remote-name r)))

  (define (node-id-root r) (format "\"root:~a\"" r))
  (define (node-id-remote r) (format "\"remote:~a\"" (Remote-addr r)))
  (define (node-id-local l) (format "\"local:~a:~a\"" (Local-hostname l) (Local-path l)))

  (define (node-label-root r) (format "~v" r))
  (define (node-label-remote r) (format "~v" (Remote-addr r)))
  (define (node-label-local l)
    (define hostname (Local-hostname l))
    (define description (let ([d (Local-description l)]) (if d d "")))
    (define path (Local-path l))
    (format "<~a>"
            (xexpr->string
              `(table
                ([border      "0"]
                 [cellborder  "0"]
                 [cellpadding "1"]
                 [cellspacing "0"])
                (tr (td (b "host ")) (td ([align "left"]) ,hostname))
                (tr (td (b "path ")) (td ([align "left"]) ,path))
                (tr (td (b "desc ")) (td ([align "left"]) ,description))))))

  (displayln "digraph {")
  (for-each
    (λ (repo)
       (for-each
         (λ (loc)
            (set-add! all-roots (Repo-root repo))
            (set-add! all-locals loc)
            (printf
              "~a -> ~a [label=~a, fontname=monospace, fontsize=8, color=yellowgreen];~n"
              (node-id-root (Repo-root repo))
              (node-id-local loc)
              (edge-label-root2local (Repo-root repo) loc))
            (for-each
              (λ (rem)
                 (set-add! all-remotes rem)
                 (printf
                   "~a -> ~a [label=~a, fontname=monospace, fontsize=8, color=lightblue fontcolor=lightblue3, dir=both, arrowtail=dot];~n"
                   (node-id-local loc)
                   (node-id-remote rem)
                   (edge-label-local2remote loc rem)))
              (Local-remotes loc)))
         (Repo-locals repo)))
    repos)

  (invariant-assertion (set/c string? #:kind 'mutable) all-roots)
  (set-for-each
    all-roots
    (λ (r)
       (printf
         "~a [label=~a, shape=rectangle, style=filled, fillcolor=yellowgreen, fontname=monospace, fontsize=8];~n"
         (node-id-root r)
         (node-label-root r))))

  (invariant-assertion (set/c Local? #:kind 'mutable) all-locals)
  (set-for-each
    all-locals
    (λ (l)
       (printf
         "~a [label=~a shape=folder, style=filled, fillcolor=wheat, fontname=monospace, fontsize=8, fontcolor=black];~n"
         (node-id-local l)
         (node-label-local l))))

  (invariant-assertion (set/c Remote? #:kind 'mutable) all-remotes)
  (set-for-each
    all-remotes
    (λ (r)
       ; TODO Use shape=folder for scheme-less remotes (i.e. dirs).
       ; TODO Use fillcolor=red for local references to nonexisting dir.
       (printf
         "~a [label=~a, shape=oval, style=filled, fillcolor=lightblue, fontname=monospace, fontsize=8];~n"
         (node-id-remote r)
         (node-label-remote r))))

  (displayln "}"))

(define/contract (input data-source input-paths)
  (-> data-source? (listof path-string?) (listof Repo?))
  (match data-source
    [(Search exclude-prefix exclude-regexp)
     (find-git-repos (gethostname) input-paths exclude-prefix exclude-regexp)]
    ['merge
     (if (empty? input-paths)
         (deserialize (read))
         (append* (map (λ (p)
                          (with-input-from-file p (λ () (deserialize (read)))))
                       input-paths)))]
    ['merge-tables
     (if (empty? input-paths)
         (raise "Reading table from stdin is not currently implemented.") ; TODO
         (input-table input-paths))]))

(define/contract (output out-format repos)
  (-> out-format? (listof Repo?) void?)
  (match out-format
    ['serial (write (serialize repos))]
    ['table (output-table repos)]
    ['graph (output-graph repos)]))

(define (main data-source input-paths out-filters out-format out-dst)
  (define t0 (current-inexact-milliseconds))
  ; TODO Time filters separately from reading input.
  (define repos
    (let ([repos (input data-source input-paths)])
      (match (set->list out-filters)
        ['() repos]
        [(and (list* _ _) filters)
         (filter (λ (r) (andmap (λ (f) (f r)) filters)) repos)])))
  (define t1 (current-inexact-milliseconds))
  (match out-dst
    ['stdout (output out-format repos)]
    [(cons 'file file-path)
     (with-output-to-file file-path
                          (λ () (output out-format repos))
                          #:exists 'replace)])
  (eprintf "~a ~a roots, ~a locals and ~a remotes in ~a seconds.~n"
           (match data-source
             [(Search _ _) "Found"]
             [(or 'merge 'merge-tables) "Read"])
           (length repos)
           (length (uniq (append* (map Repo-locals repos))))
           (length (uniq (flatten (map (λ (locals) (map Local-remotes locals))
                                       (map Repo-locals repos)))))
           (real->decimal-string (/ (- t1 t0) 1000) 3))
  )

(module+ main
  ; TODO handle sub commands:
  ; - TODO "collect" data for current host
  ; - TODO "integrate" data from per-host data files into a graphviz file

  (let ([out-format     'serial]
        [out-dst        'stdout]
        [out-filters    (mutable-set)]
        [data-source    (Search '() '())]
        [exclude-prefix (mutable-set)]
        [exclude-regexp (mutable-set)])
    (command-line
      #:program (find-system-path 'run-file)

      ; TODO serialization format as default input format
      ; TODO merge by default (instead of search)
      ; TODO stdin is default if no input file paths provided
      ; TODO make this work: git-locations --search ~ | git-locations --graph | neato | feh

      ; TODO overhaul option prefixes: --in-.., --out-.., ...

      ; Input actions:
      #:once-any
      [("--search")
       "Find repos on the current machine via a filesystem search in the given paths. [DEFAULT]"
       (set! data-source (Search '() '()))]
      [("--merge")
       "Merge serialized search results from previous searches (maybe from multiple machines)."
       (set! data-source 'merge)]
      [("--merge-tables")
       "Merge tabularized search results from previous searches (maybe from multiple machines)."
       (set! data-source 'merge-tables)]

      ; Input filters:
      #:multi
      [("-e" "--exclude-prefix")
       directory "Input filter: directory subtree prefix to exclude from the found candidate paths."
       (invariant-assertion path-string? directory)
       (set-add! exclude-prefix directory)]
      [("-x" "--exclude-regexp")
       perl-like-regexp "Input filter: pattern to exclude from the found candidate paths."
       (let ([px (pregexp perl-like-regexp (λ (err-msg) err-msg))])
         (invariant-assertion pregexp? px)
         (set-add! exclude-regexp px))]
      ; TODO --exclude-file which contains all the exclusions:
      ;     # comment
      ;     p <prefix>
      ;     x <regexp>

      ; Output filters:
      #:once-each
      ; TODO orphans (no remotes)
      ; TODO What else?
      [("--multi-homed")
       "Output filter: only repos with multiple local directories (local forks)."
       (set-add! out-filters multi-homed?)]

      ; Output format:
      #:once-any
      [("-s" "--serial")
       "Output in Racket serialization format (for self-consumption). Lossless. [DEFAULT]"
       (set! out-format 'serial)]
      [("-t" "--table")
       "Output in a tabular text format (for Unix tools consumption). Lossy."
       (set! out-format 'table)]
      [("-g" "--graph")
       "Output in DOT language (for Graphviz). Lossy."
       (set! out-format 'graph)]
      ; TODO --html

      ; Output destination:
      #:once-each
      [("-o" "--output-file")
       file-path "Output to file. If not provided, output to stdout."
       (invariant-assertion path-string? file-path)
       (set! out-dst (cons 'file file-path))]

      ; Input sources (files|directories to merge|search, depending on input actions):
      #:args input-paths
      (invariant-assertion (listof path-string?) input-paths)
      (invariant-assertion out-format?  out-format)
      (invariant-assertion data-source? data-source)
      (invariant-assertion (set/c (-> Repo? boolean?) #:kind 'mutable) out-filters)
      ; TODO Make sure all other option containers are asserted!
      (match data-source
        [(Search _ _)
         (set! data-source (Search (set->list exclude-prefix)
                                   (set->list exclude-regexp)))]
        [_
          (void)])

      (main data-source input-paths out-filters out-format out-dst))))
