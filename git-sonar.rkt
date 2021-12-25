#lang racket

(require racket/os
         racket/serialize
         xml)

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
                 [name string?] ; TODO Should be: [names (listof string?)]
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

(define out-format? (or/c 'report-graph 'serial))
(define out-dst? (or/c (cons/c 'file path-string?) 'stdout))

(struct/contract Search
                 ([exclude-prefix (listof path-string?)]
                  [exclude-regexp (listof pregexp?)]))

(define data-source? (or/c Search? 'merge))

(struct Ok (data) #:transparent)

(struct Error (data) #:transparent)

(define (Result/c α β)
  (or/c (struct/dc Ok [data α])
        (struct/dc Error [data β])))

(define/contract (exe program . args)
  (->* (string?) #:rest (listof string?) (Result/c (listof string?) integer?))
  (define program-path (find-executable-path program))
  (match-define
    (list stdout stdin _pid stderr ctrl)
    (apply process* program-path args))
  (define lines (port->lines stdout))
  (ctrl 'wait)
  (match (ctrl 'status)
    ['done-ok (void)]
    ['done-error
     (eprintf "~n")
     (eprintf "Command failure. program-path:~v args:~v~n" program-path args)
     (copy-port stderr (current-error-port))
     (eprintf "~n")
     (Error (ctrl 'exit-code))])
  (close-output-port stdin)
  (close-input-port stdout)
  (close-input-port stderr)
  (Ok lines))

(define/contract (git-dir->remotes dir)
  (-> path-string? (listof Remote?))
  ; FIXME Handle N names for 1 address.
  (match (exe "git" (format "--git-dir=~a" dir) "remote" "-v")
    [(Error _) '()]
    [(Ok lines)
     (uniq (map (λ (line) (apply Remote (take (string-split line) 2))) lines))]))

(define/contract (git-dir->root dir)
  (-> path-string? (or/c #f string?))
  (match (exe "git" (format "--git-dir=~a" dir) "rev-list" "--max-parents=0" "HEAD")
    [(Error _) #f]
    [(Ok '()) #f]
    [(Ok (list* root _)) root])) ; FIXME Refactor for multiple roots!

(define/contract (find-git-dirs search-paths)
  (-> (listof path-string?) (listof path-string?))
  (define (find search-path)
    ; TODO Check OS and maybe dispatch the (albeit slower) Racket version of find.
    ; TODO find can take all the search paths at once - pass them here!
    (match (exe "find" search-path "-type" "d" "-name" ".git")
      [(Error _) '()]
      [(Ok lines) lines]))
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
                       input-paths)))]))

(define/contract (output out-format repos)
  (-> out-format? (listof Repo?) void?)
  (match out-format
    ['serial (write (serialize repos))]
    ['report-graph (output-graph repos)]))

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
             [(or 'merge) "Read"])
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
      ; TODO make this work: git-sonar --search ~ | git-sonar --graph | neato | feh

      ; TODO overhaul option prefixes: --in-.., --out-.., ...

      ; Input actions:
      #:once-any
      [("--search")
       "Find repos on the current machine via a filesystem search in the given paths. [DEFAULT]"
       (set! data-source (Search '() '()))]
      [("--merge")
       "Merge serialized search results from previous searches (maybe from multiple machines)."
       (set! data-source 'merge)]

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
       "Output serialized results (for subsequent self-consumption). Lossless. [DEFAULT]"
       (set! out-format 'serial)]
      [("-g" "--report-graph")
       "Output a report in DOT language (for Graphviz). Lossy."
       (set! out-format 'report-graph)]
      ; TODO --report-html

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
