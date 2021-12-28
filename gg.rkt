#lang racket

(require racket/os
         racket/serialize
         xml)

; TODO serializable-struct/contract
; TODO serializable-struct/versions/contract

(serializable-struct/versions
  ; TODO Check and mark remote status. git ls-remote --heads (<remote-name>|<remote-addr>)
  Remote
  0
  (name addr)
  ()
  #:transparent)

(set! Remote?
      (struct/dc Remote
                 [name string?] ; TODO Should be: [names (set/c string?)]
                 [addr string?]))

(serializable-struct/versions
  Local
  0
  (hostname path bare? description remotes)
  ()
  #:transparent)

(set! Local?
      (struct/dc Local
                 [hostname    string?]
                 [path        (and/c path? absolute-path?)]
                 [bare?       boolean?]
                 [description (or/c #f string?)]
                 [remotes     (listof Remote?)]))

;; TODO locals should be a custom set keyed on hostname+path
(define locals? (listof Local?))

(serializable-struct/versions
  Repo
  0
  (roots locals)
  ()
  #:transparent)

(set! Repo?
      (struct/dc Repo
                 [roots  (set/c string?)]
                 [locals locals?]))

(struct/contract Ignore
                 ([prefix (set/c path?)]
                  [regexp (set/c pregexp?)]))

(struct/contract Search
                 ([ignore Ignore?]))

(struct/contract DirTree
                 ([root path?]))

(define out-format? (or/c 'serial DirTree? 'report-graph 'report-html))
(define out-dst? (or/c (cons/c 'file path?) 'stdout))
(define data-source? (or/c Search? 'read))

(struct Ok (data) #:transparent)

(struct Error (data) #:transparent)

(define (Result/c α β)
  (or/c (struct/dc Ok [data α])
        (struct/dc Error [data β])))

(define current-ignore-file
  (make-parameter
    (build-path (current-directory)
                (format ".gg-ignore-~a" (gethostname)))))

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
  (-> path? (listof Remote?))
  ; FIXME Handle N names for 1 address.
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "remote" "-v")
    [(Error _) '()]
    [(Ok lines)
     (uniq (map (λ (line) (apply Remote (take (string-split line) 2))) lines))]))

(define/contract (git-dir->bare? dir)
  (-> path? (Result/c boolean? integer?))
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "rev-parse" "--is-bare-repository")
    [(and (Error _) e) e]
    [(Ok (list* "true" _)) (Ok #t)]
    [(Ok (list* "false" _)) (Ok #f)]))

(define/contract (git-dir->roots dir)
  (-> path? (or/c #f (listof string?)))
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "rev-list" "--max-parents=0" "HEAD")
    [(Error _) #f]
    [(Ok '()) #f]
    [(Ok (and (list* _ _) roots)) roots]))

(define/contract (find-git-dirs search-paths)
  (-> (listof path?) (listof path?))
  (define (find search-path)
    ; TODO Check OS and maybe dispatch the (albeit slower) Racket version of find.
    ; TODO find can take all the search paths at once - pass them here!
    (match (exe "find" (path->string search-path) "-type" "d" "-name" ".git")
      [(Error _) '()]
      [(Ok lines) (map normalize-path lines)]))
  (append* (map find search-paths)))

(define uniq
  (compose set->list list->set))

(define/contract (local-dir->description path)
  (-> path? (or/c #f string?))
  (define path-description (build-path path "description"))
  (if (file-exists? path-description)
      (match (file->lines path-description)
        ['() #f]
        [(list* line _)
         (if (string-prefix? line "Unnamed repository;")
             #f
             line)])
      #f))

(define/contract (find-git-repos hostname search-paths ignore)
  (-> string? (listof path?) Ignore? (listof Repo?))
  ; All root sets are the same in a group, so roots of the first repo is as good as any:
  (define (group->roots group) (first (first group)))
  (define (group->local-dirs group) (map second group))
  (define (group->remotes group) (uniq (append* (map third group))))
  (map (λ (group)
          (define roots (group->roots group))
          (define locals
            (map (λ (local-dir)
                    (define description (local-dir->description local-dir))
                    (define remotes (group->remotes group))
                    (define bare?-result (git-dir->bare? local-dir))
                    ; XXX Assuming we already know local-dir is a valid repo:
                    (invariant-assertion Ok? bare?-result)
                    (Local hostname
                           local-dir
                           (Ok-data bare?-result)
                           description
                           remotes))
                 (group->local-dirs group)))
          (Repo roots locals))
       (group-by first
                 (foldl (λ (dir repos)
                           ; TODO git lookups can be done concurrently
                           (match (git-dir->roots dir)
                             [#f repos]
                             [roots
                               (define remotes (git-dir->remotes dir))
                               (define repo (list (list->set roots) dir remotes))
                               (cons repo repos)]))
                        '()
                        (filter
                          (λ (path)
                             (define path-str (path->string path))
                             (and (not (ormap (λ (p)
                                                 (string-prefix? path-str
                                                                 (path->string p)))
                                              (set->list (Ignore-prefix ignore))))
                                  (not (ormap (λ (px) (regexp-match? px path-str))
                                              (set->list (Ignore-regexp ignore))))))
                          (find-git-dirs search-paths))))))

(define/contract (multi-homed? repo)
  (-> Repo? boolean?)
  (match (Repo-locals repo)
    [(list* _ _ _) #t]
    [(list* _) #f]))

(define/contract (multi-rooted? repo)
  (-> Repo? boolean?)
  (> (set-count (Repo-roots repo)) 1))

(define/contract (remoteless? repo)
  (-> Repo? boolean?)
  (match (append* (map Local-remotes (Repo-locals repo)))
    ['() #t]
    [(list* _) #f]))

(define/contract (ignore-union i1 i2)
  (-> Ignore? Ignore? Ignore?)
  (Ignore (set-union (Ignore-prefix i1)
                     (Ignore-prefix i2))
          (set-union (Ignore-regexp i1)
                     (Ignore-regexp i2))))

;; TODO Tests for file->ignore.
(define/contract (file->ignore path)
  (-> path? Ignore?)
  ; # comment
  ; p <prefix>
  ; x <regexp>
  (eprintf "parsing ignore file: ~a~n" path)
  (foldl (λ (line ignore)
            (define len (string-length line))
            (cond
              ; empty
              [(= len 0) ignore]

              ; comment
              [(and (> len 0)
                    (equal? #\# (string-ref line 0)))
               ignore]

              ; prefix
              [(and (> len 2)
                    (equal? #\p (string-ref line 0))
                    (path-string? (substring line 2 len)))
               (struct-copy Ignore
                            ignore
                            [prefix (set-add (Ignore-prefix ignore)
                                             (expand-user-path (substring line 2 len)))])]

              ; regexp
              [(and (> len 2)
                    (equal? #\x (string-ref line 0))
                    (pregexp? (pregexp (substring line 2 len) identity)))
               (struct-copy Ignore
                            ignore
                            [regexp (set-add (Ignore-regexp ignore)
                                             (pregexp (substring line 2 len)))])]

              ; invalid
              [else
                (eprintf "WARNING: skipping an invalid line in ignore file: ~v~n" line)
                ignore]))
         (Ignore (set) (set))
         (file->lines path)))

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
    (define path (path->string (Local-path l)))
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
            (set-for-each
              (Repo-roots repo)
              (λ (root)
                 (printf
                   "~a -> ~a [label=~a, fontname=monospace, fontsize=8, color=yellowgreen];~n"
                   (node-id-root root)
                   (node-id-local loc)
                   (edge-label-root2local root loc))
                 (set-add! all-roots root)))
            (set-add! all-locals loc)
            (for-each
              (λ (rem)
                 (set-add! all-remotes rem)
                 (printf
                   ; TODO Red edge between a local and an unreachable remote.
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
       (printf
         "~a [label=~a, shape=oval, style=filled, fillcolor=lightblue, fontname=monospace, fontsize=8];~n"
         (node-id-remote r)
         (node-label-remote r))))

  (displayln "}"))

(define/contract (output-html repos)
  (-> (listof Repo?) void?)
  (define (remotes-table remotes)
    (if (empty? remotes)
        '()
        (list* 'table '([bgcolor "lightblue"]
                        [border      "0"]
                        [cellborder  "0"]
                        [cellspacing "5"]
                        [cellpadding "5"])
               '(tr
                 (th "name")
                 (th "addr"))
               (map (λ (rem)
                       `(tr
                         (td ,(Remote-name rem))
                         (td ,(Remote-addr rem))))
                    remotes))))
  (define (local->row loc)
    `(tr
      (td ,(Local-hostname loc))
      (td ,(path->string (Local-path loc)))
      (td ,(remotes-table (Local-remotes loc)))))
  (define (locals-table locals)
    (list* 'table '([border      "0"]
                    [cellborder  "0"]
                    [cellspacing "5"]
                    [cellpadding "5"])
           '(tr
             (th "host")
             (th "path")
             (th "remotes"))
           (map local->row locals)))
  (define (roots-list roots)
    (list* 'ul (map (λ (r) `(li ,r)) roots)))
  (define (repo->row r)
    `(tr
      (td ([bgcolor "yellowgreen"]) ,(roots-list (set->list (Repo-roots r))))
      (td ([bgcolor "wheat"]) ,(locals-table (Repo-locals r)))))
  (define repos-table
    (list* 'table '([border      "0"]
                    [cellborder  "0"]
                    [cellspacing "5"]
                    [cellpadding "0"])
           '(tr
             (th ([align "left"]) "roots")
             (th ([align "left"]) "locals"))
           (map repo->row repos)))
  (displayln (xexpr->string `(html
                              (head)
                              (body
                                ([style "font-family:mono"])
                                ,repos-table)))))

(define/contract (output-dir-tree repos rooted-in)
  (-> (listof Repo?) path? void?)
  (for-each
    (λ (rep)
       (for-each
         (λ (loc)
            (define local-path
              ; XXX Use work tree for non-bares. TODO Consider optionalizing.
              (if (Local-bare? loc)
                  (Local-path loc)
                  (match-let-values
                    ([(work-tree .git trailing-slash?) (split-path (Local-path loc))])
                    (invariant-assertion absolute-path? work-tree)
                    (invariant-assertion ".git" (path->string .git))
                    (invariant-assertion #f trailing-slash?)
                    work-tree)))
            (define dir-index-by-host
              (let ([new-root (build-path rooted-in
                                          "indices"
                                          "by-host"
                                          (Local-hostname loc))])
                (reroot-path local-path new-root)))
            (define file-roots (build-path dir-index-by-host "roots.txt"))
            (define file-remotes (build-path dir-index-by-host "remotes.txt"))
            (make-parent-directory* file-roots)
            (make-parent-directory* file-remotes)
            (display-lines-to-file
              (map (λ (rem) (format "~a ~a" (Remote-name rem) (Remote-addr rem)))
                   (sort (Local-remotes loc)
                         (λ (r1 r2) (string<? (Remote-name r1)
                                              (Remote-name r2)))))
              file-remotes
              #:exists 'replace)
            (display-lines-to-file
              (sort (set->list (Repo-roots rep)) string<?)
              file-roots
              #:exists 'replace))
         (Repo-locals rep)))
    repos))

(define/contract (input data-source input-paths)
  (-> data-source? (listof path?) (listof Repo?))
  (match data-source
    [(Search ignore)
     (find-git-repos (gethostname) input-paths ignore)]
    ['read
     (if (empty? input-paths)
         (deserialize (read))
         (append* (map (λ (p)
                          (with-input-from-file p (λ () (deserialize (read)))))
                       input-paths)))]))

(define/contract (output out-format repos)
  (-> out-format? (listof Repo?) void?)
  (match out-format
    ['serial (write (serialize repos))]
    [(DirTree rooted-in) (output-dir-tree repos rooted-in)]
    ['report-html (output-html repos)]
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
  (eprintf "~a ~a repos, ~a roots, ~a locals and ~a remotes in ~a seconds.~n"
           (match data-source
             [(Search _) "Found"]
             [(or 'read) "Read"])
           (length repos)
           (length (append* (map (compose set->list Repo-roots) repos)))
           (length (uniq (append* (map Repo-locals repos))))
           (length (uniq (flatten (map (λ (locals) (map Local-remotes locals))
                                       (map Repo-locals repos)))))
           (real->decimal-string (/ (- t1 t0) 1000) 3)))

(module+ main
  ; TODO sub commands?
  (let ([out-format     'serial]
        [out-dst        'stdout]
        [out-filters    (mutable-set)]
        [data-source    (Search (Ignore (set) (set)))]
        [exclude-prefix (mutable-set)]
        [exclude-regexp (mutable-set)])
    (command-line
      #:program (find-system-path 'run-file)

      ; TODO stdin is default if no input file paths provided
      ; TODO overhaul option prefixes: --in-.., --out-.., ...

      ; Input actions:
      #:once-any
      [("--search")
       "Find repos on the current machine via a filesystem search in the given paths. [DEFAULT]"
       (set! data-source (Search (Ignore (set) (set))))]
      [("--read")
       "Merge serialized search results from previous searches (maybe from multiple machines)."
       (set! data-source 'read)]

      ; Input filters:
      #:multi
      [("-i" "--ignore-file")
       ignore-file "Input filters file. Default: $PWD/.gg-ignore-$HOST"
       (invariant-assertion path-string? ignore-file)
       (invariant-assertion file-exists? ignore-file)
       (current-ignore-file (normalize-path ignore-file))]
      [("-e" "--exclude-prefix")
       directory "Input filter: directory subtree prefix to exclude from the found candidate paths."
       (invariant-assertion path-string? directory)
       (set-add! exclude-prefix directory)]
      [("-x" "--exclude-regexp")
       perl-like-regexp "Input filter: pattern to exclude from the found candidate paths."
       (let ([px (pregexp perl-like-regexp (λ (err-msg) err-msg))])
         (invariant-assertion pregexp? px)
         (set-add! exclude-regexp px))]

      ; Output filters:
      #:once-each
      ; TODO orphans (no remotes)
      ; TODO What else?
      [("--multi-homed")
       "Output filter: only repos with multiple local directories (local forks)."
       (set-add! out-filters multi-homed?)]
      [("--multi-rooted")
       "Output filter: only repos with multiple roots."
       (set-add! out-filters multi-rooted?)]
      [("--remoteless")
       "Output filter: only repos without remotes."
       (set-add! out-filters remoteless?)]

      ; Output format:
      #:once-any
      [("-s" "--serial")
       "Output serialized results (for subsequent self-consumption). Lossless. [DEFAULT]"
       (set! out-format 'serial)]
      [("-d" "--dir-tree")
       rooted-in "Output as a directory tree, rooted in the given directory."
       (invariant-assertion path-string? rooted-in)
       (invariant-assertion directory-exists? rooted-in)
       (set! out-format (DirTree (normalize-path rooted-in)))]
      [("-g" "--report-graph")
       "Output a report in DOT language (for Graphviz). Lossy."
       (set! out-format 'report-graph)]
      [("--report-html")
       "Output a report in HTML."
       (set! out-format 'report-html)]

      ; Output destination:
      #:once-each
      [("-o" "--output-file")
       file-path "Output to file. If not provided, output to stdout."
       (invariant-assertion path-string? file-path)
       (set! out-dst (cons 'file (normalize-path file-path)))]

      ; Input sources (files|directories to read|search, depending on input actions):
      #:args input-paths
      (invariant-assertion (listof path-string?) input-paths)
      (invariant-assertion out-format?  out-format)
      (invariant-assertion data-source? data-source)
      (invariant-assertion (set/c (-> Repo? boolean?) #:kind 'mutable) out-filters)
      ; TODO Make sure all other option containers are asserted!
      (match data-source
        [(Search _)
         (let ([from-cli (Ignore (list->set (set->list exclude-prefix))
                                 (list->set (set->list exclude-regexp)))]
               [from-file (if (file-exists? (current-ignore-file))
                              (file->ignore (current-ignore-file))
                              (Ignore (set) (set)))])
           (set! data-source (Search (ignore-union from-cli from-file))))]
        [_
          (void)])
      (let ([input-paths (map normalize-path input-paths)])
        (invariant-assertion (listof absolute-path?) input-paths)
        (main data-source
              input-paths
              out-filters
              out-format
              out-dst)))))
