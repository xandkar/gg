#lang racket

(require file/sha1
         net/url
         racket/os
         racket/serialize
         xml)

; TODO serializable-struct/contract
; TODO serializable-struct/versions/contract

(serializable-struct/versions Ref 0 (name digest) () #:transparent)

(set! Ref?
      (struct/dc Ref
                 [name string?]
                 [digest string?]))

(serializable-struct/versions
  Remote 0
  (name addr heads reachable?)
  ()
  #:transparent)

(set! Remote?
      (struct/dc Remote
                 ; Possibly multiple names per address.
                 ; TODO Refactor to: [names (set/c string?)] ?
                 [name string?]
                 [addr string?]
                 [heads (listof Ref?)]
                 [reachable? boolean?]))

(serializable-struct/versions
  Local 0
  (hostname path bare? description roots heads remotes)
  ()
  #:transparent)

;; TODO Maybe unify the idea of Local and Remote?
;; "Local" should just be "Repo" and "Remote" a "RemoteRef" which points to a
;; "Repo" in some table, which we try to fill with remote lookups?
;; 1) find local repos;
;; 2) collect remote refs from found local repos;
;; 3) lookup remote refs and add the found ones to list of repos.
(set! Local?
      (struct/dc Local
                 [hostname    string?]
                 [path        (and/c path? absolute-path?)]
                 [bare?       boolean?]
                 [description (or/c #f string?)]
                 [roots       (set/c string?)]
                 [heads       (listof Ref?)]
                 [remotes     (listof Remote?)]))

(define out-filter? (-> Local? boolean?))

(struct/contract Ignore
                 ([prefix (set/c path?)]
                  [regexp (set/c pregexp?)]))

(struct/contract Search
                 ([ignore Ignore?]))

(struct/contract DirTree
                 ([root path?]))

(define out-format? (or/c 'serial DirTree? 'report-graph))
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

(define (str->ref str)
  (match-define (list digest name-path) (string-split str))
  (define name
    ; "refs/heads/author/branch" -> "author/branch"
    ; "refs/heads/master"        -> "master"
    (string-join (drop (string-split name-path "/") 2) "/"))
  (Ref name digest))

(define (lines->refs lines)
  (map str->ref lines))

(define/contract (exe program . args)
  (->* (string?) #:rest (listof string?) (Result/c (listof string?)
                                                   (cons/c integer? (listof string?))))
  (define program-path (find-executable-path program))
  (match-define
    (list stdout stdin _pid stderr ctrl)
    (apply process* program-path args))
  (close-output-port stdin)
  (thread (λ ()
             ; XXX Subprocess gets blocked if we don't drain its stderr
             ;     when there're more err lines than buffer:
             (copy-port stderr (current-error-port))
             (close-input-port stderr)))
  (define lines (port->lines stdout))
  (close-input-port stdout)
  (ctrl 'wait)
  (match (ctrl 'exit-code)
    [0 (Ok lines)]
    [error-code
      (eprintf "~nCommand failure: ~a, program-path:~v, args:~v.~n~n"
               error-code
               (path->string program-path)
               args)
      (Error (cons error-code lines))]))

(define/contract (git-dir->remotes dir)
  (-> path? (listof Remote?))
  ; FIXME Handle N names for 1 address.
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "remote" "-v")
    [(Error _) '()]
    [(Ok lines)
     (map (match-lambda [(list name addr)
                         (define-values (heads reachable?)
                           (match (git-remote-heads addr)
                             [(Ok heads) (values heads #t)]
                             [(Error _) (values '() #f)]))
                         (Remote name addr heads reachable?)])
          (uniq (map (λ (line) (take (string-split line) 2)) lines)))]))

(define/contract (git-dir->bare? dir)
  (-> path? (Result/c boolean? integer?))
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "rev-parse" "--is-bare-repository")
    [(Error (cons code _)) (Error code)]
    [(Ok (list* "true" _)) (Ok #t)]
    [(Ok (list* "false" _)) (Ok #f)]))

(define/contract (git-dir->roots dir)
  (-> path? (or/c #f (listof string?)))
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "rev-list" "--max-parents=0" "HEAD")
    [(Error _) #f]
    [(Ok '()) #f]
    [(Ok (and (list* _ _) roots)) roots]))

(define/contract (git-dir->heads dir)
  (-> path? (Result/c (listof Ref?) integer?))
  (match (exe "git" (format "--git-dir=~a" (path->string dir)) "show-ref" "--heads")
    [(Error (cons code _)) (Error code)]
    [(Ok lines) (Ok (lines->refs lines))]))

(define/contract (git-remote-heads address)
  (-> string? (Result/c (listof Ref?) (or/c integer? 'timeout)))
  (define timeout-seconds 3) ; TODO Make configurable.
  (define timeout-chan (make-channel))
  (define result-chan (make-channel))
  (define timeout-thread
    (thread (λ ()
               (sleep timeout-seconds)
               (channel-put timeout-chan (Error 'timeout)))))
  (define result-thread
    (thread (λ ()
               (define result
                 (match (exe "git" "ls-remote" "--heads" address)
                   [(Error (cons code _)) (Error code)]
                   [(Ok lines) (Ok (lines->refs lines))]))
               (channel-put result-chan result))))
  (define result (sync timeout-chan result-chan))
  (kill-thread result-thread)
  (kill-thread timeout-thread)
  result)

(define/contract (find-git-dirs search-paths)
  (-> (listof path?) (listof path?))
  ; TODO Revert to a stream, write to channel, consume by N workers.
  ; TODO Check OS and maybe dispatch the (albeit slower) Racket version of find.
  (define lines
    (match (apply exe (append (cons "find" (map path->string search-paths))
                              '("-type" "d" "-name" ".git")))
      [(Error (cons _ lines)) lines]
      [(Ok lines) lines]))
  (map normalize-path lines))

(define uniq
  (compose set->list list->set))

(define/contract (git-dir->description path)
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
  (-> string? (listof path?) Ignore? (listof Local?))
  ; TODO concurrent-filter-map
  (filter-map
    (λ (dir)
       (match (git-dir->roots dir)
         ; May still not actually be a valid repo:
         [#f #f]
         ; But if roots fetching worked, then we'll assume it is, and
         ; assert other git ops will work:
         [roots
           (define description          (git-dir->description dir))
           (define bare?       (Ok-data (git-dir->bare? dir)))
           (define heads       (Ok-data (git-dir->heads dir)))
           (define remotes              (git-dir->remotes dir))
           (Local hostname dir bare? description (list->set roots) heads remotes)
           ]))
    (filter
      (λ (dir)
         (let ([dir (path->string dir)])
           (and (not (ormap (λ (p) (string-prefix? dir (path->string p)))
                            (set->list (Ignore-prefix ignore))))
                (not (ormap (λ (px) (regexp-match? px dir))
                            (set->list (Ignore-regexp ignore)))))))
      (find-git-dirs search-paths))))

(define/contract (multi-rooted? loc)
  out-filter?
  (> (set-count (Local-roots loc)) 1))

(define/contract (remoteless? loc)
  out-filter?
  (match (Local-remotes loc)
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

(define/contract (output-graph locals)
  (-> (listof Local?) void?)
  (define all-roots (mutable-set))
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
    (λ (loc)
       (printf
         "~a [label=~a shape=folder, style=filled, fillcolor=wheat, fontname=monospace, fontsize=8, fontcolor=black];~n"
         (node-id-local loc)
         (node-label-local loc))
       (set-for-each
         (Local-roots loc)
         (λ (root)
            (printf
              "~a -> ~a [label=~a, fontname=monospace, fontsize=8, color=yellowgreen];~n"
              (node-id-root root)
              (node-id-local loc)
              (edge-label-root2local root loc))
            (set-add! all-roots root)))
       (for-each
         (λ (rem)
            (set-add! all-remotes rem)
            (define edge-color
              (if (Remote-reachable? rem)
                  "lightblue"
                  "tomato"))
            (printf
              "~a -> ~a [label=~a, fontname=monospace, fontsize=8, color=~a fontcolor=lightblue3, dir=both, arrowtail=dot];~n"
              (node-id-local loc)
              (node-id-remote rem)
              (edge-label-local2remote loc rem)
              edge-color))
         (Local-remotes loc)))
    locals)

  (invariant-assertion (set/c string? #:kind 'mutable) all-roots)
  (set-for-each
    all-roots
    (λ (r)
       (printf
         "~a [label=~a, shape=rectangle, style=filled, fillcolor=yellowgreen, fontname=monospace, fontsize=8];~n"
         (node-id-root r)
         (node-label-root r))))

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

(define sha256
  (compose bytes->hex-string sha256-bytes string->bytes/utf-8))

(define (roots-hash roots)
  (sha256 (string-join (sort (set->list roots) string<?) ",")))

(define/contract (link target alias)
  (-> path? path? void?)
  (unless (link-exists? alias)
    (make-parent-directory* alias)
    (make-file-or-directory-link
      (apply build-path (append (map (λ (_) 'up) (cdr (explode-path alias))) `(,target)))
      alias)))

(define/contract (lines-to-file path lines)
  (-> path? (listof string?) void?)
  (make-parent-directory* path)
  (display-lines-to-file lines path #:exists 'replace))

(define/contract (reroot path root)
  (-> path? path? path?)
  (cond
    [(relative-path? path) (build-path root path)]
    [(absolute-path? path) (apply build-path (cons root (cdr (explode-path path))))]
    [else (assert-unreachable)]))

(define/contract (output-dir-tree locals)
  (-> (listof Local?) void?)
  ; TODO Refactor this big mess of a procedure
  ; TODO data/$host.rktd
  ; TODO reports/...
  (define (local->work-tree-path loc)
    ; XXX Use work tree for non-bares. TODO Consider optionalizing.
    (if (Local-bare? loc)
        (Local-path loc)
        (match-let-values
          ([(work-tree .git trailing-slash?) (split-path (Local-path loc))])
          (invariant-assertion absolute-path? work-tree)
          (invariant-assertion ".git" (path->string .git))
          (invariant-assertion #f trailing-slash?)
          work-tree)))

  (define (local->by-host-dir-path loc)
    (reroot
      (local->work-tree-path loc)
      (build-path "by-host" (Local-hostname loc))))

  (define/contract (remote->by-host-dir-path rem)
    (-> Remote? (or/c #f (cons/c string? path?)))
    (with-handlers
      ([exn? (λ (_)
                ; TODO Parse git's alternative scp-like SSH URI syntax
                ;      https://git-scm.com/docs/git-clone#_git_urls
                #f)])
      (define u (string->url (Remote-addr rem)))
      (define path (reroot
                     (apply build-path (map path/param-path (url-path u)))
                     (build-path "by-host" (url-host u))))
      (cons (Remote-name rem) path)))

  (define/contract (write-by-host loc)
    (-> Local? void?)
    (define dir-index-by-host (local->by-host-dir-path loc))

    (when (Local-description loc)
      (lines-to-file
        (build-path dir-index-by-host "description")
        `(,(Local-description loc))))

    (define remotes (Local-remotes loc))
    (define remotes-parsed (filter-map remote->by-host-dir-path remotes))
    (for-each
      (match-lambda
        [(cons name path)
         ; XXX A dummy file until we implement spidering out to remotes:
         (lines-to-file
           (build-path path "placeholder-until-querying-remotes-is-implemented")
           '())
         (link path
               (build-path dir-index-by-host "remotes" name))])
      remotes-parsed)

    (lines-to-file
      (build-path dir-index-by-host "remotes.txt")
      (map (λ (rem) (format "~a ~a" (Remote-name rem) (Remote-addr rem)))
           (sort remotes
                 (λ (r1 r2) (string<? (Remote-name r1)
                                      (Remote-name r2))))))

    (let ([dir (build-path dir-index-by-host "roots")])
      (set-for-each
        (Local-roots loc)
        (λ (root)
           (link
             (build-path "by-root" root)
             (build-path dir root))))))

  (define/contract (write-by-roots loc)
    (-> Local? void?)
    (define dir-index-by-roots
      ; Concatenated-roots string is sometimes too-long for a valid filename.
      (build-path "by-roots" (roots-hash (Local-roots loc))))
    (define loc-path (local->work-tree-path loc))
    (invariant-assertion absolute-path? loc-path)
    (define link-name
      (string-join (cons (Local-hostname loc)
                         (map path->string (cdr (explode-path loc-path))))
                   "---"))
    (link (local->by-host-dir-path loc)
          (build-path dir-index-by-roots link-name)))

  (define/contract (write-by-root loc)
    (-> Local? void?)
    (set-for-each
      (Local-roots loc)
      (λ (root)
         (define dir-index-by-root (build-path "by-root" root))
         (define loc-path (local->work-tree-path loc))
         (invariant-assertion absolute-path? loc-path)
         (define link-name
           (string-join (cons (Local-hostname loc)
                              (map path->string (cdr (explode-path loc-path))))
                        "---"))
         (link (local->by-host-dir-path loc)
               (build-path dir-index-by-root link-name)))))

  (define/contract (local->name loc)
    (-> Local? string?)
    (path->string (last (explode-path (local->work-tree-path loc)))))

  (define/contract (write-by-name loc)
    (-> Local? void?)
    (define link-name
      (let ([loc-path (local->work-tree-path loc)])
        (invariant-assertion absolute-path? loc-path)
        (string-join (cons (Local-hostname loc)
                           (map path->string (cdr (explode-path loc-path))))
                     "---")))
    (link (local->by-host-dir-path loc)
          (build-path "by-name" (local->name loc) link-name)))

  (for-each
    (λ (loc)
       (write-by-host loc)
       (write-by-name loc)
       (write-by-root loc)
       (write-by-roots loc))
    locals))

(define/contract (input data-source input-paths)
  (-> data-source? (listof path?) (listof Local?))
  (match data-source
    [(Search ignore)
     (find-git-repos (gethostname) input-paths ignore)]
    ['read
     (if (empty? input-paths)
         (deserialize (read))
         (append* (map (λ (p)
                          (with-input-from-file p (λ () (deserialize (read)))))
                       input-paths)))]))

(define/contract (output out-format locals)
  (-> out-format? (listof Local?) void?)
  (match out-format
    [(DirTree rooted-in)
     (parameterize ([current-directory rooted-in])
       (output-dir-tree locals))]
    ['serial (write (serialize locals))]
    ['report-graph (output-graph locals)]))

(define (main data-source input-paths out-filters out-format out-dst)
  (define t0 (current-inexact-milliseconds))
  ; TODO Time filters separately from reading input.
  (define locals
    (let ([locals (input data-source input-paths)])
      (match (set->list out-filters)
        ['() locals]
        [(and (list* _ _) filters)
         (filter (λ (r) (andmap (λ (f) (f r)) filters)) locals)])))
  (define t1 (current-inexact-milliseconds))
  (let ([output (λ () (output out-format locals))])
    (match out-dst
      ['stdout (output)]
      [(cons 'file file-path)
       (with-output-to-file file-path (λ () (output)) #:exists 'replace)]))
  (eprintf "~a ~a local repos, ~a roots and ~a remotes in ~a seconds.~n"
           (match data-source
             [(Search _) "Found"]
             [(or 'read) "Read"])
           (length locals)
           (length (append* (map (compose set->list Local-roots) locals)))
           (length (uniq (append* (map Local-remotes locals))))
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
      ; TODO Re-implement multi-homed?.
      ;      Will now need (listof Local?) as input, rather than just Local?
      ; [("--multi-homed")
      ; "Output filter: only repos with multiple local directories (local forks)."
      ; (set-add! out-filters multi-homed?)]

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
      ; TODO Re-implement HTML report?
      ;[("--report-html")
      ; "Output a report in HTML."
      ; (set! out-format 'report-html)]

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
      (invariant-assertion (set/c out-filter? #:kind 'mutable) out-filters)
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
