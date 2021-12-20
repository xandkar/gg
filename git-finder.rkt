#lang racket

(require racket/os)

(define local? path-string?)
(define locals? (listof local?))
(define remote? string?)
(define remotes? (listof remote?))

(define-struct/contract Repo
  ([hostname string?]
   [root string?]
   ;[description (or/c #f string?)] ; TODO Grab from .git/description
   ; TODO Each local can have diff description. Combine them?
   [locals locals?]
   [remotes remotes?])
  #:transparent)

(define/contract (exe cmd)
  (-> string? (listof string?))
  (match-define
    (list stdout stdin _pid stderr ctrl)
    (process cmd))
  (define lines (port->lines stdout))
  (ctrl 'wait)
  ; TODO Print stderr if not 'done-ok
  (invariant-assertion 'done-ok (ctrl 'status))
  (close-output-port stdin)
  (close-input-port stdout)
  (close-input-port stderr)
  lines)

(define/contract (git-dir->remotes git-dir-path)
  (-> path-string? locals?)
  (exe
    (string-append
      "git --git-dir=" git-dir-path " remote -v | awk '{print $2}' | sort -u")))

(define/contract (git-dir->root-digest git-dir-path)
  (-> path-string? (or/c #f string?))
  (define cmd
    (string-append
      "git --git-dir=" git-dir-path " log --pretty=oneline --reverse | head -1 | awk '{print $1}'"))
  (match (exe cmd)
    ['() #f]
    [(list digest) digest]
    [_ (assert-unreachable)]))

(define/contract (find-git-dirs search-paths)
  (-> (listof path-string?) (listof local?))
  (define (find search-path)
    (exe (string-append "find " search-path " -type d -name .git")))
  (append* (map find search-paths)))

(define uniq
  (compose set->list list->set))

(define/contract (find-git-repos hostname search-paths exclude-prefix exclude-regexp)
  (-> string? (listof path-string?) (listof path-string?) (listof pregexp?) (listof Repo?))
  (define (root repos) (first (first repos))) ; All roots are the same in a group
  (define (locals repos) (map second repos))
  (define (remotes repos) (uniq (append* (map third repos))))
  (map (λ (repos-with-shared-root-commit)
          (Repo hostname
                (root repos-with-shared-root-commit)
                (locals repos-with-shared-root-commit)
                (remotes repos-with-shared-root-commit)))
       (group-by first (foldl (λ (dir repos)
                                 ; TODO git lookups can be done concurrently
                                 (define root (git-dir->root-digest dir))
                                 (define remotes (git-dir->remotes dir))
                                 (define repo (list root dir remotes))
                                 (if root
                                     (cons repo repos)
                                     repos))
                              '()
                              (filter
                                (λ (path)
                                   (and (not (ormap (curry string-prefix? path)
                                                    exclude-prefix))
                                        (not (ormap (λ (px) (regexp-match? px path))
                                                    exclude-regexp))))
                                (find-git-dirs search-paths))))))

(define/contract (print-table repos)
  (-> (listof Repo?) void?)
  (define (output root hostname tag locations)
    (for-each
      (λ (location)
         (displayln (string-join (list root hostname tag location) " ")))
      locations))
  (for-each
    (λ (repo)
       (match-define (Repo hostname root locals remotes) repo)
       (output root hostname "local" locals)
       (output root hostname "remote" remotes)
       (newline) ; So that same-root locations are visually grouped.
       )
    repos))

(define/contract (print-graph repos)
  (-> (listof Repo?) void?)
  (define all-roots (mutable-set))
  (define all-locals (mutable-set))
  (define all-remotes (mutable-set))
  (displayln "digraph {")
  (for-each
    (λ (r)
       ; TODO Color and shape codes for: root, local and remote.
       (match r
         [(Repo hostname root (and locals (list* _ _ _)) remotes)
          (for-each
            (λ (l)
               (set-add! all-roots root)
               (set-add! all-locals l)
               (printf
                 "~v -> ~v [label=~v, fontname=monospace, fontsize=8];~n"
                 root
                 l
                 hostname))
            locals)
          (for-each
            (λ (r)
               (set-add! all-roots root)
               (set-add! all-remotes r)
               (printf
                 "~v -> ~v [label=~v, fontname=monospace, fontsize=8];~n"
                 root
                 r
                 hostname))
            remotes)]
         [_ (void)]))
    repos)
  (set-for-each
    all-roots
    (λ (r)
       (printf
         "~v [shape=point style=filled, fillcolor=yellowgreen fontcolor=white, fontname=monospace, fontsize=8];~n"
         r)))
  (set-for-each
    all-locals
    (λ (l)
       (printf
         "~v [shape=folder, style=filled, fillcolor=wheat, fontname=monospace, fontsize=8];~n"
         l)))
  (set-for-each
    all-remotes
    (λ (r)
       (printf
         "~v [shape=oval, style=filled, fillcolor=lightblue, fontname=monospace, fontsize=8];~n"
         r)))
  (displayln "}"))

(module+ main
  ; TODO handle sub commands:
  ; - TODO "collect" data for current host
  ; - TODO "integrate" data from per-host data files into a graphviz file

  (let ([out-format 'table]
        [exclude-prefix (mutable-set)]
        [exclude-regexp (mutable-set)])
    (command-line
      #:program "git-finder"

      #:once-any
      [("-t" "--table")
       "All found repos in a tabular text format."
       (set! out-format 'table)]
      [("-g" "--graph-dupes")
       "Multi-homed repos in DOT language for Graphviz."
       (set! out-format 'graph)]

      #:multi
      [("-e" "--exclude-prefix")
       directory "Directory subtree prefix to exclude the found candidate paths."
       (invariant-assertion path-string? directory)
       (set-add! exclude-prefix directory)]
      [("-x" "--exclude-regexp")
       perl-like-regexp "Pattern to exclude from the found candidate paths."
       (let ([px (pregexp perl-like-regexp (λ (err-msg) err-msg))])
         (invariant-assertion pregexp? px)
         (set-add! exclude-regexp px))]

      #:args search-paths
      (invariant-assertion (listof path-string?) search-paths)

      (define output
        (case out-format
          [(table) print-table]
          [(graph) print-graph]))
      (define t0 (current-inexact-milliseconds))
      (define repos
        (find-git-repos (gethostname)
                        search-paths
                        (set->list exclude-prefix)
                        (set->list exclude-regexp)))
      (output repos)
      (define t1 (current-inexact-milliseconds))
      (eprintf "Found ~a roots, ~a locals and ~a remotes in ~a seconds.~n"
               (length repos)
               (length (uniq (append* (map Repo-locals repos))))
               (length (uniq (append* (map Repo-remotes repos))))
               (real->decimal-string (/ (- t1 t0) 1000) 3)))))
