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

(define/contract (git-dir->remotes git-dir-path)
  (-> path-string? locals?)
  (define cmd
    (string-append
      "git --git-dir="
      git-dir-path
      " remote -v | awk '{print $2}' | sort -u"))
  (match-define
    (list stdout _stdin _pid stderr ctrl)
    (process cmd))
  (ctrl 'wait)
  (invariant-assertion 'done-ok (ctrl 'status))
  (invariant-assertion '() (port->lines stderr))
  (port->lines stdout))

(define/contract (git-dir->root-digest git-dir-path)
  (-> local? (or/c #f string?))
  (define harmless-error-msg
    ; TODO Use regex to match any branch name
    "fatal: your current branch 'master' does not have any commits yet")
  (define (harmless-error? msg)
    (string=? msg harmless-error-msg))
  (define (unexpected-error? msg) (not (harmless-error? msg)))
  (define cmd
    (string-append
      "git --git-dir="
      git-dir-path
      " log --pretty=oneline --reverse | head -1 | awk '{print $1}'"))
  (match-define
    (list stdout _stdin _pid stderr ctrl)
    (process cmd))
  (ctrl 'wait)
  (invariant-assertion 'done-ok (ctrl 'status))
  (invariant-assertion '() (filter unexpected-error? (port->lines stderr)))
  (match (port->lines stdout)
    ['() #f]
    [(list digest) digest]
    [_ (assert-unreachable)]))

(define/contract (find-git-dirs search-paths)
  (-> (listof path-string?) (stream/c local?))
  ; TODO Check stderr?
  (define (find search-path)
    (match-define
      (list stdout _stdin _pid _stderr _ctrl)
      (process (string-append "find " search-path " -type d -name .git")))
    (sequence->stream (in-lines stdout)))
  (apply stream-append (map find search-paths)))

(define uniq
  (compose set->list list->set))

(define/contract (find-git-repos hostname search-paths)
  (-> string? (listof path-string?) (listof Repo?))
  (define (root repos) (first (first repos))) ; All roots are the same in a group
  (define (locals repos) (map second repos))
  (define (remotes repos) (uniq (append* (map third repos))))
  (map (λ (repos-with-shared-root-commit)
          (Repo hostname
                (root repos-with-shared-root-commit)
                (locals repos-with-shared-root-commit)
                (remotes repos-with-shared-root-commit)))
       (group-by first (stream-fold (λ (repos dir)
                                       ; TODO git lookups can be done concurrently
                                       (define root (git-dir->root-digest dir))
                                       (define remotes (git-dir->remotes dir))
                                       (define repo (list root dir remotes))
                                       (if root
                                           (cons repo repos)
                                           repos))
                                    '()
                                    (find-git-dirs search-paths)))))

(define/contract (print-table repos)
  (-> (listof Repo?) void?)
  (define (output hostname root tag locations)
    (for-each
      (λ (location)
         (displayln (string-join (list hostname root tag location) " ")))
      locations))
  (for-each
    (λ (repo)
       (match-define (Repo hostname root locals remotes) repo)
       (output hostname root "local" locals)
       (output hostname root "remote" remotes)
       (newline) ; So that same-root locations are visually grouped.
       )
    repos))

(define/contract (print-graph repos)
  (-> (listof Repo?) void?)
  (displayln "digraph {")
  (for-each
    (λ (r)
       ; TODO Color and shape codes for: root, local and remote.
       ; locals
       (match r
         [(Repo _hostname root (and locals (list* _ _ _)) _)
          (for-each
            (λ (l) (printf "~v -> ~v;~n" root l))
            locals)]
         [_ (void)])

       ; remotes
       (match r
         [(Repo _hostname root _ (and remotes (list* _ _ _)))
          (for-each
            (λ (r) (printf "~v -> ~v;~n" root r))
            remotes)]
         [_ (void)])
       )
    repos)
  (displayln "}"))

(module+ main
  ; TODO handle sub commands:
  ; - TODO "collect" data for current host
  ; - TODO "integrate" data from per-host data files into a graphviz file

  (let ([out-format 'table])
    (command-line
      #:program "git-finder"

      #:once-any
      [("-t" "--table")
       "All found repos in a tabular text format."
       (set! out-format 'table)]
      [("-g" "--graph-dupes")
       "Multi-homed repos in DOT language for Graphviz."
       (set! out-format 'graph)]

      #:args search-paths
      (invariant-assertion (listof path-string?) search-paths)

      (define output
        (case out-format
          [(table) print-table]
          [(graph) print-graph]))
      (define repos (find-git-repos (gethostname) search-paths))
      (output repos)
      (eprintf "Found ~a roots ~a locals, ~a remotes.~n"
               (length repos)
               (length (uniq (append* (map Repo-locals repos))))
               (length (uniq (append* (map Repo-remotes repos))))))))
