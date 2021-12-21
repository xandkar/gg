#! /usr/bin/env racket

#lang racket

(require racket/os
         xml)

(define-struct/contract Remote
  ([name string?]
   [addr string?])
  #:transparent)

(define-struct/contract Local
  ([hostname string?]
   [path path-string?]
   [description (or/c #f string?)]
   [remotes (listof Remote?)])
  #:transparent)

;; TODO locals should be a custom set keyed on hostname+path
(define locals? (listof Local?))

(define-struct/contract Repo
  ([root string?]
   [locals locals?])
  #:transparent)

(define/contract (exe cmd)
  (-> string? (listof string?))
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
  (define cmd
    (string-append
      "git --git-dir=" git-dir-path " remote -v | awk '{print $1, $2}' | sort -u"))
  (map (λ (line)
          (match-define (list name addr) (string-split line))
          (Remote name addr))
       (exe cmd)))

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
  (-> (listof path-string?) (listof path-string?))
  (define (find search-path)
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
                           (match (git-dir->root-digest dir)
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

(define/contract (print-table repos)
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

(define/contract (print-graph repos)
  (-> (listof Repo?) void?)
  (define all-roots (mutable-set))
  (define all-locals (mutable-set))
  (define all-remotes (mutable-set))
  (define (local-id l) (format "~a:~a" (Local-hostname l) (Local-path l)))
  (define (local-label l)
    (define hostname (Local-hostname l))
    (define description (let ([d (Local-description l)]) (if d d "")))
    (define path (Local-path l))
    (xexpr->string
      `(table
        ([border      "0"]
         [cellborder  "0"]
         [cellpadding "1"]
         [cellspacing "0"])
        (tr (td (b "host ")) (td ([align "left"]) ,hostname))
        (tr (td (b "path ")) (td ([align "left"]) ,path))
        (tr (td (b "desc ")) (td ([align "left"]) ,description)))))
  (displayln "digraph {")
  (for-each
    (λ (r)
       ; TODO Color and shape codes for: root, local and remote.
       (match r
         [(Repo root (and locals (list* _ _ _)))
          (for-each
            (λ (l)
               (set-add! all-roots root)
               (set-add! all-locals l)
               (printf
                 "~v -> ~v [fontname=monospace, fontsize=8, color=yellowgreen];~n"
                 root
                 (local-id l))
               (for-each
                 (λ (r)
                    (set-add! all-remotes (Remote-addr r))
                    (printf
                      "~v -> ~v [label=~v, fontname=monospace, fontsize=8, color=lightblue fontcolor=lightblue3, dir=both, arrowtail=dot];~n"
                      (local-id l)
                      (Remote-addr r)
                      (Remote-name r)))
                 (Local-remotes l)))
            locals)]
         [_ (void)]))
    repos)
  (set-for-each
    all-roots
    (λ (r)
       (printf
         "~v [shape=rectangle, style=filled, fillcolor=yellowgreen, fontname=monospace, fontsize=8];~n"
         r)))
  (set-for-each
    all-locals
    (λ (l)
       (printf
         "~v [label=<~a> shape=folder, style=filled, fillcolor=wheat, fontname=monospace, fontsize=8, fontcolor=black];~n"
         (local-id l)
         (local-label l))))
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
               (length (uniq (flatten (map (λ (locals) (map Local-remotes locals)) (map Repo-locals repos)))))
               (real->decimal-string (/ (- t1 t0) 1000) 3)))))
