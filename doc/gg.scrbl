#lang scribble/manual
@; vim: filetype=scribble

@require[@for-label[gg
                    racket/file
                    racket/base]]

@title{gg: git git}
@author[(author+email "Siraaj Khandkar" "siraaj@khandkar.net")]
@defmodule[gg]

A tool to git all your gits and answer the age-old question: "Where my gits at?"

@section{What?}

@code{gg} searches your hard drive for @code{git} repositories and outputs some
data about their locations and state (roots, heads, remotes, etc.).

Can output data in the following formats:
@itemlist[
    @item{directory tree index}
    @item{DOT (for Graphviz)}
    @item{serialized (for later re-consumption)}
]

Putting that data into a git repository itself and then repeating the same
process on all your machines, will give you a catalogue of all your repos and
show their relationships.

@section{Why?}

@subsection{Problem!}

@itemlist[
  @item{
    You may have several machines in use at any given time,
    with subsets of all repos sparsely scattered across.
  }
  @item{Same repo's replica directories may end-up named differently.}
  @item{You may end-up missing a local copy of an Internet-hosted replica.}
  @item{You may have orphaned local repos (with no remotes)}
  @item{You can end-up with any combination of the above scenarios!}
]

@subsection{Solution?}
So, how to be sure everything is in sync?

@itemlist[#:style 'ordered
  @item{Find all the repos and confirm what is a fork of what (sharing an ancestor);}
  @item{Compare their status to understand what is out of date;}
  @item{Decide what should be pruned, moved, updated or whatever;}
  @item{GOTO 1}
]

@code{gg} is mainly motivated by 1, but also aims to be useful for 2.

See the @code{example} meta-repo in @code{gg}'s source repo.

@section{Compatibility}

Due to a dependency on the Unix @code{find} utility, only Unix-like operating
systems are currently supported and the only ones I've tested @code{gg} on
are:

@itemlist[
    @item{Void Linux}
    @item{Debian 10}
    @item{Ubuntu 18.04}
]

This dependency isn't strictly necessary, it's just much faster than Racket's
@racket[find-files]. I'll eventually get around to adding a fallback on
@racket[find-files] when system @code{find} isn't available.
