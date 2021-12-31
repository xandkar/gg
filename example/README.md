Example meta repo
=================
Here's what an example repo to track all your repos might look like. Take a
look at the `Makefile`.

TL;DR
-----
Collect search results from all machines and integrate them into reports and
indices.

NTL;R
-----
We search for `git` repositories in the target directories (specified in the
targets file) of the current machine, ignoring paths matching those in the
ignore file, and save the results as `data/$HOST.rktd`.

Then we read everything in `data/*.rktd` (search results from all your
machines) and create `reports` and `indices` which help you see which
repositories exist on which machines and what they point to as their remotes
(if any!).

Before creating the indices though, we delete the previous ones, so that we can
piggyback on git to track for us when the file got moved or removed on one of
the machines.

Each, the targets file and the ignore file are differentiated by host, to
accommodate potentially-differing filesystem organization schemes on different
machines.
