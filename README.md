gg
=======
git git

A tool to git all your gits and answer the age-old question: "Where my gits at?"

To discover, catalogue and compare repos across N machines.

problem!
--------
* You may have several machines in use at any given time,
  with subsets of all repos sparsely scattered across.
* Same repo's replica directories may end-up named differently.
* You may end-up missing a local copy of an Internet-hosted replica.
* You may have orphaned local repos (with no remotes)
* You can end-up with any combination of the above scenarios!

solution?
---------
So, how to be sure everything is in sync?

1. Find all the repos and confirm what is a fork of what (sharing an ancestor);
2. Compare their status to understand what is out of date;
3. Decide what should be pruned, moved, updated or whatever;
4. GOTO 1

`gg` is mainly motivated by 1, but also aims to be useful for 2.

See the [example](example) meta-repo.
