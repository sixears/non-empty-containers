1.4.3.31 2022-11-19
===================
- base0t->0.0.1.13

1.4.3.30 2022-11-19
===================
- natural->0.0.1.13

1.4.3.29 2022-11-19
===================
- has-callstack->1.0.1.16

1.4.3.28 2022-11-19
===================
- tasty-plus->1.5.2.19;exited->1.0.4.19;base0t->0.0.1.12;monaderror-io->1.2.5.16;tfmt->0.2.7.20;number->1.1.2.13;has-callstack->1.0.1.15;base0->0.0.4.10;base1->0.0.9.27;natural->0.0.1.12;more-unicode->0.0.17.11;fpath->1.3.2.28

1.4.3.27 2022-11-19
===================
- flake-build-utils->1.0.0.13

1.4.3.26 2022-11-18
===================
- index->1.0.1.18

1.4.3.25 2022-11-18
===================
- index->1.0.1.17;base0t->0.0.1.10;number->1.1.2.11;base0->0.0.4.8;more-unicode->0.0.17.9;flake-build-utils->1.0.0.12

1.4.3.24 2022-11-18
===================
- tfmt->0.2.7.17

1.4.3.23 2022-11-18
===================
- has-callstack->1.0.1.13

1.4.3.22 2022-11-18
===================
- flake-build-utils->1.0.0.12

1.4.3.21 2022-11-18
===================
- tasty-plus->1.5.2.16

1.4.3.20 2022-11-18
===================
- exited->1.0.4.16

1.4.3.19 2022-11-18
===================
- monaderror-io->1.2.5.13

1.4.3.18 2022-11-17
===================
- tfmt->0.2.7.15

1.4.3.17 2022-11-17
===================
- has-callstack->1.0.1.12

1.4.3.16 2022-11-17
===================
- number->r1.1.2.10

1.4.3.15 2022-11-17
===================
- base0t->r0.0.1.9

1.4.3.14 2022-11-17
===================
- base0->r0.0.4.7

1.4.3.13 2022-11-17
===================
- upgrade to callPackage-based versions

1.4.3.12 2022-11-13
===================
- fix fixed-package-name typo in flake-build-utils

1.4.3.11 2022-11-04
===================
- fix package names

1.4.3.10 2022-11-03
===================
- remove redundant "output" flake-utils

1.4.3.9 2022-11-03
==================
- flake-build-utils->1.0.0.6

1.4.3.8 2022-11-03
==================
- base0t->0.0.1.3

1.4.3.7 2022-11-03
==================
- base0->0.0.4.2; base0t->0.0.1.2

1.4.3.6 2022-11-03
==================
- base0->0.0.4.1; base0t->0.0.1.1

1.4.3.5 2022-11-02
==================
- natural->0.0.1.2

1.4.3.4 2022-11-02
==================
- more-unicode -> 0.0.17.2

1.4.3.3 2022-11-02
==================
- upgrade flake-build-utils to 1.0.0.3

1.4.3.2 2022-10-27
==================
- add flake
- use ghc-8.10.7 for tfmt

1.4.3.1 2022-04-06
==================
- upgrade base1 to 0.0.8.0

1.4.3.0 2022-01-05
==================
- export toList from NonEmptyHashSet

1.4.2.0 2021-07-12
==================
- add NFData instance of SeqNE

1.4.1.0 2021-06-11
==================
- add in NonEmptyHashSet (from Fluffy)

1.4.0.0 2020-02-05
==================
- Add Lift instance to SeqNE

1.2.0.0 2019-12-14
==================
- Revert 1.1.0.0, indeed tidy up name usage to make use of 'Mono' in the class
  name standard wherever 'Element α' is used.

1.1.0.0 2019-12-14
==================
- Rename Mono classes to remove the Mono.  ABORTED; classes that require Element
  from Data.MonoTraversable should be marked as Mono, because they are usable
  with instances that have a single Element type (e.g., Text, Bytestring, FPath).

1.0.1.0 2019-12-13
==================
- add instances for:
    - FromMonoSeq [α]
    - ToMonoSeq   (Seq α)
    - ToMonoSeq   [α]
    - ToMonoSeq   (NonNull [α])
    - ToMonoSeq   (NonEmpty α)
    - IsMonoSeq   (Seq α)
    - IsMonoSeq   [α]
    - AsMonoSeq   [α]
    - FromMonoSeqNonEmpty (NonEmpty α)
    - FromMonoSeqNonEmpty (NonNull [α])
    - FromMonoSeqNonEmpty [α]
    - ToMonoSeqNonEmpty   (NonEmpty α)
    - ToMonoSeqNonEmpty   (NonNull [α])
    - IsMonoSeqNonEmpty   (SeqNE α)
    - IsMonoSeqNonEmpty   (NonEmpty α)
    - IsMonoSeqNonEmpty   (NonNull [α])
    - AsMonoSeqNonEmpty   (Seq α)
    - AsMonoSeqNonEmpty   (NonEmpty α)
    - AsMonoSeqNonEmpty   (NonNull [α])
    - AsMonoSeqNonEmpty   [α]

1.0.0.0 2019-09-21
==================
- factored out from fpath
