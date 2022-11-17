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
