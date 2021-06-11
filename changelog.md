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
