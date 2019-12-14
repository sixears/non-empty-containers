1.1.0.0 2019-12-14
==================
- change {To,From,As,Is}MonoSeq{,NonEmpty} to drop the 'Mono' (since there's 
  nothing 'Mono' about them)

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
