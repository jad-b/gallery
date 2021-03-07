module Sorting
where

-- Quicksort in Erlang:
--
-- qsort([]) -> [];
-- qsort([Pivot|T]) ->
--     qsort([X||X<-T,X =< Pivot]) ++
--     [Pivot] ++
--     qsort([X||X<-T,X > Pivot])
