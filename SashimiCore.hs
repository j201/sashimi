{-# LANGUAGE QuasiQuotes #-}
module SashimiCore (coreText) where

import Text.RawString.QQ

coreText = [r|

module "Sashimi.Core";

export map = fn l, f:
    lazyList(fn:
        if l == []:
            [],
            lazyCons(f(first(l)), fn: map(rest(l), f)));

export dropUntil = fn l, f:
    if l == []: [],
    if f(first(l)): l,
        dropUntil(rest(l), f);

export filter = fn l, f:
    lazyList(fn:
        let dropped = dropUntil(l, f):
        if dropped == []:
            [],
            lazyCons(first(dropped), fn: filter(rest(dropped), f)));

// TODO: TCO
export reduce = fn [
    l, i, f:
        if l == []:
           i,
           reduce(rest(l), f(i, first(l)), i),
    l, f: reduce(rest(l), first(l), f)
];

export reverse = fn l:
    reduce(l, [], cons);

export take = fn l, n:
    lazyList(fn:
        if n == 0:
            [],
            lazyCons(first(l), fn: take(rest(l), n-1)));

export range = fn [
    start, end, step = 1: 
        if start >= end:
            [],
            lazyList(fn: lazyCons(start, fn: range(start + step, end, step))),
    end:
        range(0, end),
    :
        let rangeFrom = fn n:
            lazyList(fn: lazyCons(n, fn: rangeFrom(n+1))):
        rangeFrom(0)
];

|]
