{-# LANGUAGE QuasiQuotes #-}
module SashimiCore (coreText) where

import Text.RawString.QQ

coreText = [r|

module "Sashimi.Core";

export map = fn l, f:
	reduceRight(l, [], fn acc, el:
        cons(f(el), acc));

export filter = fn l, f:
    reduceRight(l, [], fn acc, el:
        if (f(el)):
            cons(el, acc),
            acc);

export reverse = fn l:
    reduce(l, [], cons);

export take = fn l, n:
    if (n == 0):
        [],
        cons(first(l), take(rest(l), n-1));

|]
