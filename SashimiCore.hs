{-# LANGUAGE QuasiQuotes #-}
module SashimiCore (coreText) where

import Text.RawString.QQ

coreText = [r|
module "Sashimi.Core";

N = import "Sashimi.Native";

export first = N.first;
export rest = N.rest;
export empty = N.empty;
export cons = N.cons;

export sum = fn l:
	if empty(l): 0, first(l) + sum(rest(l));

export reduce = fn [
	l, i, f:
		if empty(l):
			i,
			reduce(rest(l), f(i, first(l)), f),
	l, f:
		reduce(rest(l), first(l), f)
];

export map = fn l, f:
	if empty(l):
		[],
		cons(f(first(l)), map(rest(l), f));

|]
