# Erlang QuickCheck tests for Maps

We are at a point where maps are becoming more and more ingrained in our code
bases. This repository contains a complete suite for running QuickCheck against
the maps operations in the hope this can uncover or provide a stable point of
interest.

## Requirements

Quviq Erlang QuickCheck 1.33.3 or higher I think. May work on earlier versions of QuickCheck. The stateful nature and grouped commands are not supported well in either Triq or Proper. Furthermore, it would be neat to extend the model with feature–support in the longer run, and this is not supported either.

## Status

The current model crashes `OTP-17.4.1-1428-g7409949` with a segmentation fault. I have yet to figure out what is wrong, but something is wrong.

## Features

We currently test against the following features:

	R001: is_key/2 on a present key
	R002: is_key/2 on a non-existing key
	R003: put/3 on existing key
	R004: put on a new key
	R005: size/1 on a 128+ map
	R006: size/1 on a 64+ map
	R007: size/1 on a 16+ map
	R008: size/1 on an empty (0) map
	R009: size/1 on a small non-empty map
	R010: Calling keys/1 on the map
	R011: remove/2 of present key
	R012: remove/2 of non-present key
	R013: to_list/1 called on map
	R014: values/1 called on map
	R015: update/3 on an existing key
	R016: update/3 on a non-existing key
	R017x: populating an empty map with from_list/1 of size: " ++ Sz
	R018x: populating an empty map with put/2 size: " ++ Sz
	R019: Merging two maps
	R020: find on a non-existing key
	R021: find on an existing key
	R022: get/2 on a non-existing key
	R023: get on a successful key
	R024: get/3 on an existing key
	R025: get/3 on a non-existing key
	R026: using the map/2 functor on the map()
	R027: traverse over the map by fold/3
	R028: with/2 query on present keys
	R029: with/2 query on non-existing keys
	R030: withtout/2 query on present keys
	R031: withtout/2 query on non-existing keys
	R032: with/2 on present keys
	R033: with/2 on non-existing keys
	R034: withtout/2 on present keys
	R035: withtout/2 on non-existing keys
	R036: Maps are consistent when sending them to another process
	R037: Maps sent roundtrip through another process are reflexive
	R038: Recalled a persisted version of the map successfully
	R039: Refocus and "become" an old version of the map