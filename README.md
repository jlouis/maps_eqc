# Erlang QuickCheck tests for Maps

We are at a point where maps are becoming more and more ingrained in our code
bases. This repository contains a complete suite for running QuickCheck against
the maps operations in the hope this can uncover or provide a stable point of
interest.

## Requirements

Quviq Erlang QuickCheck 1.33.3 or higher I think. May work on earlier versions of QuickCheck. The stateful nature and grouped commands are not supported well in either Triq or Proper.

Needs rebar3 for the full compile.

## Running the test

The test case can be run in one of two modes: local and distributed.

#### Running Locally:

This is the simplest form. Compile the tests:

	$ rebar3 compile
	
Then run an Erlang, for instance:

	$ erl -name eqc@127.0.0.1 -pa _build/default/lib/*/ebin
	## Replace with ${ERL_TOP}/bin/cerl -debug and so on

Then, in the shell, run:

	1> eqc:module([{testing_budget, 15}, {testing_profile, local}], maps_eqc).

which will run tests for 15 seconds.

#### Running distributed:

To make this test work, you need two nodes with the same *cookie*. The 'eqc' node will run the model, and the 'runner' node will run the maps. Hence, if a map test segfaults, the 'eqc' node knows this and can keep the model and output the commands from the failure.

Set up the runner node in a heartbeat:

	$ export HEART_COMMAND='erl -name runner@127.0.0.1 -pa _build/default/lib/*/ebin -heart -env HEART_BOOT_DELAY 0'
	
Then run it in the `maps_eqc` directory:

	$ ${HEART_COMMAND}
	
Now, start the 'eqc' node:

	$ erl -name eqc@127.0.0.1 -pa _build/default/lib/*/ebin
	## Replace with ${ERL_TOP}/bin/cerl -debug and so on
	
And run the tests with distribution enabled:

	1> eqc:module([{testing_budget, 15}, {testing_profile, distributed}], maps_eqc).
	
Failures can segfault the 'runner' node but it will be heart–restarted and then shrinking will continue.

## Status

* 17.4.1: Found problems with comparison of exact terms. Fixed in 17.5 with OTP-12623
* 18.0-rc1: Numerous bugs are detected by this suite. Some causes the system to segfault.
* 18.0-rc1+patches: Maps are stable, except for maps where many keys collide. For those, we have found errors in maps:merge/2 which causes the system to excessively allocate memory until it runs out and crashes.
* 18.0-rc1+patches: Collision errors found in maps:from_list/1 with numerous colliding keys. Also found errors in `binary_to_term/1` and `term_to_binary/1`.

## Features

We test a lot of cases which are highly unlikely to occur in the real world:

* We store really odd map terms as keys
* We run odd command orders on maps
* We have maps in which almost every key causes a hash collision in the HAMT hash

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
	R017: populating an empty map with from_list/1
	R018: populating an empty map with put/2
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
	R040: Converted map there and back again
	R041: badarg check on maps:size/1
	R042: badarg check on maps:put/3
	R043: badarg check on maps:is_key/2
	R044: badarg check on maps:keys/1
	R045: badarg check on maps:remove/2
	R046: badarg check on maps:to_list/1
	R047: badarg check on maps:update/3
	R048: badarg check on maps:values/1
	R049: badarg check on maps:from_list/1
	R050: badarg check on maps:get/2
	R051: badarg check on maps:find/2
	R052: badarg check on maps:merge/2
	R053: badarg check on maps:fold/3
	R054: badarg check on maps:with/2
	R055: badarg check on maps:without/2
	R056: badkey on a maps:take/2
	R057: succesful maps:take/2

