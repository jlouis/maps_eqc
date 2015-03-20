# Erlang QuickCheck tests for Maps

We are at a point where maps are becoming more and more ingrained in our code
bases. This repository contains a complete suite for running QuickCheck against
the maps operations in the hope this can uncover or provide a stable point of
interest.

## Requirements

Quviq Erlang QuickCheck 1.33.3 or higher I think. May work on earlier versions of QuickCheck. The stateful nature and grouped commands are not supported well in either Triq or Proper. Furthermore, it would be neat to extend the model with feature–support in the longer run, and this is not supported either.

## Status

The current model crashes `OTP-17.4.1-1428-g7409949` with a segmentation fault. I have yet to figure out what is wrong, but something is wrong.