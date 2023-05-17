#!/bin/bash
erlc -o  ambient.erl
erlc -o  render.erl
erlc -o  car.erl

erl  -eval "ambient:main(3,3)" 
