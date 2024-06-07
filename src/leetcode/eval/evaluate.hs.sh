#!/bin/bash

# Check if the data_path and dir_path argument is provided
if [ $# -ne 2 ]; then
	echo "Usage: $0 <data_path> <dir_path>"
	exit 1
fi

data_path="$1"
dir_path="$2"

# Check if the datasets exist
if [ ! -d "$data_path" ]; then
	echo "data path not found: $data_path"
	exit 1
fi

# Check if the implementations exist
if [ ! -d "$dir_path" ]; then
	echo "dir path not found: $dir_path"
	exit 1
fi

data_path=$(realpath "$data_path")
dir_path=$(realpath "$dir_path")

relative_data_path=$(realpath --relative-to="$dir_path" "$data_path")

(
	cd "$dir_path" || exit 1
	cabal build
	cabal run trees-exe -- "$data_path"
)
