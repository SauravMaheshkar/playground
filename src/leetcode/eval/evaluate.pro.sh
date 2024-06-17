#!/bin/bash

# Check if the data_path and dir_path argument is provided
if [ $# -ne 1 ]; then
	echo "Usage: $0 <dir_path>"
	exit 1
fi

dir_path="$1"

# Check if the implementations exist
if [ ! -d "$dir_path" ]; then
	echo "dir path not found: $dir_path"
	exit 1
fi

for file in "$dir_path"/*.pro; do
	if [ -f "$file" ]; then
		result=$(swipl -g "run_tests." -t halt $file)

		if [ -n "$result" ]; then
			echo "Failing for: $file"
		fi
	fi
done
