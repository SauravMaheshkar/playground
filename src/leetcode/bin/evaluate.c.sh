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

for file in "$data_path"/*.json; do
	# Iterate over all datasets
	if [ -f "$file" ]; then

		# compile source code
		problem=$(basename -- "$file")
		problem="${problem%.*}" # Remove extension to get the base filename
		source_code="$dir_path/$problem.c"

		# skip if implementation doesn't exist
		if [ ! -f "$source_code" ]; then
			echo "Corresponding C implementation not found for: $problem"
			continue
		fi

		# Compile source code to a executable
		gcc -o "$dir_path/$problem" "$source_code"

		if [ $? -eq 0 ]; then
			# Process input, output pairs
			jq -c '.[]' "$file" | while IFS= read -r pair; do

				# Everything but last element is the input
				input=$(echo "$pair" | jq -r 'del(.[-1]) | @json')
				output=$(echo "$pair" | jq -r '.[-1] | @json')
				if [[ "$input" =~ ^\[(.*)\]$ ]]; then # Check if the input is an array
					filtered_input="${BASH_REMATCH[1]}"
				fi

				if [[ "$filtered_input" == *"],["* ]]; then
					split_var=$(echo "$filtered_input" | sed 's/],\[/\n/g')

					# Assign values to first_part and second_part
					first_part=$(echo "$split_var" | head -n 1)
					second_part=$(echo "$split_var" | tail -n 1)
					# Add missing "]" and "["
					first_part=$(printf "%s]" "$first_part")
					second_part=$(printf "[%s" "$second_part")

					"./$dir_path/$problem" "$first_part" "$second_part" "$output"
				else
					"./$dir_path/$problem" "$filtered_input" "$output"
				fi
			done

			# Delete generated binary
			rm "./$dir_path/$problem"
		else
			echo "Compilation failed for $source_code"
		fi
	fi
done
