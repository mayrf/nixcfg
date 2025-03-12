#!/usr/bin/env bash
if [[ -z "$SOPS_AGE_KEY" ]]; then
  echo "Error: SOPS_AGE_KEY environment variable is not set."
  exit 1
fi

# Create a temporary file for the Age key
age_key_file=$(mktemp)

# Ensure the temporary file is deleted when the script exits, even on errors or interruptions
trap 'rm -f "$age_key_file"' EXIT

# Write the key to the temporary file
echo "$SOPS_AGE_KEY" > "$age_key_file"

# Set SOPS_AGE_KEY_FILE and run the sops command
export SOPS_AGE_KEY_FILE=$age_key_file 
sops edit "$@"
