#!/bin/bash
set -e  # Exit immediately if a command fails

echo "Checking for updates from remote..."
git remote update

# Check if the branch is explicitly up-to-date
if git status -uno | grep -q "Your branch is up to date with 'origin/main'"; then
  echo "Repository is already up-to-date $(date)"
else
  echo "Repository needs updating, resetting to match origin/main..."
  # Force reset to match origin/main exactly
  git fetch origin
  git reset --hard origin/main
  echo "Updated to latest main branch $(date)"
fi
